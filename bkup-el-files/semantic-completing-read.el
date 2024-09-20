;;; semantic-completing-read.el --- Semantic completing read
;;
;; Copyright(C) 2023 John Kitchin
;;
;; Package-Requires: (emacs-jupyter "0")
;; 
;;; Commentary:
;;
;; You must have a working emacs-jupyter installation. Additionally your Jupyter
;; environment must have pycse, faiss and sentence_transformers installed.
;;
;; The principle here is we open a buffer named `scr-buffer-name' and start a
;; Jupyter REPL there. Then we communicate with that REPL to load candidates,
;; create the embedding vector database, and search it with completion.
;;
;; The embedding process is modestly expensive for large collections, so I use a
;; cheap cache mechanism. You specify a "name" for the candidates, and this is
;; the key in a set of dictionaries for the model, index and candidates. The
;; first time you load them there is a one-time cost for embedding (at least
;; until you change the candidates), then it should be fast. A collection of ~4K
;; bibtex strings takes about 30s to embed in my experience. If you set
;; `scr-cache-location' to a directory then pycse.hashcache will be used for
;; persistent caching. This is only lightly tested, and can definitely cause
;; problems if you change models. You can delete the cache directory to recover
;; from this.
;;
;; You can customize the embedding model with `scr-model-name'. See
;; https://www.sbert.net/docs/pretrained_models.html for choices if you have
;; some good reason to do that. I don't recommend changing the model
;; dynamically; set it once and stick with it.
;;
;; Sometimes large candidate sets can cause a timeout in Jupyter-emacs.
;; `scr-timeout' can be modified if you think more time is required. To minimize
;; this, the collections are embedded in batches of `scr-batch-size' or less.
;;
;; The main user command is `scr-read'. This takes a collection of strings, and
;; a name. You can also give it any options you would normally give to
;; `ivy-read'.
;;
;; I still find it a little awkward to send Python to Jupyter this way. It works
;; ok if the logic is not to complex, and you can use string formatting to build
;; up code strings. It seems a little more integrated than running a Flask app
;; though; at least I don't have to do port management. It is a little fragile
;; with respect to errors, and difficult to debug. There is a command `scr-repl'
;; that will open the REPL. You can use this to run Python commands to see what
;; might be happening. You can also set `scr-debug' to non-nil and see the code
;; that is sent to the REPL in the `scr-buffer-name' buffer.
;;
;; The completing read is based on ivy-read with a dynamic collection. In theory
;; I should be able to make it work on `completing-read', but I have found it
;; very difficult to do.
;;
;; This is still alpha-code. It works ok for me, but probably has room for
;; improvement in performance and robustness. I would not rely on the API yet.


;;; Code:
(require 'ivy)
(require 'json)

(defcustom scr-model-name
  "all-MiniLM-L6-v2"
  "Model to use in SentenceTransformer.
See https://www.sbert.net/docs/pretrained_models.html. If you
change this after starting the kernel, you need to change the
name of your collection."
  :group 'semantic-completing-read
  :type 'string)


(defcustom scr-buffer-name
  "*semantic-completing-read*"
  "Buffer name for the Jupyter REPL."
  :group 'semantic-completing-read
  :type 'string)


(defcustom scr-timeout
  30
  "Number of seconds to allow Jupyter to run before timing out.
For large collections you may need more time."
  :group 'semantic-completing-read
  :type 'integer)


(defcustom scr-batch-size
  100
  "Batch size for loading candidates.
if nil, use one batch."
  :group 'semantic-completing-read
  :type 'integer)


(defcustom scr-cache-location
  (file-name-as-directory
   (expand-file-name "~/.cache/semantic-completing-read"))
  "Directory for joblib cache.")


(defcustom scr-debug
  nil
  "If non-nil write debug code to `scr-buffer-name'.")


;; check if libraries are installed.
(cl-loop for pylib in '("sentence_transformers" "faiss" "pycse" "joblib")
	 do
	 (unless
	     (read
	      (string-trim
	       (shell-command-to-string
		(format "python -c \"import importlib.util; found = 't' if importlib.util.find_spec('%s') else 'nil'; print(found)\"" pylib))))
	   (error "%s is not installed in Python. You should probably run `pip install %s`"
		  pylib pylib)))


(defun scr-log (str)
  "Write STR to `scr-buffer-name' if `scr-debug' is non-nil.
Returns STR either way."
  (if scr-debug
      (with-current-buffer (get-buffer-create scr-buffer-name)
	(insert str "\n")
	str)
    str))


(defun scr-startup ()
  "Run the startup python."
  (with-current-buffer (get-buffer-create scr-buffer-name)
    
    (jupyter-eval
     (scr-log
      (format
       "from operator import itemgetter
import faiss
from sentence_transformers import SentenceTransformer
from joblib import Parallel, delayed
"))))
  
  (with-current-buffer (get-buffer-create scr-buffer-name)
    (jupyter-eval
     (scr-log
      "MODELS = {}
INDEXES = {}
CANDIDATES = {}

def tolisp(strings):
    return '(' + ' '.join(strings) + ')'

# model_name is not used here, it is just for caching
def scr_get_embeddings(strings, model_name):
    embeddings = model.encode(strings)    
    return embeddings   


def scr_search(str):
    embedding = model.encode([str])
    D, I = index.search(embedding, 20)
    matches = [int(x) for x in list(I.squeeze()) if x >= 0]
    c = [f'\"{x}\"' for x in itemgetter(*matches)(candidates)]
    return tolisp(c)
")))

  (when scr-cache-location
    (scr-cache-on)))

(defun scr-cache-on ()
  "Turn caching on."
  (when scr-cache-location
    (with-current-buffer (get-buffer-create scr-buffer-name)
      (jupyter-eval
       (scr-log
	(format "from pycse import hashcache
hashcache.cache = '%s'
hashcache.verbose = False
scr_get_embeddings = hashcache()(scr_get_embeddings)
"
		scr-cache-location))))))


(defun scr-cache-off ()
  "Turn caching off."
  (when scr-cache-location
    (with-current-buffer (get-buffer-create scr-buffer-name)
      (jupyter-eval
       (scr-log
	"scr_get_embeddings = scr_get_embeddings.__wrapped__")))))


(defun scr-initialize ()
  "Initialize the Jupyter REPL.
      This is done in a buffer defined by `scr-buffer-name'.
      Also does the initial imports in Python."
  
  (when (get-buffer scr-buffer-name)
    (scr-quit))
  
  (with-current-buffer (get-buffer-create scr-buffer-name)
    (erase-buffer)
    (setq jupyter-current-client (jupyter-run-repl "python3"))
    (python-mode))

  (scr-startup))


(defun scr-set-model-index (name)
  "Set the model and index for NAME.
      NAME is a string. This allow you to use one REPL for many
      collections. The NAME is a key in some dictionaries where the
      models and indexes are stored."
  (with-current-buffer (get-buffer-create scr-buffer-name)
    (jupyter-eval
     (scr-log
      (format "if '%s' in MODELS:
    model = MODELS['%s']
    index = INDEXES['%s']
    candidates = CANDIDATES['%s']
else:
    model = SentenceTransformer('%s')
    dimension = len(model.encode('test'))
      
    MODELS['%s'] = model
    INDEXES['%s'] = faiss.IndexFlatL2(dimension)
    CANDIDATES['%s'] = None
    candidates = None
"
	      name
	      name name name
	      scr-model-name
	      name name name)))))


(defun scr-load-candidates (candidates name)
  "Load, embed and index the CANDIDATES in the repl associated with NAME.
      CANDIDATES is a list of strings.

      The loading is done in batches of up to `scr-batch-size' strings.
      This is done because it can take a while to do the embeddings,
      and sometimes jupyter time out."
  (with-current-buffer (get-buffer-create scr-buffer-name)

    ;; First make sure we have the right model and index.
    (scr-set-model-index name)

    ;; Next we have to see if we should load these.
    (when (read
	   (substring
	    (jupyter-eval
	     (scr-log
	      (format "'t' if CANDIDATES.get('%s') != %s else 'nil'"
		      name
		      (json-encode candidates))))
	    1 -1))
      
      ;; Since we are loading, we reset the index.
      (jupyter-eval
       (scr-log "index.reset()
candidates = []"))

      ;; Now load in batches
      (let* ((jupyter-default-timeout scr-timeout)
	     
	     (batches (seq-partition candidates
				     ;; we want the lesser of the batch size and
				     ;; # candidates in case the # candidates is
				     ;; less than the batch size
				     (min
				      ;; if nil, use # of candidates
				      (or scr-batch-size
					  (length candidates))
				      (length candidates))))
	     (nbatches (length batches)))

	;; now loop over each batch
	(cl-loop for i from 1 upto nbatches for batch in batches
		 do
		 (message "loading batch %s of %s" i nbatches)
		 (setq candidate-string (json-encode batch))
		 
		 (jupyter-eval
		  (scr-log
		   (format "
candidates += %s
embeddings = scr_get_embeddings(%s, '%s')
index.add(embeddings)"
			   candidate-string
			   candidate-string
			   scr-model-name)))))
      ;; And save candidates for later
      (jupyter-eval
       (format "CANDIDATES['%s'] = %s"
	       name
	       (json-encode candidates))))))


(defun scr-search-function (str)
  "Search the vector database for STR."
  (or
   ;; I like to see candidates when I start
   (when (string= "" str)
     (with-current-buffer (get-buffer-create scr-buffer-name)
       (read (substring
	      (jupyter-eval
	       (scr-log
		"
c = [f'\"{x}\"' for x in candidates]
# return a lisp list we can read in Emacs.
'(' + ' '.join(c) + ')'"))
	      1 -1))))
   ;; This is what updates it
   (with-current-buffer (get-buffer-create scr-buffer-name)
     (read (substring
	    (jupyter-eval
	     (scr-log (format "scr_search('%s')" str)))
	    1 -1)))))


(cl-defun scr-read (candidates name &rest ivy-options)
  "Semantic completing read.
CANDIDATES is a list of strings.
NAME represents the collection.
IVY-OPTIONS will be passed to `ivy-read'. It should be in plist form."

  (unless (get-buffer scr-buffer-name)
    (scr-initialize))

  ;; Make sure we have the model and index for NAME
  (scr-set-model-index name)

  ;; Load the candidates if necessary
  (scr-load-candidates candidates name)

  (apply 'ivy-read "query: "
	 #'scr-search-function
	 :dynamic-collection t
	 ivy-options))


(defun scr-repl ()
  "Open the repl for debugging."
  (interactive)
  (pop-to-buffer scr-buffer-name)
  (split-window-below)
  (jupyter-repl-pop-to-buffer))


(defun scr-quit ()
  "Kill the buffer `scr-buffer-name' and associated REPL."
  (interactive)
  (when (get-buffer scr-buffer-name)
    (with-current-buffer scr-buffer-name
      
      ;; Kill REPL buffer and process
      (kill-buffer (jupyter-with-repl-buffer jupyter-current-client
		     (current-buffer))))
    (kill-buffer scr-buffer-name)))


(provide 'semantic-completing-read)

;;; semantic-completing-read.el ends here

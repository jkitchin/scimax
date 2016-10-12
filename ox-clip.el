;;; ox-clip.el --- Cross-platform Formatted copy commands for org-mode

;; Copyright(C) 2016 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/scimax/ox-clip.el
;; Version: 0.2
;; Keywords: org-mode
;; Package-Requires: ((org "8.2") (htmlize "0"))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; This module copies selected regions in org-mode as formatted text on the
;; clipboard that can be pasted into other applications. When not in org-mode,
;; the htmlize library is used instead.

;; For Windows the html-clip-w32.py script will be installed. It works pretty
;; well, but I noticed that the hyperlinks in the TOC to headings don't work,
;; and strike-through doesn't seem to work. I have no idea how to fix either
;; issue.

;; Mac OSX needs textutils and pbcopy, which should be part of the base install.

;; Linux needs a relatively modern xclip. https://github.com/astrand/xclip

;; There is one command: `ox-clip-formatted-copy' that should work across
;; Windows, Mac and Linux.

(require 'htmlize)

;;; Code:
(defgroup ox-clip nil
  "Customization group for ox-clip."
  :tag "ox-clip"
  :group 'org)


(defcustom ox-clip-w32-cmd
  (format "python %s"
	  (expand-file-name
	   "html-clip-w32.py"
	   (file-name-directory (or load-file-name (buffer-file-name)))))
  "Absolute path to html-clip-w32.py."
  :group 'ox-clip)


(defcustom ox-clip-osx-cmd
  "textutil -inputencoding UTF-8 -stdin -format html -convert rtf -stdout | pbcopy"
  "Command to copy formatted text on osX."
  :group 'ox-clip)


(defcustom ox-clip-linux-cmd
  "xclip -verbose -i /tmp/ox-clip-org.html -t text/html -selection clipboard"
  "Command to copy formatted text on linux."
  :group 'ox-clip)


(defvar ox-clip-w32-py "#!/usr/bin/env python
# Adapted from http://code.activestate.com/recipes/474121-getting-html-from-the-windows-clipboard/
# HtmlClipboard
# An interface to the \"HTML Format\" clipboard data format

__author__ = \"Phillip Piper (jppx1[at]bigfoot.com)\"
__date__ = \"2006-02-21\"
__version__ = \"0.1\"

import re
import win32clipboard

#---------------------------------------------------------------------------
#  Convenience functions to do the most common operation

def HasHtml():
    \"\"\"
    Return True if there is a Html fragment in the clipboard..
    \"\"\"
    cb = HtmlClipboard()
    return cb.HasHtmlFormat()


def GetHtml():
    \"\"\"
    Return the Html fragment from the clipboard or None if there is no Html in the clipboard.
    \"\"\"
    cb = HtmlClipboard()
    if cb.HasHtmlFormat():
        return cb.GetFragment()
    else:
        return None


def PutHtml(fragment):
    \"\"\"
    Put the given fragment into the clipboard.
    Convenience function to do the most common operation
    \"\"\"
    cb = HtmlClipboard()
    cb.PutFragment(fragment)


#---------------------------------------------------------------------------

class HtmlClipboard:

    CF_HTML = None

    MARKER_BLOCK_OUTPUT = \\
        \"Version:1.0\\r\\n\" \\
        \"StartHTML:%09d\\r\\n\" \\
        \"EndHTML:%09d\\r\\n\" \\
        \"StartFragment:%09d\\r\\n\" \\
        \"EndFragment:%09d\\r\\n\" \\
        \"StartSelection:%09d\\r\\n\" \\
        \"EndSelection:%09d\\r\\n\" \\
        \"SourceURL:%s\\r\\n\"

    MARKER_BLOCK_EX = \\
        \"Version:(\\S+)\\s+\" \\
        \"StartHTML:(\\d+)\\s+\" \\
        \"EndHTML:(\\d+)\\s+\" \\
        \"StartFragment:(\\d+)\\s+\" \\
        \"EndFragment:(\\d+)\\s+\" \\
        \"StartSelection:(\\d+)\\s+\" \\
        \"EndSelection:(\\d+)\\s+\" \\
        \"SourceURL:(\\S+)\"
    MARKER_BLOCK_EX_RE = re.compile(MARKER_BLOCK_EX)

    MARKER_BLOCK = \
        \"Version:(\\S+)\\s+\" \\
        \"StartHTML:(\\d+)\\s+\" \\
        \"EndHTML:(\\d+)\\s+\" \\
        \"StartFragment:(\\d+)\\s+\" \\
        \"EndFragment:(\\d+)\\s+\" \\
           \"SourceURL:(\\S+)\"
    MARKER_BLOCK_RE = re.compile(MARKER_BLOCK)

    DEFAULT_HTML_BODY = \
        \"<!DOCTYPE HTML PUBLIC \\\"-//W3C//DTD HTML 4.0 Transitional//EN\\\">\" \\
        \"<HTML><HEAD></HEAD><BODY><!--StartFragment-->%s<!--EndFragment--></BODY></HTML>\"

    def __init__(self):
        self.html = None
        self.fragment = None
        self.selection = None
        self.source = None
        self.htmlClipboardVersion = None


    def GetCfHtml(self):
        \"\"\"
        Return the FORMATID of the HTML format
        \"\"\"
        if self.CF_HTML is None:
            self.CF_HTML = win32clipboard.RegisterClipboardFormat(\"HTML Format\")

        return self.CF_HTML


    def GetAvailableFormats(self):
        \"\"\"
        Return a possibly empty list of formats available on the clipboard
        \"\"\"
        formats = []
        try:
            win32clipboard.OpenClipboard(0)
            cf = win32clipboard.EnumClipboardFormats(0)
            while (cf != 0):
                formats.append(cf)
                cf = win32clipboard.EnumClipboardFormats(cf)
        finally:
            win32clipboard.CloseClipboard()

        return formats


    def HasHtmlFormat(self):
        \"\"\"
        Return a boolean indicating if the clipboard has data in HTML format
        \"\"\"
        return (self.GetCfHtml() in self.GetAvailableFormats())


    def GetFromClipboard(self):
        \"\"\"
        Read and decode the HTML from the clipboard
        \"\"\"

        try:
            win32clipboard.OpenClipboard(0)
            src = win32clipboard.GetClipboardData(self.GetCfHtml())
            self.DecodeClipboardSource(src.decode('utf-8'))
        finally:
            win32clipboard.CloseClipboard()


    def DecodeClipboardSource(self, src):
        \"\"\"
        Decode the given string to figure out the details of the HTML that's on the string
        \"\"\"
                    # Try the extended format first (which has an explicit selection)
        matches = self.MARKER_BLOCK_EX_RE.match(src)
        if matches:
            self.prefix = matches.group(0)
            self.htmlClipboardVersion = matches.group(1)
            self.html = src[int(matches.group(2)):int(matches.group(3))]
            self.fragment = src[int(matches.group(4)):int(matches.group(5))]
            self.selection = src[int(matches.group(6)):int(matches.group(7))]
            self.source = matches.group(8)
        else:
                    # Failing that, try the version without a selection
            matches = self.MARKER_BLOCK_RE.match(src)
            if matches:
                self.prefix = matches.group(0)
                self.htmlClipboardVersion = matches.group(1)
                self.html = src[int(matches.group(2)):int(matches.group(3))]
                self.fragment = src[int(matches.group(4)):int(matches.group(5))]
                self.source = matches.group(6)
                self.selection = self.fragment


    def GetHtml(self, refresh=False):
        \"\"\"
        Return the entire Html document
        \"\"\"
        if not self.html or refresh:
            self.GetFromClipboard()
        return self.html


    def GetFragment(self, refresh=False):
        \"\"\"
        Return the Html fragment. A fragment is well-formated HTML enclosing the selected text
        \"\"\"
        if not self.fragment or refresh:
            self.GetFromClipboard()
        return self.fragment


    def GetSelection(self, refresh=False):
        \"\"\"
        Return the part of the HTML that was selected. It might not be well-formed.
        \"\"\"
        if not self.selection or refresh:
            self.GetFromClipboard()
        return self.selection


    def GetSource(self, refresh=False):
        \"\"\"
        Return the URL of the source of this HTML
        \"\"\"
        if not self.selection or refresh:
            self.GetFromClipboard()
        return self.source


    def PutFragment(self, fragment, selection=None, html=None, source=None):
        \"\"\"
        Put the given well-formed fragment of Html into the clipboard.

        selection, if given, must be a literal string within fragment.
        html, if given, must be a well-formed Html document that textually
        contains fragment and its required markers.
        \"\"\"
        if selection is None:
            selection = fragment
        if html is None:
            html = self.DEFAULT_HTML_BODY % fragment
        if source is None:
            source = \"\"

        fragmentStart = html.index(fragment)
        fragmentEnd = fragmentStart + len(fragment)
        selectionStart = html.index(selection)
        selectionEnd = selectionStart + len(selection)
        self.PutToClipboard(html, fragmentStart, fragmentEnd, selectionStart, selectionEnd, source)


    def PutToClipboard(self, html, fragmentStart, fragmentEnd, selectionStart, selectionEnd, source=\"None\"):
        \"\"\"
        Replace the Clipboard contents with the given html information.
        \"\"\"

        try:
            win32clipboard.OpenClipboard(0)
            win32clipboard.EmptyClipboard()
            src = self.EncodeClipboardSource(html, fragmentStart, fragmentEnd, selectionStart, selectionEnd, source)
            win32clipboard.SetClipboardData(self.GetCfHtml(), src.encode('utf-8'))
        finally:
            win32clipboard.CloseClipboard()


    def EncodeClipboardSource(self, html, fragmentStart, fragmentEnd, selectionStart, selectionEnd, source):
        \"\"\"
        Join all our bits of information into a string formatted as per the HTML format specs.
        \"\"\"
                    # How long is the prefix going to be?
        dummyPrefix = self.MARKER_BLOCK_OUTPUT % (0, 0, 0, 0, 0, 0, source)
        lenPrefix = len(dummyPrefix)

        prefix = self.MARKER_BLOCK_OUTPUT % (lenPrefix, len(html)+lenPrefix,
                        fragmentStart+lenPrefix, fragmentEnd+lenPrefix,
                        selectionStart+lenPrefix, selectionEnd+lenPrefix,
                        source)
        return (prefix + html)


def DumpHtml():

    cb = HtmlClipboard()
    print(\"GetAvailableFormats()=%s\" % str(cb.GetAvailableFormats()))
    print(\"HasHtmlFormat()=%s\" % str(cb.HasHtmlFormat()))
    if cb.HasHtmlFormat():
        cb.GetFromClipboard()
        print(\"prefix=>>>%s<<<END\" % cb.prefix)
        print(\"htmlClipboardVersion=>>>%s<<<END\" % cb.htmlClipboardVersion)
        print(\"GetSelection()=>>>%s<<<END\" % cb.GetSelection())
        print(\"GetFragment()=>>>%s<<<END\" % cb.GetFragment())
        print(\"GetHtml()=>>>%s<<<END\" % cb.GetHtml())
        print(\"GetSource()=>>>%s<<<END\" % cb.GetSource())

        
if __name__ == '__main__':
    import sys
    data = sys.stdin.read()
    PutHtml(data)
"
  "Windows Python Script for copying formatted text.")

;; Create the windows python script if needed.
(when (and (eq system-type 'windows-nt)
	   (not (file-exists-p ox-clip-w32-cmd)))
  (with-temp-file "html-clip-w32.py"
    (insert ox-clip-w32-py)))


;;;###autoload
(defun ox-clip-formatted-copy (r1 r2)
  "Export the selected region to HTML and copy it to the clipboard.
R1 and R2 define the selected region."
  (interactive "r")
  (copy-region-as-kill r1 r2)
  (if (equal major-mode 'org-mode)
      (save-window-excursion
	(let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
	       (html (with-current-buffer buf (buffer-string))))
	  (cond
	   ((eq system-type 'windows-nt)
	    (with-current-buffer buf
	      (shell-command-on-region
	       (point-min)
	       (point-max)
	       ox-clip-w32-cmd)))
	   ((eq system-type 'darwin)
	    (with-current-buffer buf
	      (shell-command-on-region
	       (point-min)
	       (point-max)
	       ox-clip-osx-cmd)))
	   ((eq system-type 'gnu/linux)
	    ;; For some reason shell-command on region does not work with xclip.
	    (with-temp-file "/tmp/ox-clip-org.html"
	      (insert (with-current-buffer buf (buffer-string))))
	    (apply
	     'start-process "ox-clip" "*ox-clip*"
	     (split-string ox-clip-linux-cmd " ")))) 
	  (kill-buffer buf)))
    ;; Use htmlize when not in org-mode.
    (let ((html (htmlize-region-for-paste r1 r2)))
      (cond
       ((eq system-type 'windows-nt)
	(with-temp-buffer
	  (insert html)
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   ox-clip-w32-cmd)))
       ((eq system-type 'darwin)
	(with-temp-buffer
	  (insert html)
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   ox-clip-osx-cmd)))
       ((eq system-type 'gnu/linux)
	(with-temp-file "/tmp/ox-clip-org.html"
	  (insert html))
	(apply
	 'start-process "ox-clip" "*ox-clip*"
	 (split-string ox-clip-linux-cmd " ")))))))

(provide 'ox-clip)

;;; ox-clip.el ends here

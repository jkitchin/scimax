// @license magnet:?xt=urn:btih:1f739d935676111cfff4b4693e3816e664797050&dn=gpl-3.0.txt GPL-v3-or-Later
// How many characters to include on either side of match keyword
const summaryInclude=60;

// Options for fuse.js
let fuseOptions = {
  shouldSort: true,
  includeMatches: true,
  tokenize: true,
  matchAllTokens: true,
  threshold: 0.0,
  location: 0,
  distance: 100,
  maxPatternLength: 64,
  minMatchCharLength: 3,
  keys: [
    {name:"title",weight:0.8},
    {name:"tags",weight:0.5},
    {name:"categories",weight:0.5},
    {name:"contents",weight:0.4}
  ]
};

function getUrlParameter(name) {
  name = name.replace(/[\[]/, '\\[').replace(/[\]]/, '\\]');
  let regex = new RegExp('[\\?&]' + name + '=([^&#]*)');
  let results = regex.exec(location.search);
  return results === null ? '' : decodeURIComponent(results[1].replace(/\+/g, ' '));
}

let searchQuery = getUrlParameter('q');

if(searchQuery){
  document.getElementById("search-query").value = searchQuery;
  executeSearch(searchQuery);
} else {
  document.getElementById('search-results').innerHTML = "<p class=\"no-results\">Please enter a word or phrase above</p>";
}

function executeSearch(searchQuery) {
  // Look for "index.json" in the same directory where this script is called.
  fetch("index.json").
  then(function (response) {
    return response.json()
  }).
  then(function (data) {
    let fuse = new Fuse(data, fuseOptions);
    let result = fuse.search(searchQuery);
    if (result.length > 0) {
      populateResults(result);
    } else {
      document.getElementById('search-results').innerHTML = "<p class=\"no-results\">No matches found</p>";
    }
  });
}

function populateResults(result){
  result.forEach( function (value, key) {
    let contents= value.item.contents;
    let snippet = "";
    let snippetHighlights=[];
    snippetHighlights.push(searchQuery);
    if(snippet.length<1){
      var getSentenceByWordRegex = new RegExp( 
        `[^.?!]*(?<=[.?\\s!])${searchQuery}(?=[\\s.?!])[^.?!]*[.?!]`,
        'i'
      );
      var maxTextLength = summaryInclude*2
      // Index of the matched search term
      var indexOfMatch = contents.toLowerCase().indexOf(
        searchQuery.toLowerCase()
      );
      // Index of the first word of the sentence with the search term in it
      var indexOfSentence = contents.indexOf(
        getSentenceByWordRegex.exec(contents)
      );
      
      var start 
      var cutStart = false
      // Is the match in the result?
      if(indexOfSentence+maxTextLength < indexOfMatch){
        // Make sure that the match is in the result
        start = indexOfMatch
        // This bool is used to replace the first part with '...'
        cutStart = true 
      } else {
        // Match is in view, even if we show the whole sentence
        start = indexOfSentence
      }
      
      // Change end length to the text length if it is longer than 
      // the text length to prevent problems
      var end = start + maxTextLength 
      if (end > contents.length){
        end = contents.length
      }

      if(cutStart){
        // Replace first three characters with '...'
        end -= 3;
        snippet += "…" + contents.substring(start, end).trim();
      }
      else{
        snippet += contents.substring(start, end).trim();
      }     
    }
    snippet += "…";

    // Lifted from https://stackoverflow.com/posts/3700369/revisions
    var elem = document.createElement('textarea');
    elem.innerHTML = snippet;
    var decoded = elem.value;

    // Pull template from hugo template definition
    let frag = document.getElementById('search-result-template').content.cloneNode(true);
    // Replace values
    frag.querySelector(".search_summary").setAttribute("id", "summary-" + key);
    frag.querySelector(".search_link").setAttribute("href", value.item.permalink);
    frag.querySelector(".search_title").textContent = value.item.title;
    frag.querySelector(".search_snippet").textContent = decoded;
    let tags = value.item.tags;
    if (tags) {
      frag.querySelector(".search_tags").textContent = tags;
    } else {
      frag.querySelector(".search_iftags").remove();
    }
    let categories = value.item.categories;
    if (categories) {
      frag.querySelector(".search_categories").textContent = categories;
    } else {
      frag.querySelector(".search_ifcategories").remove();
    }
    snippetHighlights.forEach( function (snipvalue, snipkey) {
      let markjs = new Mark(frag);
      markjs.mark(snipvalue);
    });
    document.getElementById("search-results").appendChild(frag);
  });
}
// @license-end

// initialize an introjs instance          
var intro = introJs();

// handler 1
Shiny.addCustomMessageHandler("setHelpContent",
  
  // callback function. 
  // note: data is passed by shiny and contains the tour data
  function(data){

    // load data 
    intro.setOptions({steps: data});
  }
);

// handler 2
Shiny.addCustomMessageHandler("startHelp",
  
  // callback function
  function(message) {

    // start intro.js
    // note: we don't need information from shiny, just start introJS
    intro.start();
  }
  
);
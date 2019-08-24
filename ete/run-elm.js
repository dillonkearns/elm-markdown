#! /usr/bin/env node
const { Elm } = require("./elm.js");
console.warn = function(message) {};

let stdin = process.openStdin();

let data = "";
stdin.on("data", function(chunk) {
  data += chunk;
});

stdin.on("end", function() {
  const app = Elm.OutputMarkdownHtml.init({ flags: data });

  app.ports.printOutput.subscribe(output => {
    console.log(output);
  });

  app.ports.error.subscribe(output => {
    console.error(output);
    process.exit(127);
  });
});

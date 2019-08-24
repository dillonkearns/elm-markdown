#! /usr/bin/env node
const { Elm } = require("./elm.js");

const markdownInput = "# Title example";
const app = Elm.OutputMarkdownHtml.init({ flags: markdownInput });

app.ports.printOutput.subscribe(output => {
  console.log(output);
});

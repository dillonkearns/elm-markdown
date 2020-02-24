const { Elm } = require("../elm.js");

function runner(markdown) {
  return new Promise(resolve => {
    console.warn = function(message) {};
    const app = Elm.OutputMarkdownHtml.init({});

    app.ports.printOutput.subscribe(output => {
      resolve(output);
    });

    app.ports.error.subscribe(output => {
      resolve("ERROR\n" + output);
    });

    // console.log("@@@@@@@");
    // console.log(markdown);
    app.ports.requestHtml.send(markdown);
  });
}

module.exports = function(markdown) {
  return runner(markdown);
};

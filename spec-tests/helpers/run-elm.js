const { Elm } = require("../elm.js");

// Reuse single Elm app instance across all tests
let app = null;
let pendingResolve = null;

function init() {
  // Suppress Elm's console.warn during init
  const originalWarn = console.warn;
  console.warn = function() {};

  app = Elm.OutputMarkdownHtml.init({});

  console.warn = originalWarn;

  app.ports.printOutput.subscribe(output => {
    if (pendingResolve) {
      const resolve = pendingResolve;
      pendingResolve = null;
      resolve(output);
    }
  });

  app.ports.error.subscribe(output => {
    if (pendingResolve) {
      const resolve = pendingResolve;
      pendingResolve = null;
      resolve("ERROR\n" + output);
    }
  });
}

function runner(markdown) {
  if (!app) init();

  return new Promise(resolve => {
    pendingResolve = resolve;
    app.ports.requestHtml.send(markdown);
  });
}

module.exports = runner;

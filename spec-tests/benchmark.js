/**
 * Benchmark script for elm-markdown parser performance.
 *
 * Usage:
 *   cd spec-tests
 *   npx elm make OutputMarkdownHtml.elm --optimize --output elm.js
 *   node benchmark.js
 */

const { Elm } = require("./elm.js");

const app = Elm.OutputMarkdownHtml.init({});

let pending = null;
app.ports.printOutput.subscribe(output => { if (pending) pending(output); });
app.ports.error.subscribe(output => { if (pending) pending("ERROR: " + output); });

function parse(markdown) {
  return new Promise(resolve => {
    pending = resolve;
    app.ports.requestHtml.send(markdown);
  });
}

async function benchmark(name, markdown, iterations = 500) {
  // Warmup
  for (let i = 0; i < 10; i++) await parse(markdown);

  const start = Date.now();
  for (let i = 0; i < iterations; i++) {
    await parse(markdown);
  }
  const elapsed = Date.now() - start;
  const perOp = (elapsed / iterations).toFixed(3);

  console.log(`${name.padEnd(45)} ${perOp}ms  (${markdown.length} chars)`);
  return parseFloat(perOp);
}

async function run() {
  console.log("=".repeat(70));
  console.log("elm-markdown Performance Benchmark");
  console.log("=".repeat(70));
  console.log();

  console.log("--- Plain Text (benefits from tokenization fast-path) ---");
  await benchmark("Plain text, no formatting", "Lorem ipsum dolor sit amet. ".repeat(50));
  await benchmark("Plain text with newlines", ("Lorem ipsum dolor sit amet. ".repeat(10) + "\n\n").repeat(5));
  console.log();

  console.log("--- Formatted Content ---");
  await benchmark("Bold and italic", "Text with **bold** and *italic* words. ".repeat(20));
  await benchmark("Code spans", "Use `code` and `more code` here. ".repeat(20));
  await benchmark("Links", "See [link one](url1) and [link two](url2). ".repeat(20));
  await benchmark("Mixed inline formatting", ("**bold** *italic* `code` [link](url) ").repeat(50));
  console.log();

  console.log("--- Tables (benefits from cell parsing optimization) ---");
  const smallTable = "| A | B | C |\n|---|---|---|\n| 1 | 2 | 3 |";
  const largeTable = "| Col1 | Col2 | Col3 | Col4 | Col5 |\n|------|------|------|------|------|\n" +
    Array(50).fill("| cell | data | more | text | here |").join("\n");
  const longCellTable = "| " + "x".repeat(200) + " | " + "y".repeat(200) + " |\n|---|---|\n| a | b |";

  await benchmark("Small table (9 cells)", smallTable);
  await benchmark("Large table (250 cells)", largeTable);
  await benchmark("Table with long cells (200 chars each)", longCellTable);
  console.log();

  console.log("--- Real-World Documents ---");
  const readme = `# Project Title

A brief description of what this project does and who it's for.

## Installation

\`\`\`bash
npm install my-package
\`\`\`

## Usage

Here is how to use **this package** in your code:

\`\`\`javascript
const pkg = require('my-package');
pkg.doSomething();
\`\`\`

## Features

- Feature one with *emphasis*
- Feature two with \`inline code\`
- Feature three with [a link](https://example.com)

## API

| Method | Description |
|--------|-------------|
| \`doSomething()\` | Does something useful |
| \`doOther(thing)\` | Does other things |

## License

MIT - see [LICENSE](LICENSE) for details.
`;

  await benchmark("Typical README (~500 chars)", readme);
  await benchmark("README x5 (~2500 chars)", readme.repeat(5));
  await benchmark("README x10 (~5000 chars)", readme.repeat(10));
  console.log();

  console.log("--- Pathological Cases (stress tests) ---");
  await benchmark("Long line, no formatting (10k chars)", "a".repeat(10000));
  await benchmark("Many inline elements (200 bolds)", Array(200).fill("**bold**").join(" "));
  await benchmark("Deeply nested list", Array(20).fill(null).map((_, i) => "  ".repeat(i) + "- item").join("\n"));
  console.log();

  console.log("=".repeat(70));
  console.log("Benchmark complete.");
  console.log("=".repeat(70));
}

run().catch(console.error);

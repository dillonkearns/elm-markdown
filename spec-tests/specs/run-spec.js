const path = require("path");
const load = require("../helpers/load.js");
const runElm = require("../helpers/run-elm.js");
const htmlDiffer = require("../helpers/html-differ.js");
const fs = require("fs");
var stringify = require("json-stable-stringify");

function runSpecs(title, dir, showCompletionTable, options) {
  options = options || {};
  const specs = load.loadFiles(path.resolve(__dirname, dir));

  if (showCompletionTable) {
    load.outputCompletionTable(title, specs);
  }

  describe(title, () => {
    afterAll(() => {
      printStatus();
      writeFailuresMarkdown(title);
      writePassingMarkdown(title);
    });

    Object.keys(specs)
      .sort()
      .forEach((section) => {
        describe(section, () => {
          specs[section].specs.forEach((spec) => {
            spec.options = Object.assign({}, options, spec.options || {});
            const example = spec.example ? " example " + spec.example : "";
            spec.suiteTitle = title;
            const passFail = spec.shouldFail ? "fail" : "pass";
            if (typeof spec.options.silent === "undefined") {
              spec.options.silent = true;
            }
            if (spec.options.sanitizer) {
              // eslint-disable-next-line no-eval
              spec.options.sanitizer = eval(spec.options.sanitizer);
            }
            (spec.only ? fit : spec.skip ? xit : it)(
              "should " + passFail + example,
              (done) => {
                const before = process.hrtime();

                runElm(spec.markdown).then((actual) => {
                  htmlDiffer.firstDiff(actual, spec.html).then((diff) => {
                    if (spec.shouldFail) {
                      expect(spec).not.toRender(actual, diff, spec.html);
                    } else {
                      expect(spec).toRender(actual, diff, spec.html);
                    }
                    const elapsed = process.hrtime(before);
                    if (elapsed[0] > 0) {
                      const s = (elapsed[0] + elapsed[1] * 1e-9).toFixed(3);
                      fail(`took too long: ${s}s`);
                    }
                    done();
                  });
                });
              }
            );
          });
        });
      });
  });
}

runSpecs("GFM", "./gfm", true, {
  gfm: true,
  pedantic: false,
  headerIds: false,
});
runSpecs("CommonMark", "./commonmark", true, {
  gfm: false,
  pedantic: false,
  headerIds: false,
});
runSpecs("Original", "./original", false, { gfm: false, pedantic: true });
runSpecs("New", "./new");
runSpecs("ReDOS", "./redos");
runSpecs("Security", "./security", false, { silent: true }); // silent - do not show deprecation warning

function printStatus() {
  /** @typedef { { markdown: string; suiteTitle: string; html: string; example: number; start_line: number; end_line: number; section: string; options: { gfm: boolean; pedantic: boolean; headerIds: boolean; silent: boolean; }; } } Spec */
  // @ts-ignore
  /** @type {Spec[]]} */ let failures = global.fails;
  // @ts-ignore
  /** @type {Spec[]]} */ let passes = global.passes;
  const passedJson = passes.reduce(function (
    /** @type {Object} */ accumulator,
    spec
  ) {
    accumulator[spec.suiteTitle] = accumulator[spec.suiteTitle] || {};
    accumulator[spec.suiteTitle][spec.section] =
      accumulator[spec.suiteTitle][spec.section] || [];
    accumulator[spec.suiteTitle][spec.section].push(spec.example);

    // Pass the object on to the next loop
    return accumulator;
  },
  {});
  Object.keys(passedJson)
    .sort()
    .forEach((suiteTitle) => {
      Object.keys(passedJson[suiteTitle])
        .sort()
        .forEach((section) => {
          passedJson[suiteTitle][section].sort();
        });
    });
  fs.writeFileSync("./spec-results.json", stringify(passedJson, { space: 2 }));
}

function writeFailuresMarkdown(/** @type {string} */ suiteTitle) {
  /** @typedef { { markdown: string; suiteTitle: string; html: string; example: number; start_line: number; end_line: number; section: string; options: { gfm: boolean; pedantic: boolean; headerIds: boolean; silent: boolean; }; } } Spec */
  // @ts-ignore
  /** @type {Spec[]]} */ let failures = global.fails;
  const failedJson = failures.reduce(function (
    /** @type {Object} */ accumulator,
    spec
  ) {
    if (spec.suiteTitle === suiteTitle) {
      accumulator[spec.section] = accumulator[spec.section] || [];
      accumulator[spec.section].push(spec);
    }
    // Pass the object on to the next loop
    return accumulator;
  },
  {});

  /** @type {Object} */ let markdownSections = {};
  Object.keys(failedJson)
    .sort()
    .forEach((section) => {
      /** @type {string} */ let sectionMarkdown = `# ${suiteTitle} - ${section}\n\n`;
      failedJson[section].sort(
        (/** @type {Spec} */ specA, /** @type {Spec} */ specB) => {
          if (specA.example && specB.example) {
            return specA.example - specB.example;
          } else {
            return 0;
          }
        }
      );
      failedJson[section].forEach((/** @type {Spec} */ spec) => {
        if (spec.start_line && spec.example) {
          sectionMarkdown += `## [Example ${spec.example}](https://spec.commonmark.org/0.30/#example-${spec.example})`;
        } else if (spec.example) {
          sectionMarkdown += `## [Example ${spec.example}](https://github.github.com/gfm/#example-${spec.example})`;
        } else {
          sectionMarkdown += `## Example ${spec.example}`;
        }

        sectionMarkdown += `

This markdown:

\`\`\`\`\`\`\`\`\`\`\`\`markdown
${spec.markdown}
\`\`\`\`\`\`\`\`\`\`\`\`

Should give output:

\`\`\`\`\`\`\`\`\`\`\`\`html
${spec.diff.expected}
\`\`\`\`\`\`\`\`\`\`\`\`

But instead was:

\`\`\`\`\`\`\`\`\`\`\`\`html
${spec.diff.actual}
\`\`\`\`\`\`\`\`\`\`\`\`
`;
      });
      fs.mkdirSync(`./test-results/failing/${suiteTitle}`, {
        recursive: true,
      });

      fs.writeFileSync(
        `./test-results/failing/${suiteTitle}/${section}.md`,
        sectionMarkdown
      );
    });
}

function writePassingMarkdown(/** @type {string} */ suiteTitle) {
  /** @typedef { { markdown: string; suiteTitle: string; html: string; example: number; start_line: number; end_line: number; section: string; options: { gfm: boolean; pedantic: boolean; headerIds: boolean; silent: boolean; }; } } Spec */
  // @ts-ignore
  /** @type {Spec[]]} */ let passes = global.passes;
  const passedJson = passes.reduce(function (
    /** @type {Object} */ accumulator,
    spec
  ) {
    if (spec.suiteTitle === suiteTitle) {
      accumulator[spec.section] = accumulator[spec.section] || [];
      accumulator[spec.section].push(spec);
    }
    // Pass the object on to the next loop
    return accumulator;
  },
  {});

  /** @type {string} */ let markdown = "";
  markdown += `# ${suiteTitle}\n\n`;
  Object.keys(passedJson)
    .sort()
    .forEach((section) => {
      markdown += `## ${section}\n\n`;
      passedJson[section].sort(
        (/** @type {Spec} */ specA, /** @type {Spec} */ specB) => {
          if (specA.example && specB.example) {
            return specA.example - specB.example;
          } else {
            // return 0;
            throw `${section} couldn't sort. \nA: ${specA}\n B: ${specB}`;
          }
        }
      );
      passedJson[section].forEach((/** @type {Spec} */ spec) => {
        if (spec.start_line && spec.example) {
          markdown += `### [Example ${spec.example}](https://spec.commonmark.org/0.30/#example-${spec.example})`;
        } else if (spec.example) {
          markdown += `### [Example ${spec.example}](https://github.github.com/gfm/#example-${spec.example})`;
        } else {
          markdown += `### Example ${spec.example}`;
        }
        markdown += `

This markdown:


\`\`\`\`\`\`\`\`\`\`\`\`markdown
${spec.markdown}
\`\`\`\`\`\`\`\`\`\`\`\`

Gives this correct output:


\`\`\`\`\`\`\`\`\`\`\`\`html
${spec.html}
\`\`\`\`\`\`\`\`\`\`\`\`

`;
      });
    });

  fs.writeFileSync(`./test-results/passing-${suiteTitle}.md`, markdown);
}

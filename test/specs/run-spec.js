const path = require("path");
const load = require("../helpers/load.js");
const runElm = require("../helpers/run-elm.js");
const htmlDiffer = require("../helpers/html-differ.js");
const fs = require("fs");

function runSpecs(title, dir, showCompletionTable, options) {
  options = options || {};
  const specs = load.loadFiles(path.resolve(__dirname, dir));

  if (showCompletionTable) {
    load.outputCompletionTable(title, specs);
  }

  describe(title, () => {
    afterAll(() => {
      printStatus();
    });

    Object.keys(specs).forEach(section => {
      describe(section, () => {
        specs[section].specs.forEach(spec => {
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
            done => {
              const before = process.hrtime();

              runElm(spec.markdown).then(actual => {
                htmlDiffer.firstDiff(actual, spec.html).then(diff => {
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
  headerIds: false
});
runSpecs("CommonMark", "./commonmark", true, {
  gfm: false,
  pedantic: false,
  headerIds: false
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
  const passedJson = passes.reduce(function(
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
  Object.keys(passedJson).forEach(suiteTitle => {
    Object.keys(passedJson[suiteTitle]).forEach(section => {
      passedJson[suiteTitle][section].sort();
    });
  });
  fs.writeFileSync("./spec-results.json", JSON.stringify(passedJson));
}

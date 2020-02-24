/** @type {Spec[]} */
let passes = [];
/** @type {Spec[]} */
let fails = [];

// @ts-ignore
global.fails = fails;
// @ts-ignore
global.passes = passes;

/** @typedef { { markdown: string; diff: string; html: string; example: number; start_line: number; end_line: number; section: string; options: { gfm: boolean; pedantic: boolean; headerIds: boolean; silent: boolean; }; } } Spec */

let example = {
  markdown: "> aaa\n***\n> bbb\n",
  html:
    "<blockquote>\n<p>aaa</p>\n</blockquote>\n<hr />\n<blockquote>\n<p>bbb</p>\n</blockquote>\n",
  example: 216,
  start_line: 3631,
  end_line: 3643,
  section: "Block quotes",
  options: { gfm: false, pedantic: false, headerIds: false, silent: true }
};

beforeEach(() => {
  jasmine.addMatchers({
    toRender: () => {
      return {
        compare: (/** @type {Spec} */ spec, actual, diff, expected) => {
          // console.log("spec", spec);
          const result = {};
          result.pass = !diff;
          if (result.pass) {
            passes.push(spec);
          } else {
            spec.diff = diff;
            fails.push(spec);
          }

          if (!diff) {
            result.message = `${spec.markdown}\n------\n\nExpected: Should Fail`;
          } else {
            result.message = `Expected: ${diff.expected}\n  Actual: ${diff.actual}`;
          }
          return result;
        }
      };
    }
  });
});

// afterAll(() => {
//   console.log(`@@@@
// Passed ${passes.length}

// Fails ${fails.length}

// @@@@
// `);
// });

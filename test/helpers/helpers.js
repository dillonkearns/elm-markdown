const htmlDiffer = require("./html-differ.js");
const runElm = require("./run-elm.js");

beforeEach(() => {
  jasmine.addMatchers({
    toRender: () => {
      return {
        compare: async (spec, expected) => {
          const result = {};
          const actual = await runElm(spec.markdown);

          if (result.pass) {
            result.message = `${spec.markdown}\n------\n\nExpected: Should Fail`;
          } else {
            const diff = htmlDiffer.firstDiff(actual, expected);
            result.message = `Expected: ${diff.expected}\n  Actual: ${diff.actual}`;
          }
          return result;
        }
      };
    }
  });
});

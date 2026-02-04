const HtmlDiffer = require("@markedjs/html-differ").HtmlDiffer;
const htmlDiffer = new HtmlDiffer({ ignoreSelfClosingSlash: true });

module.exports = {
  isEqual: htmlDiffer.isEqual.bind(htmlDiffer),
  firstDiff: async (actual, expected, padding) => {
    // Fast path: exact string match (rare but essentially free)
    if (actual === expected) {
      return null;
    }

    // Use library's isEqual (handles whitespace, self-closing, etc.)
    const equal = await htmlDiffer.isEqual(actual, expected);
    if (equal) {
      return null;
    }

    // Only compute diff when test fails (need it for error message)
    const diffs = await htmlDiffer.diffHtml(actual, expected);
    const result = diffs.reduce(
      (obj, diff) => {
        if (diff.added) {
          if (obj.firstIndex === null) {
            obj.firstIndex = obj.expected.length;
          }
          obj.expected += diff.value;
        } else if (diff.removed) {
          if (obj.firstIndex === null) {
            obj.firstIndex = obj.actual.length;
          }
          obj.actual += diff.value;
        } else {
          obj.actual += diff.value;
          obj.expected += diff.value;
        }

        return obj;
      },
      {
        firstIndex: null,
        actual: "",
        expected: "",
      }
    );

    if (padding) {
      return {
        actual: result.actual.substring(
          result.firstIndex - padding,
          result.firstIndex + padding
        ),
        expected: result.expected.substring(
          result.firstIndex - padding,
          result.firstIndex + padding
        ),
      };
    } else {
      return {
        actual: result.actual,
        expected: result.expected,
      };
    }
  },
};

beforeEach(() => {
  jasmine.addMatchers({
    toRender: () => {
      return {
        compare: (spec, actual, diff, expected) => {
          const result = {};

          if (result.pass) {
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

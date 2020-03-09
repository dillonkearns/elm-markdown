# Contributing to `dillonkearns/elm-markdown`

Thanks for taking a look at this document!

The Github-Flavored Markdown spec is huge so I would very much appreciate community contributions for this project!

Before diving into a pull request, here are some things to keep in mind:

- Make sure you're familiar with [this project's philosophy and goals](https://github.com/dillonkearns/elm-markdown#philosophy--goals)
- In particular, keep in mind that this project prioritizes **helpful errors** over fault-tolerance whenever there is a tradeoff between the two

## Getting help and communicating
If you're not already on the Elm slack, take a moment to [setup an Elm slack account](http://elmlang.herokuapp.com/).

The `#markdown` channel on the Elm slack is a great place to discuss things, including:

* Asking for help as you're contributing
* Asking for feedback on design ideas

Sometimes it's difficult to stay on top of a lot of Github issues, so if you have a quick question, Slack is the best place to get a fast answer to a small question.

### Live Streams
You can also check out the [Incremental Elm Live](https://incrementalelm.com/live) series. I do live coding on Elm Open Source there, sometimes on `elm-markdown`.

If you have an issue that would be fun to pair on, let me know and maybe we can work on it in a live stream!

## Finding a good place to contribute

Two great places to start looking for places to contribute:

- [Open issues](https://github.com/dillonkearns/elm-markdown) (especially [ones marked _good first issue_](https://github.com/dillonkearns/elm-markdown/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22))
- Take a look at the [current failures for the Github-Flavored Markdown Spec](https://github.com/dillonkearns/elm-markdown/tree/master/test-results/failing/GFM) (the output is super easy to read and includes links to the relevant section in the GFM Spec)

## Running the Tests

### Unit Tests

There is a suite of Elm unit tests in the `tests` folder.

Run those with

```shell
npm test
```

Or you can watch the tests with

```shell
npm run autotest
```

### End-to-end tests

You can see the current result of the end-to-end tests by running

```shell
npm run test:ete # this will fail
```

Note that this will have several failures and a non-zero exit status. When you make a change, what you want to do is make sure that there are no **new failures** (i.e. regressions). To do this, you can simply `git diff` the `spec-results.json` file

```
git diff spec-results.json
```

If there is any red, it means a test that had been passing has now started to fail 🛑

If there is green, that means a new test has started to pass! ✅ 🎉

Just be sure that the diff only has new passing tests added, and none removed, and you're in good shape! You can commit the new `spec-results.json` file whenever you get new tests passing.

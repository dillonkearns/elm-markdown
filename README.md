# elm-markdown-parser
[![Build Status](https://travis-ci.org/dillonkearns/elm-markdown.svg?branch=master)](https://travis-ci.org/dillonkearns/elm-markdown)

Extensible markdown parsing in pure elm.

## Philosophy & Goals

- Render markdown to any type (`Html`, `elm-ui` `Element`s, `String`s representing ANSI color codes for terminal output... or even a function, allowing you to inject dynamic values into your markdown view)
- Extend markdown without adding to the syntax using custom HTML renderers, and fail explicitly for unexpected HTML tags, or missing attributes within those tags
- Allow users to give custom parsing failures with nice error messages (for example, broken links, or custom validation like titles that are too long)

### Parsing Goals
This is evolving and I would like input on the direction of parsing. My current thinking is that this library should:

- Do not add any new syntax, this library has a subset of the features of Github flavored markdown.
- Only parse the [Github-flavored markdown style](https://github.github.com/gfm/) (not CommonMark or other variants)
- (This breaks GFM compliance in favor of explicit errors) All markdown is valid in github-flavored markdown and other variants. This library aims to give explicit errors instead of falling back and silently continuing, see example below
- Only deviate from Github-flavored markdown rules when it helps give better error feedback for "things you probably didn't mean to do." In all other cases, follow the Github-flavored markdown spec.

## Current Github-flavored markdown compliance
The test suite for this library runs through all the expected outputs outlined in the GFM spec. It uses the same test suite to test these cases as highlight.js (the library that `elm-explorations/elm-markdown` uses under the hood).

You can see the latest passing and failing tests from this test suite [in the `test-results` folder](https://github.com/dillonkearns/elm-markdown/tree/master/test-results). Note: because some of the files are very long, Github won't render them correctly (they are in markdown with clickable links). If you want to see a nicer formatted version, you can clone this repo and use a Markdown preview tool in your text editor of choice to open them up.

### Examples of fallback behavior
Github flavored markdown behavior:
Links with missing closing parens are are rendered as raw text instead of links

```markdown
[My link](/home/ wait I forgot to close the link
```

Renders the raw string instead of a link, like so:

```html
<p>
[My link](/home/ wait I forgot to close the link
</p>
```

This library gives an error message here, and aims to do so in similar situations.

## Contributors

Thank you [@jinjor](https://github.com/jinjor) for your
[`elm-xml-parser`](https://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser) package!

I needed to tweak it so I copied it into the project, but it is one of the dependencies and it worked
without a hitch!

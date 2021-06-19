# GFM - ATX headings

## [Example 38](https://spec.commonmark.org/0.29/#example-38)

This markdown:

````````````markdown
 ### foo
  ## foo
   # foo

````````````

Should give output:

````````````html
<h3>foo</h3><h2>foo</h2><h1>foo</h1>
````````````

But instead was:

````````````html
ERROR Problem at row 1 Expecting symbol - Problem at row 1 Expecting symbol + Problem at row 1 Expecting symbol *
````````````
## [Example 40](https://spec.commonmark.org/0.29/#example-40)

This markdown:

````````````markdown
foo
    # bar

````````````

Should give output:

````````````html
<p>foo # bar</p>
````````````

But instead was:

````````````html
ERROR Problem at row 2 Expecting symbol - Problem at row 2 Expecting symbol + Problem at row 2 Expecting symbol *
````````````
## [Example 41](https://spec.commonmark.org/0.29/#example-41)

This markdown:

````````````markdown
## foo ##
  ###   bar    ###

````````````

Should give output:

````````````html
<h2>foo</h2><h3>bar</h3>
````````````

But instead was:

````````````html
ERROR Problem at row 2 Expecting symbol - Problem at row 2 Expecting symbol + Problem at row 2 Expecting symbol *
````````````

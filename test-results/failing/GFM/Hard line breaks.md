# GFM - Hard line breaks

## [Example 642](https://spec.commonmark.org/0.30/#example-642)

This markdown:

````````````markdown
<a href="foo  
bar">

````````````

Should give output:

````````````html
<p><a href="foo  
bar"></p>
````````````

But instead was:

````````````html
ERROR Problem at row 3 Expecting symbol
````````````
## [Example 643](https://spec.commonmark.org/0.30/#example-643)

This markdown:

````````````markdown
<a href="foo\
bar">

````````````

Should give output:

````````````html
<p><a href="foo\
bar"></p>
````````````

But instead was:

````````````html
ERROR Problem at row 3 Expecting symbol
````````````

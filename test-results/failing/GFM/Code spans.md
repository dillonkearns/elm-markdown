# GFM - Code spans

## [Example 344](https://spec.commonmark.org/0.30/#example-344)

This markdown:

````````````markdown
<a href="`">`

````````````

Should give output:

````````````html
<p><a href="`">`</p>
````````````

But instead was:

````````````html
ERROR Problem at row 2 Expecting symbol
````````````

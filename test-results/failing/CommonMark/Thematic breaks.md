# CommonMark - Thematic breaks

## [Example 19](https://spec.commonmark.org/0.29/#example-19)

This markdown:

````````````markdown
Foo
    ***

````````````

Should give output:

````````````html
<p>Foo ***</p>
````````````

But instead was:

````````````html
ERROR Problem at row 2 Expected one or more character
````````````
## [Example 26](https://spec.commonmark.org/0.29/#example-26)

This markdown:

````````````markdown
 *-*

````````````

Should give output:

````````````html
<p><em>-</em></p>
````````````

But instead was:

````````````html
ERROR Problem at row 1 Expecting a newline Problem at row 1 Expecting a carriage return Problem at row 1 Expecting the end of the input Problem at row 1 Expected one or more character
````````````

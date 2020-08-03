# GFM - [extension] Tables

## [Example 201](https://github.github.com/gfm/#example-201)

This markdown:

````````````markdown
| abc | def |
| --- | --- |
| bar | baz |
> bar
````````````

Should give output:

````````````html
<table><thead><tr><th>abc</th><th>def</th></tr></thead><tbody><tr><td>bar</td><td>baz</td></tr></tbody></table><blockquote><p>bar</p></blockquote>
````````````

But instead was:

````````````html
<table><thead><tr><th>abc</th><th>def</th></tr></thead><tbody><tr><td>bar</td><td>baz</td></tr><tr><td>&gt; bar</td><td></td></tr></tbody></table>
````````````

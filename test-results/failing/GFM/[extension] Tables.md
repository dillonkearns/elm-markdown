# GFM - [extension] Tables

## [Example 199](https://github.github.com/gfm/#example-199)

This markdown:

````````````markdown
| abc | defghi |
:-: | -----------:
bar | baz
````````````

Should give output:

````````````html
<table><thead><tr><th align="center">abc</th><th align="right">defghi</th></tr></thead><tbody><tr><td align="center">bar</td><td align="right">baz</td></tr></tbody></table>
````````````

But instead was:

````````````html
<p>| abc | defghi | :-: | -----------: bar | baz</p>
````````````
## [Example 200](https://github.github.com/gfm/#example-200)

This markdown:

````````````markdown
| f\|oo  |
| ------ |
| b `\|` az |
| b **\|** im |
````````````

Should give output:

````````````html
<table><thead><tr><th>f|oo</th></tr></thead><tbody><tr><td>b<code>|</code>az</td></tr><tr><td>b<strong>|</strong>im</td></tr></tbody></table>
````````````

But instead was:

````````````html
<p>| f|oo | | ------ | | b<code>\|</code>az | | b<strong>|</strong>im |</p>
````````````
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
## [Example 202](https://github.github.com/gfm/#example-202)

This markdown:

````````````markdown
| abc | def |
| --- | --- |
| bar | baz |
bar

bar
````````````

Should give output:

````````````html
<table><thead><tr><th>abc</th><th>def</th></tr></thead><tbody><tr><td>bar</td><td>baz</td></tr><tr><td>bar</td><td></td></tr></tbody></table><p>bar</p>
````````````

But instead was:

````````````html
<p>| abc | def | | --- | --- | | bar | baz | bar</p><p>bar</p>
````````````

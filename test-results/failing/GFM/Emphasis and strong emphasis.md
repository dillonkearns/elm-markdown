# GFM - Emphasis and strong emphasis

## [Example 415](https://spec.commonmark.org/0.29/#example-415)

This markdown:

````````````markdown
foo***bar***baz

````````````

Should give output:

````````````html
<p>foo<em><strong>bar</strong></em>baz</p>
````````````

But instead was:

````````````html
<p>foo***bar***baz</p>
````````````
## [Example 416](https://spec.commonmark.org/0.29/#example-416)

This markdown:

````````````markdown
foo******bar*********baz

````````````

Should give output:

````````````html
<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>
````````````

But instead was:

````````````html
<p>foo******bar*********baz</p>
````````````
## [Example 463](https://spec.commonmark.org/0.29/#example-463)

This markdown:

````````````markdown
****foo****

````````````

Should give output:

````````````html
<p><strong><strong>foo</strong></strong></p>
````````````

But instead was:

````````````html
<p><strong>foo</strong></p>
````````````
## [Example 464](https://spec.commonmark.org/0.29/#example-464)

This markdown:

````````````markdown
____foo____

````````````

Should give output:

````````````html
<p><strong><strong>foo</strong></strong></p>
````````````

But instead was:

````````````html
<p><strong>foo</strong></p>
````````````
## [Example 465](https://spec.commonmark.org/0.29/#example-465)

This markdown:

````````````markdown
******foo******

````````````

Should give output:

````````````html
<p><strong><strong><strong>foo</strong></strong></strong></p>
````````````

But instead was:

````````````html
<p><strong>foo</strong></p>
````````````
## [Example 466](https://spec.commonmark.org/0.29/#example-466)

This markdown:

````````````markdown
***foo***

````````````

Should give output:

````````````html
<p><em><strong>foo</strong></em></p>
````````````

But instead was:

````````````html
<p><strong>foo</strong></p>
````````````
## [Example 467](https://spec.commonmark.org/0.29/#example-467)

This markdown:

````````````markdown
_____foo_____

````````````

Should give output:

````````````html
<p><em><strong><strong>foo</strong></strong></em></p>
````````````

But instead was:

````````````html
<p><strong>foo</strong></p>
````````````
## [Example 474](https://spec.commonmark.org/0.29/#example-474)

This markdown:

````````````markdown
*<img src="foo" title="*"/>

````````````

Should give output:

````````````html
<p>*<img src="foo" title="*"></p>
````````````

But instead was:

````````````html
<p>*<img src="foo" title="*"></img></p>
````````````
## [Example 475](https://spec.commonmark.org/0.29/#example-475)

This markdown:

````````````markdown
**<a href="**">

````````````

Should give output:

````````````html
<p>**<a href="**"></p>
````````````

But instead was:

````````````html
<p><strong>&lt;a href=&quot;</strong>&quot;&gt;</p>
````````````
## [Example 476](https://spec.commonmark.org/0.29/#example-476)

This markdown:

````````````markdown
__<a href="__">

````````````

Should give output:

````````````html
<p>__<a href="__"></p>
````````````

But instead was:

````````````html
<p><strong>&lt;a href=&quot;</strong>&quot;&gt;</p>
````````````

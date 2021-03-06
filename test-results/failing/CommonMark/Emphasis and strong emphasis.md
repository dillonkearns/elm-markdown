# CommonMark - Emphasis and strong emphasis

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

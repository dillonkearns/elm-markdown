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
<p>&lt;a href=&quot;foo<br>bar&quot;&gt;</p>
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
<p>&lt;a href=&quot;foo<br>bar&quot;&gt;</p>
````````````

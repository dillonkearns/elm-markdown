# GFM - Thematic breaks

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
<p>Foo</p><hr>
````````````
## [Example 29](https://spec.commonmark.org/0.29/#example-29)

This markdown:

````````````markdown
Foo
---
bar

````````````

Should give output:

````````````html
<h2>Foo</h2><p>bar</p>
````````````

But instead was:

````````````html
<table><thead><th>Foo</th></thead></table><p>bar</p>
````````````
## [Example 30](https://spec.commonmark.org/0.29/#example-30)

This markdown:

````````````markdown
* Foo
* * *
* Bar

````````````

Should give output:

````````````html
<ul><li>Foo</li></ul><hr><ul><li>Bar</li></ul>
````````````

But instead was:

````````````html
<ul><li>Foo</li><li>* *</li><li>Bar</li></ul>
````````````
## [Example 31](https://spec.commonmark.org/0.29/#example-31)

This markdown:

````````````markdown
- Foo
- * * *

````````````

Should give output:

````````````html
<ul><li>Foo</li><li><hr></li></ul>
````````````

But instead was:

````````````html
<ul><li>Foo</li><li>* * *</li></ul>
````````````

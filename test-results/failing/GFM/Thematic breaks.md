# GFM - Thematic breaks

## [Example 21](https://spec.commonmark.org/0.29/#example-21)

This markdown:

````````````markdown
 - - -

````````````

Should give output:

````````````html
<hr>
````````````

But instead was:

````````````html
<p>- - -</p>
````````````
## [Example 22](https://spec.commonmark.org/0.29/#example-22)

This markdown:

````````````markdown
 **  * ** * ** * **

````````````

Should give output:

````````````html
<hr>
````````````

But instead was:

````````````html
<p>** * ** * ** * **</p>
````````````
## [Example 23](https://spec.commonmark.org/0.29/#example-23)

This markdown:

````````````markdown
-     -      -      -

````````````

Should give output:

````````````html
<hr>
````````````

But instead was:

````````````html
<ul><li>- - -</li></ul>
````````````
## [Example 24](https://spec.commonmark.org/0.29/#example-24)

This markdown:

````````````markdown
- - - -    

````````````

Should give output:

````````````html
<hr>
````````````

But instead was:

````````````html
<ul><li>- - -</li></ul>
````````````
## [Example 25](https://spec.commonmark.org/0.29/#example-25)

This markdown:

````````````markdown
_ _ _ _ a

a------

---a---

````````````

Should give output:

````````````html
<p>_ _ _ _ a</p><p>a------</p><p>---a---</p>
````````````

But instead was:

````````````html
ERROR Problem at row 7 Expecting end Problem at row 7 Expecting newline
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
<p>Foo</p><hr><p>bar</p>
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

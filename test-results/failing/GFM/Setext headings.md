# GFM - Setext headings

## [Example 50](https://spec.commonmark.org/0.29/#example-50)

This markdown:

````````````markdown
Foo *bar*
=========

Foo *bar*
---------

````````````

Should give output:

````````````html
<h1>Foo<em>bar</em></h1><h2>Foo<em>bar</em></h2>
````````````

But instead was:

````````````html
<p>Foo<em>bar</em>=========</p><table><thead><th>Foo<em>bar</em></th></thead></table>
````````````
## [Example 51](https://spec.commonmark.org/0.29/#example-51)

This markdown:

````````````markdown
Foo *bar
baz*
====

````````````

Should give output:

````````````html
<h1>Foo<em>bar baz</em></h1>
````````````

But instead was:

````````````html
<p>Foo<em>bar baz</em>====</p>
````````````
## [Example 52](https://spec.commonmark.org/0.29/#example-52)

This markdown:

````````````markdown
  Foo *bar
baz*	
====

````````````

Should give output:

````````````html
<h1>Foo<em>bar baz</em></h1>
````````````

But instead was:

````````````html
<p>Foo<em>bar baz</em>====</p>
````````````
## [Example 53](https://spec.commonmark.org/0.29/#example-53)

This markdown:

````````````markdown
Foo
-------------------------

Foo
=

````````````

Should give output:

````````````html
<h2>Foo</h2><h1>Foo</h1>
````````````

But instead was:

````````````html
<table><thead><th>Foo</th></thead></table><p>Foo =</p>
````````````
## [Example 54](https://spec.commonmark.org/0.29/#example-54)

This markdown:

````````````markdown
   Foo
---

  Foo
-----

  Foo
  ===

````````````

Should give output:

````````````html
<h2>Foo</h2><h2>Foo</h2><h1>Foo</h1>
````````````

But instead was:

````````````html
<table><thead><th>Foo</th></thead></table><table><thead><th>Foo</th></thead></table><p>Foo ===</p>
````````````
## [Example 55](https://spec.commonmark.org/0.29/#example-55)

This markdown:

````````````markdown
    Foo
    ---

    Foo
---

````````````

Should give output:

````````````html
<pre><code>Foo --- Foo</code></pre><hr>
````````````

But instead was:

````````````html
<pre><code>Foo ---</code></pre><pre><code>Foo</code></pre><hr>
````````````
## [Example 56](https://spec.commonmark.org/0.29/#example-56)

This markdown:

````````````markdown
Foo
   ----      

````````````

Should give output:

````````````html
<h2>Foo</h2>
````````````

But instead was:

````````````html
<p>Foo</p><hr>
````````````
## [Example 57](https://spec.commonmark.org/0.29/#example-57)

This markdown:

````````````markdown
Foo
    ---

````````````

Should give output:

````````````html
<p>Foo ---</p>
````````````

But instead was:

````````````html
<p>Foo</p><hr>
````````````
## [Example 59](https://spec.commonmark.org/0.29/#example-59)

This markdown:

````````````markdown
Foo  
-----

````````````

Should give output:

````````````html
<h2>Foo</h2>
````````````

But instead was:

````````````html
<table><thead><th>Foo</th></thead></table>
````````````
## [Example 60](https://spec.commonmark.org/0.29/#example-60)

This markdown:

````````````markdown
Foo\
----

````````````

Should give output:

````````````html
<h2>Foo\</h2>
````````````

But instead was:

````````````html
<table><thead><th>Foo\</th></thead></table>
````````````
## [Example 61](https://spec.commonmark.org/0.29/#example-61)

This markdown:

````````````markdown
`Foo
----
`

<a title="a lot
---
of dashes"/>

````````````

Should give output:

````````````html
<h2>`Foo</h2><p>`</p><h2>&lt;a title=&quot;a lot</h2><p>of dashes&quot;/&gt;</p>
````````````

But instead was:

````````````html
<table><thead><th>`Foo</th></thead></table><p>`</p><a title="a lot
---
of dashes"></a>
````````````
## [Example 65](https://spec.commonmark.org/0.29/#example-65)

This markdown:

````````````markdown
Foo
Bar
---

````````````

Should give output:

````````````html
<h2>Foo Bar</h2>
````````````

But instead was:

````````````html
<p>Foo</p><table><thead><th>Bar</th></thead></table>
````````````
## [Example 66](https://spec.commonmark.org/0.29/#example-66)

This markdown:

````````````markdown
---
Foo
---
Bar
---
Baz

````````````

Should give output:

````````````html
<hr><h2>Foo</h2><h2>Bar</h2><p>Baz</p>
````````````

But instead was:

````````````html
<hr><table><thead><th>Foo</th></thead></table><table><thead><th>Bar</th></thead></table><p>Baz</p>
````````````
## [Example 72](https://spec.commonmark.org/0.29/#example-72)

This markdown:

````````````markdown
\> foo
------

````````````

Should give output:

````````````html
<h2>&gt; foo</h2>
````````````

But instead was:

````````````html
<table><thead><th>&gt; foo</th></thead></table>
````````````
## [Example 73](https://spec.commonmark.org/0.29/#example-73)

This markdown:

````````````markdown
Foo

bar
---
baz

````````````

Should give output:

````````````html
<p>Foo</p><h2>bar</h2><p>baz</p>
````````````

But instead was:

````````````html
<p>Foo</p><table><thead><th>bar</th></thead></table><p>baz</p>
````````````

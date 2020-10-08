# CommonMark - Setext headings

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
<h2>`Foo</h2><p>`</p><a title="a lot
---
of dashes"></a>
````````````
## [Example 63](https://spec.commonmark.org/0.29/#example-63)

This markdown:

````````````markdown
> foo
bar
===

````````````

Should give output:

````````````html
<blockquote><p>foo bar ===</p></blockquote>
````````````

But instead was:

````````````html
<blockquote><h1>foo bar</h1></blockquote>
````````````

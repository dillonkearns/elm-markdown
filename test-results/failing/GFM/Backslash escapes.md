# GFM - Backslash escapes

## [Example 14](https://spec.commonmark.org/0.30/#example-14)

This markdown:

````````````markdown
\*not emphasized*
\<br/> not a tag
\[not a link](/foo)
\`not code`
1\. not a list
\* not a list
\# not a heading
\[foo]: /url "not a reference"
\&ouml; not a character entity

````````````

Should give output:

````````````html
<p>*not emphasized* &lt;br/&gt; not a tag [not a link](/foo) `not code` 1. not a list * not a list # not a heading [foo]: /url &quot;not a reference&quot; &amp;ouml; not a character entity</p>
````````````

But instead was:

````````````html
<p>*not emphasized* &lt;br/&gt; not a tag [not a link](/foo) `not code` 1. not a list * not a list # not a heading [foo]: /url &quot;not a reference&quot; ö not a character entity</p>
````````````
## [Example 21](https://spec.commonmark.org/0.30/#example-21)

This markdown:

````````````markdown
<a href="/bar\/)">

````````````

Should give output:

````````````html
<a href="/bar\/)">
````````````

But instead was:

````````````html
ERROR Problem at row 2 Expecting symbol
````````````
## [Example 24](https://spec.commonmark.org/0.30/#example-24)

This markdown:

````````````markdown
``` foo\+bar
foo
```

````````````

Should give output:

````````````html
<pre><code class="language-foo+bar">foo</code></pre>
````````````

But instead was:

````````````html
<pre><code class="language-foo\+bar">foo</code></pre>
````````````

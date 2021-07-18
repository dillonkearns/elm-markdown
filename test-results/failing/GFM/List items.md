# GFM - List items

## [Example 241](https://spec.commonmark.org/0.29/#example-241)

This markdown:

````````````markdown
  10.  foo

           bar

````````````

Should give output:

````````````html
<ol start="10"><li><p>foo</p><pre><code>bar</code></pre></li></ol>
````````````

But instead was:

````````````html
<p>10. foo</p><pre><code>bar</code></pre>
````````````
## [Example 243](https://spec.commonmark.org/0.29/#example-243)

This markdown:

````````````markdown
1.     indented code

   paragraph

       more code

````````````

Should give output:

````````````html
<ol><li><pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>
````````````

But instead was:

````````````html
<ol><li>indented code</li></ol><p>paragraph</p><pre><code>more code</code></pre>
````````````
## [Example 244](https://spec.commonmark.org/0.29/#example-244)

This markdown:

````````````markdown
1.      indented code

   paragraph

       more code

````````````

Should give output:

````````````html
<ol><li><pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>
````````````

But instead was:

````````````html
<ol><li>indented code</li></ol><p>paragraph</p><pre><code>more code</code></pre>
````````````
## [Example 253](https://spec.commonmark.org/0.29/#example-253)

This markdown:

````````````markdown
1. foo
2.
3. bar

````````````

Should give output:

````````````html
<ol><li>foo</li><li></li><li>bar</li></ol>
````````````

But instead was:

````````````html
<ol><li>foo 2.</li><li>bar</li></ol>
````````````
## [Example 256](https://spec.commonmark.org/0.29/#example-256)

This markdown:

````````````markdown
 1.  A paragraph
     with two lines.

         indented code

     > A block quote.

````````````

Should give output:

````````````html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p><pre><code>indented code &gt; A block quote.</code></pre>
````````````
## [Example 257](https://spec.commonmark.org/0.29/#example-257)

This markdown:

````````````markdown
  1.  A paragraph
      with two lines.

          indented code

      > A block quote.

````````````

Should give output:

````````````html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p><pre><code>indented code &gt; A block quote.</code></pre>
````````````
## [Example 258](https://spec.commonmark.org/0.29/#example-258)

This markdown:

````````````markdown
   1.  A paragraph
       with two lines.

           indented code

       > A block quote.

````````````

Should give output:

````````````html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p><pre><code>indented code &gt; A block quote.</code></pre>
````````````
## [Example 260](https://spec.commonmark.org/0.29/#example-260)

This markdown:

````````````markdown
  1.  A paragraph
with two lines.

          indented code

      > A block quote.

````````````

Should give output:

````````````html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p><pre><code>indented code &gt; A block quote.</code></pre>
````````````
## [Example 261](https://spec.commonmark.org/0.29/#example-261)

This markdown:

````````````markdown
  1.  A paragraph
    with two lines.

````````````

Should give output:

````````````html
<ol><li>A paragraph with two lines.</li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p>
````````````

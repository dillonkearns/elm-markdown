# CommonMark - List items

## [Example 224](https://spec.commonmark.org/0.29/#example-224)

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
<ol><li>A paragraph</li></ol><pre><code>with two lines.</code></pre><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
````````````
## [Example 226](https://spec.commonmark.org/0.29/#example-226)

This markdown:

````````````markdown
- one

  two

````````````

Should give output:

````````````html
<ul><li><p>one</p><p>two</p></li></ul>
````````````

But instead was:

````````````html
<ul><li>one</li></ul><p>two</p>
````````````
## [Example 227](https://spec.commonmark.org/0.29/#example-227)

This markdown:

````````````markdown
 -    one

     two

````````````

Should give output:

````````````html
<ul><li>one</li></ul><pre><code>two</code></pre>
````````````

But instead was:

````````````html
<p>- one</p><pre><code>two</code></pre>
````````````
## [Example 228](https://spec.commonmark.org/0.29/#example-228)

This markdown:

````````````markdown
 -    one

      two

````````````

Should give output:

````````````html
<ul><li><p>one</p><p>two</p></li></ul>
````````````

But instead was:

````````````html
<p>- one</p><pre><code>two</code></pre>
````````````
## [Example 229](https://spec.commonmark.org/0.29/#example-229)

This markdown:

````````````markdown
   > > 1.  one
>>
>>     two

````````````

Should give output:

````````````html
<blockquote><blockquote><ol><li><p>one</p><p>two</p></li></ol></blockquote></blockquote>
````````````

But instead was:

````````````html
<blockquote><blockquote><ol><li>one</li></ol><pre><code>two</code></pre></blockquote></blockquote>
````````````
## [Example 232](https://spec.commonmark.org/0.29/#example-232)

This markdown:

````````````markdown
- foo


  bar

````````````

Should give output:

````````````html
<ul><li><p>foo</p><p>bar</p></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo</li></ul><p>bar</p>
````````````
## [Example 233](https://spec.commonmark.org/0.29/#example-233)

This markdown:

````````````markdown
1.  foo

    ```
    bar
    ```

    baz

    > bam

````````````

Should give output:

````````````html
<ol><li><p>foo</p><pre><code>bar</code></pre><p>baz</p><blockquote><p>bam</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<ol><li>foo</li></ol><pre><code>``` bar ```</code></pre><pre><code>baz</code></pre><pre><code>&gt; bam</code></pre>
````````````
## [Example 234](https://spec.commonmark.org/0.29/#example-234)

This markdown:

````````````markdown
- Foo

      bar


      baz

````````````

Should give output:

````````````html
<ul><li><p>Foo</p><pre><code>bar baz</code></pre></li></ul>
````````````

But instead was:

````````````html
<ul><li>Foo</li></ul><pre><code>bar</code></pre><pre><code>baz</code></pre>
````````````
## [Example 240](https://spec.commonmark.org/0.29/#example-240)

This markdown:

````````````markdown
- foo

      bar

````````````

Should give output:

````````````html
<ul><li><p>foo</p><pre><code>bar</code></pre></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo</li></ul><pre><code>bar</code></pre>
````````````
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
## [Example 247](https://spec.commonmark.org/0.29/#example-247)

This markdown:

````````````markdown
-  foo

   bar

````````````

Should give output:

````````````html
<ul><li><p>foo</p><p>bar</p></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo</li></ul><p>bar</p>
````````````
## [Example 248](https://spec.commonmark.org/0.29/#example-248)

This markdown:

````````````markdown
-
  foo
-
  ```
  bar
  ```
-
      baz

````````````

Should give output:

````````````html
<ul><li>foo</li><li><pre><code>bar</code></pre></li><li><pre><code>baz</code></pre></li></ul>
````````````

But instead was:

````````````html
<h2>- foo</h2><pre><code>bar</code></pre><p>- baz</p>
````````````
## [Example 249](https://spec.commonmark.org/0.29/#example-249)

This markdown:

````````````markdown
-   
  foo

````````````

Should give output:

````````````html
<ul><li>foo</li></ul>
````````````

But instead was:

````````````html
<ul><li></li></ul><p>foo</p>
````````````
## [Example 250](https://spec.commonmark.org/0.29/#example-250)

This markdown:

````````````markdown
-

  foo

````````````

Should give output:

````````````html
<ul><li></li></ul><p>foo</p>
````````````

But instead was:

````````````html
<p>-</p><p>foo</p>
````````````
## [Example 254](https://spec.commonmark.org/0.29/#example-254)

This markdown:

````````````markdown
*

````````````

Should give output:

````````````html
<ul><li></li></ul>
````````````

But instead was:

````````````html
<p>*</p>
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
<p>1. A paragraph with two lines.</p><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
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
<p>1. A paragraph with two lines.</p><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
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
<p>1. A paragraph with two lines.</p><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
````````````
## [Example 259](https://spec.commonmark.org/0.29/#example-259)

This markdown:

````````````markdown
    1.  A paragraph
        with two lines.

            indented code

        > A block quote.

````````````

Should give output:

````````````html
<pre><code>1. A paragraph with two lines. indented code &gt; A block quote.</code></pre>
````````````

But instead was:

````````````html
<pre><code>1. A paragraph with two lines.</code></pre><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
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
<p>1. A paragraph with two lines.</p><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
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
## [Example 262](https://spec.commonmark.org/0.29/#example-262)

This markdown:

````````````markdown
> 1. > Blockquote
continued here.

````````````

Should give output:

````````````html
<blockquote><ol><li><blockquote><p>Blockquote continued here.</p></blockquote></li></ol></blockquote>
````````````

But instead was:

````````````html
<blockquote><ol><li>&gt; Blockquote</li></ol><p>continued here.</p></blockquote>
````````````
## [Example 263](https://spec.commonmark.org/0.29/#example-263)

This markdown:

````````````markdown
> 1. > Blockquote
> continued here.

````````````

Should give output:

````````````html
<blockquote><ol><li><blockquote><p>Blockquote continued here.</p></blockquote></li></ol></blockquote>
````````````

But instead was:

````````````html
<blockquote><ol><li>&gt; Blockquote</li></ol><p>continued here.</p></blockquote>
````````````
## [Example 264](https://spec.commonmark.org/0.29/#example-264)

This markdown:

````````````markdown
- foo
  - bar
    - baz
      - boo

````````````

Should give output:

````````````html
<ul><li>foo<ul><li>bar<ul><li>baz<ul><li>boo</li></ul></li></ul></li></ul></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo</li></ul><p>- bar - baz - boo</p>
````````````
## [Example 265](https://spec.commonmark.org/0.29/#example-265)

This markdown:

````````````markdown
- foo
 - bar
  - baz
   - boo

````````````

Should give output:

````````````html
<ul><li>foo</li><li>bar</li><li>baz</li><li>boo</li></ul>
````````````

But instead was:

````````````html
<ul><li>foo</li></ul><p>- bar - baz - boo</p>
````````````
## [Example 266](https://spec.commonmark.org/0.29/#example-266)

This markdown:

````````````markdown
10) foo
    - bar

````````````

Should give output:

````````````html
<ol start="10"><li>foo<ul><li>bar</li></ul></li></ol>
````````````

But instead was:

````````````html
<ol start="10"><li>foo</li></ol><pre><code>- bar</code></pre>
````````````
## [Example 267](https://spec.commonmark.org/0.29/#example-267)

This markdown:

````````````markdown
10) foo
   - bar

````````````

Should give output:

````````````html
<ol start="10"><li>foo</li></ol><ul><li>bar</li></ul>
````````````

But instead was:

````````````html
<ol start="10"><li>foo</li></ol><p>- bar</p>
````````````
## [Example 268](https://spec.commonmark.org/0.29/#example-268)

This markdown:

````````````markdown
- - foo

````````````

Should give output:

````````````html
<ul><li><ul><li>foo</li></ul></li></ul>
````````````

But instead was:

````````````html
<ul><li>- foo</li></ul>
````````````
## [Example 269](https://spec.commonmark.org/0.29/#example-269)

This markdown:

````````````markdown
1. - 2. foo

````````````

Should give output:

````````````html
<ol><li><ul><li><ol start="2"><li>foo</li></ol></li></ul></li></ol>
````````````

But instead was:

````````````html
<ol><li>- 2. foo</li></ol>
````````````
## [Example 270](https://spec.commonmark.org/0.29/#example-270)

This markdown:

````````````markdown
- # Foo
- Bar
  ---
  baz

````````````

Should give output:

````````````html
<ul><li><h1>Foo</h1></li><li><h2>Bar</h2>baz</li></ul>
````````````

But instead was:

````````````html
<ul><li># Foo</li><li>Bar</li></ul><hr><p>baz</p>
````````````

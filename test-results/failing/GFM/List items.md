# GFM - List items

## [Example 223](https://github.github.com/gfm/#example-223)

This markdown:

```markdown
A paragraph
with two lines.

    indented code

> A block quote.

```

Should give output:

```html
<p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote>
```

But instead was:

```html
<p>A paragraph with two lines.</p><pre><code>indented code</code></pre><p>&gt; A block quote.</p>
```
## [Example 224](https://github.github.com/gfm/#example-224)

This markdown:

```markdown
1.  A paragraph
    with two lines.

        indented code

    > A block quote.

```

Should give output:

```html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
```

But instead was:

```html
<ol><li>A paragraph</li></ol><pre><code>with two lines.</code></pre><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
```
## [Example 225](https://github.github.com/gfm/#example-225)

This markdown:

```markdown
- one

 two

```

Should give output:

```html
<ul><li>one</li></ul><p>two</p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
## [Example 226](https://github.github.com/gfm/#example-226)

This markdown:

```markdown
- one

  two

```

Should give output:

```html
<ul><li><p>one</p><p>two</p></li></ul>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
## [Example 227](https://github.github.com/gfm/#example-227)

This markdown:

```markdown
 -    one

     two

```

Should give output:

```html
<ul><li>one</li></ul><pre><code>two</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 228](https://github.github.com/gfm/#example-228)

This markdown:

```markdown
 -    one

      two

```

Should give output:

```html
<ul><li><p>one</p><p>two</p></li></ul>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 229](https://github.github.com/gfm/#example-229)

This markdown:

```markdown
   > > 1.  one
>>
>>     two

```

Should give output:

```html
<blockquote><blockquote><ol><li><p>one</p><p>two</p></li></ol></blockquote></blockquote>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 230](https://github.github.com/gfm/#example-230)

This markdown:

```markdown
>>- one
>>
  >  > two

```

Should give output:

```html
<blockquote><blockquote><ul><li>one</li></ul><p>two</p></blockquote></blockquote>
```

But instead was:

```html
ERROR Problem at row 5 Expecting --- Problem at row 5 Expecting *** Problem at row 5 Expecting ___
```
## [Example 232](https://github.github.com/gfm/#example-232)

This markdown:

```markdown
- foo


  bar

```

Should give output:

```html
<ul><li><p>foo</p><p>bar</p></li></ul>
```

But instead was:

```html
ERROR Problem at row 5 Expecting --- Problem at row 5 Expecting *** Problem at row 5 Expecting ___
```
## [Example 233](https://github.github.com/gfm/#example-233)

This markdown:

```markdown
1.  foo

    ```
    bar
    ```

    baz

    > bam

```

Should give output:

```html
<ol><li><p>foo</p><pre><code>bar</code></pre><p>baz</p><blockquote><p>bam</p></blockquote></li></ol>
```

But instead was:

```html
<ol><li>foo</li></ol><pre><code>```</code></pre><pre><code>bar</code></pre><pre><code>```</code></pre><pre><code>baz</code></pre><pre><code>&gt; bam</code></pre>
```
## [Example 234](https://github.github.com/gfm/#example-234)

This markdown:

```markdown
- Foo

      bar


      baz

```

Should give output:

```html
<ul><li><p>Foo</p><pre><code>bar baz</code></pre></li></ul>
```

But instead was:

```html
<ul><li>Foo</li></ul><pre><code>bar</code></pre><pre><code>baz</code></pre>
```
## [Example 240](https://github.github.com/gfm/#example-240)

This markdown:

```markdown
- foo

      bar

```

Should give output:

```html
<ul><li><p>foo</p><pre><code>bar</code></pre></li></ul>
```

But instead was:

```html
<ul><li>foo</li></ul><pre><code>bar</code></pre>
```
## [Example 241](https://github.github.com/gfm/#example-241)

This markdown:

```markdown
  10.  foo

           bar

```

Should give output:

```html
<ol start="10"><li><p>foo</p><pre><code>bar</code></pre></li></ol>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 243](https://github.github.com/gfm/#example-243)

This markdown:

```markdown
1.     indented code

   paragraph

       more code

```

Should give output:

```html
<ol><li><pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
## [Example 244](https://github.github.com/gfm/#example-244)

This markdown:

```markdown
1.      indented code

   paragraph

       more code

```

Should give output:

```html
<ol><li><pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
## [Example 245](https://github.github.com/gfm/#example-245)

This markdown:

```markdown
   foo

bar

```

Should give output:

```html
<p>foo</p><p>bar</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 246](https://github.github.com/gfm/#example-246)

This markdown:

```markdown
-    foo

  bar

```

Should give output:

```html
<ul><li>foo</li></ul><p>bar</p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
## [Example 247](https://github.github.com/gfm/#example-247)

This markdown:

```markdown
-  foo

   bar

```

Should give output:

```html
<ul><li><p>foo</p><p>bar</p></li></ul>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
## [Example 248](https://github.github.com/gfm/#example-248)

This markdown:

```markdown
-
  foo
-
  ```
  bar
  ```
-
      baz

```

Should give output:

```html
<ul><li>foo</li><li><pre><code>bar</code></pre></li><li><pre><code>baz</code></pre></li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
## [Example 249](https://github.github.com/gfm/#example-249)

This markdown:

```markdown
-   
  foo

```

Should give output:

```html
<ul><li>foo</li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
## [Example 250](https://github.github.com/gfm/#example-250)

This markdown:

```markdown
-

  foo

```

Should give output:

```html
<ul><li></li></ul><p>foo</p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
## [Example 254](https://github.github.com/gfm/#example-254)

This markdown:

```markdown
*

```

Should give output:

```html
<ul><li></li></ul>
```

But instead was:

```html

```
## [Example 255](https://github.github.com/gfm/#example-255)

This markdown:

```markdown
foo
*

foo
1.

```

Should give output:

```html
<p>foo *</p><p>foo 1.</p>
```

But instead was:

```html
<p>foo</p><p>foo 1.</p>
```
## [Example 256](https://github.github.com/gfm/#example-256)

This markdown:

```markdown
 1.  A paragraph
     with two lines.

         indented code

     > A block quote.

```

Should give output:

```html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 257](https://github.github.com/gfm/#example-257)

This markdown:

```markdown
  1.  A paragraph
      with two lines.

          indented code

      > A block quote.

```

Should give output:

```html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 258](https://github.github.com/gfm/#example-258)

This markdown:

```markdown
   1.  A paragraph
       with two lines.

           indented code

       > A block quote.

```

Should give output:

```html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 259](https://github.github.com/gfm/#example-259)

This markdown:

```markdown
    1.  A paragraph
        with two lines.

            indented code

        > A block quote.

```

Should give output:

```html
<pre><code>1. A paragraph with two lines. indented code &gt; A block quote.</code></pre>
```

But instead was:

```html
<pre><code>1. A paragraph</code></pre><pre><code>with two lines.</code></pre><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
```
## [Example 260](https://github.github.com/gfm/#example-260)

This markdown:

```markdown
  1.  A paragraph
with two lines.

          indented code

      > A block quote.

```

Should give output:

```html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 261](https://github.github.com/gfm/#example-261)

This markdown:

```markdown
  1.  A paragraph
    with two lines.

```

Should give output:

```html
<ol><li>A paragraph with two lines.</li></ol>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 262](https://github.github.com/gfm/#example-262)

This markdown:

```markdown
> 1. > Blockquote
continued here.

```

Should give output:

```html
<blockquote><ol><li><blockquote><p>Blockquote continued here.</p></blockquote></li></ol></blockquote>
```

But instead was:

```html
<p>&gt; 1. &gt; Blockquote continued here.</p>
```
## [Example 263](https://github.github.com/gfm/#example-263)

This markdown:

```markdown
> 1. > Blockquote
> continued here.

```

Should give output:

```html
<blockquote><ol><li><blockquote><p>Blockquote continued here.</p></blockquote></li></ol></blockquote>
```

But instead was:

```html
<p>&gt; 1. &gt; Blockquote &gt; continued here.</p>
```
## [Example 264](https://github.github.com/gfm/#example-264)

This markdown:

```markdown
- foo
  - bar
    - baz
      - boo

```

Should give output:

```html
<ul><li>foo<ul><li>bar<ul><li>baz<ul><li>boo</li></ul></li></ul></li></ul></li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
## [Example 265](https://github.github.com/gfm/#example-265)

This markdown:

```markdown
- foo
 - bar
  - baz
   - boo

```

Should give output:

```html
<ul><li>foo</li><li>bar</li><li>baz</li><li>boo</li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
## [Example 266](https://github.github.com/gfm/#example-266)

This markdown:

```markdown
10) foo
    - bar

```

Should give output:

```html
<ol start="10"><li>foo<ul><li>bar</li></ul></li></ol>
```

But instead was:

```html
<ol start="10"><li>foo</li></ol><pre><code>- bar</code></pre>
```
## [Example 267](https://github.github.com/gfm/#example-267)

This markdown:

```markdown
10) foo
   - bar

```

Should give output:

```html
<ol start="10"><li>foo</li></ol><ul><li>bar</li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
## [Example 268](https://github.github.com/gfm/#example-268)

This markdown:

```markdown
- - foo

```

Should give output:

```html
<ul><li><ul><li>foo</li></ul></li></ul>
```

But instead was:

```html
<ul><li>- foo</li></ul>
```
## [Example 269](https://github.github.com/gfm/#example-269)

This markdown:

```markdown
1. - 2. foo

```

Should give output:

```html
<ol><li><ul><li><ol start="2"><li>foo</li></ol></li></ul></li></ol>
```

But instead was:

```html
<ol><li>- 2. foo</li></ol>
```
## [Example 270](https://github.github.com/gfm/#example-270)

This markdown:

```markdown
- # Foo
- Bar
  ---
  baz

```

Should give output:

```html
<ul><li><h1>Foo</h1></li><li><h2>Bar</h2>baz</li></ul>
```

But instead was:

```html
ERROR Problem at row 6 Expecting --- Problem at row 6 Expecting *** Problem at row 6 Expecting ___
```

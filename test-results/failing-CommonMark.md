# CommonMark

## List items

### [Example 223](https://spec.commonmark.org/0.29/#example-223)

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
### [Example 224](https://spec.commonmark.org/0.29/#example-224)

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
<p>1. A paragraph</p><pre><code>with two lines.</code></pre><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
```
### [Example 225](https://spec.commonmark.org/0.29/#example-225)

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
### [Example 226](https://spec.commonmark.org/0.29/#example-226)

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
### [Example 227](https://spec.commonmark.org/0.29/#example-227)

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
### [Example 228](https://spec.commonmark.org/0.29/#example-228)

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
### [Example 229](https://spec.commonmark.org/0.29/#example-229)

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
### [Example 230](https://spec.commonmark.org/0.29/#example-230)

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
### [Example 231](https://spec.commonmark.org/0.29/#example-231)

This markdown:

```markdown
-one

2.two

```

Should give output:

```html
<p>-one</p><p>2.two</p>
```

But instead was:

```html
<ul><li><p>one</p></li></ul><p>2.two</p>
```
### [Example 232](https://spec.commonmark.org/0.29/#example-232)

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
### [Example 233](https://spec.commonmark.org/0.29/#example-233)

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
<p>1. foo</p><pre><code>```</code></pre><pre><code>bar</code></pre><pre><code>```</code></pre><pre><code>baz</code></pre><pre><code>&gt; bam</code></pre>
```
### [Example 234](https://spec.commonmark.org/0.29/#example-234)

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
<ul><li><p>Foo</p></li></ul><pre><code>bar</code></pre><pre><code>baz</code></pre>
```
### [Example 235](https://spec.commonmark.org/0.29/#example-235)

This markdown:

```markdown
123456789. ok

```

Should give output:

```html
<ol start="123456789"><li>ok</li></ol>
```

But instead was:

```html
<p>123456789. ok</p>
```
### [Example 237](https://spec.commonmark.org/0.29/#example-237)

This markdown:

```markdown
0. ok

```

Should give output:

```html
<ol start="0"><li>ok</li></ol>
```

But instead was:

```html
<p>0. ok</p>
```
### [Example 238](https://spec.commonmark.org/0.29/#example-238)

This markdown:

```markdown
003. ok

```

Should give output:

```html
<ol start="3"><li>ok</li></ol>
```

But instead was:

```html
<p>003. ok</p>
```
### [Example 239](https://spec.commonmark.org/0.29/#example-239)

This markdown:

```markdown
-1. not ok

```

Should give output:

```html
<p>-1. not ok</p>
```

But instead was:

```html
<ul><li><p>1. not ok</p></li></ul>
```
### [Example 240](https://spec.commonmark.org/0.29/#example-240)

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
<ul><li><p>foo</p></li></ul><pre><code>bar</code></pre>
```
### [Example 241](https://spec.commonmark.org/0.29/#example-241)

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
### [Example 243](https://spec.commonmark.org/0.29/#example-243)

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
### [Example 244](https://spec.commonmark.org/0.29/#example-244)

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
### [Example 245](https://spec.commonmark.org/0.29/#example-245)

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
### [Example 246](https://spec.commonmark.org/0.29/#example-246)

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
### [Example 247](https://spec.commonmark.org/0.29/#example-247)

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
### [Example 248](https://spec.commonmark.org/0.29/#example-248)

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
### [Example 249](https://spec.commonmark.org/0.29/#example-249)

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
### [Example 250](https://spec.commonmark.org/0.29/#example-250)

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
### [Example 251](https://spec.commonmark.org/0.29/#example-251)

This markdown:

```markdown
- foo
-
- bar

```

Should give output:

```html
<ul><li>foo</li><li></li><li>bar</li></ul>
```

But instead was:

```html
<ul><li><p>foo</p></li><li><p></p></li><li><p>bar</p></li></ul>
```
### [Example 252](https://spec.commonmark.org/0.29/#example-252)

This markdown:

```markdown
- foo
-   
- bar

```

Should give output:

```html
<ul><li>foo</li><li></li><li>bar</li></ul>
```

But instead was:

```html
<ul><li><p>foo</p></li><li><p></p></li><li><p>bar</p></li></ul>
```
### [Example 253](https://spec.commonmark.org/0.29/#example-253)

This markdown:

```markdown
1. foo
2.
3. bar

```

Should give output:

```html
<ol><li>foo</li><li></li><li>bar</li></ol>
```

But instead was:

```html
<p>1. foo 2. 3. bar</p>
```
### [Example 254](https://spec.commonmark.org/0.29/#example-254)

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
### [Example 255](https://spec.commonmark.org/0.29/#example-255)

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
### [Example 256](https://spec.commonmark.org/0.29/#example-256)

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
### [Example 257](https://spec.commonmark.org/0.29/#example-257)

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
### [Example 258](https://spec.commonmark.org/0.29/#example-258)

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
### [Example 259](https://spec.commonmark.org/0.29/#example-259)

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
### [Example 260](https://spec.commonmark.org/0.29/#example-260)

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
### [Example 261](https://spec.commonmark.org/0.29/#example-261)

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
### [Example 262](https://spec.commonmark.org/0.29/#example-262)

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
### [Example 263](https://spec.commonmark.org/0.29/#example-263)

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
### [Example 264](https://spec.commonmark.org/0.29/#example-264)

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
### [Example 265](https://spec.commonmark.org/0.29/#example-265)

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
### [Example 266](https://spec.commonmark.org/0.29/#example-266)

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
<p>10) foo</p><pre><code>- bar</code></pre>
```
### [Example 267](https://spec.commonmark.org/0.29/#example-267)

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
### [Example 268](https://spec.commonmark.org/0.29/#example-268)

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
<ul><li><p>- foo</p></li></ul>
```
### [Example 269](https://spec.commonmark.org/0.29/#example-269)

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
<p>1. - 2. foo</p>
```
### [Example 270](https://spec.commonmark.org/0.29/#example-270)

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
## Precedence

### [Example 12](https://spec.commonmark.org/0.29/#example-12)

This markdown:

```markdown
- `one
- two`

```

Should give output:

```html
<ul><li>`one</li><li>two`</li></ul>
```

But instead was:

```html
<ul><li><p><code>one</code></p></li><li><p>two</p></li></ul>
```
## Lists

### [Example 271](https://spec.commonmark.org/0.29/#example-271)

This markdown:

```markdown
- foo
- bar
+ baz

```

Should give output:

```html
<ul><li>foo</li><li>bar</li></ul><ul><li>baz</li></ul>
```

But instead was:

```html
<ul><li><p>foo</p></li><li><p>bar</p></li></ul><p>+ baz</p>
```
### [Example 272](https://spec.commonmark.org/0.29/#example-272)

This markdown:

```markdown
1. foo
2. bar
3) baz

```

Should give output:

```html
<ol><li>foo</li><li>bar</li></ol><ol start="3"><li>baz</li></ol>
```

But instead was:

```html
<p>1. foo 2. bar 3) baz</p>
```
### [Example 273](https://spec.commonmark.org/0.29/#example-273)

This markdown:

```markdown
Foo
- bar
- baz

```

Should give output:

```html
<p>Foo</p><ul><li>bar</li><li>baz</li></ul>
```

But instead was:

```html
<p>Foo</p><ul><li><p>bar</p></li><li><p>baz</p></li></ul>
```
### [Example 275](https://spec.commonmark.org/0.29/#example-275)

This markdown:

```markdown
The number of windows in my house is
1.  The number of doors is 6.

```

Should give output:

```html
<p>The number of windows in my house is</p><ol><li>The number of doors is 6.</li></ol>
```

But instead was:

```html
<p>The number of windows in my house is 1. The number of doors is 6.</p>
```
### [Example 276](https://spec.commonmark.org/0.29/#example-276)

This markdown:

```markdown
- foo

- bar


- baz

```

Should give output:

```html
<ul><li><p>foo</p></li><li><p>bar</p></li><li><p>baz</p></li></ul>
```

But instead was:

```html
<ul><li><p>foo</p></li></ul><ul><li><p>bar</p></li></ul><ul><li><p>baz</p></li></ul>
```
### [Example 277](https://spec.commonmark.org/0.29/#example-277)

This markdown:

```markdown
- foo
  - bar
    - baz


      bim

```

Should give output:

```html
<ul><li>foo<ul><li>bar<ul><li><p>baz</p><p>bim</p></li></ul></li></ul></li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 278](https://spec.commonmark.org/0.29/#example-278)

This markdown:

```markdown
- foo
- bar

<!-- -->

- baz
- bim

```

Should give output:

```html
<ul><li>foo</li><li>bar</li></ul><ul><li>baz</li><li>bim</li></ul>
```

But instead was:

```html
ERROR Problem at row 6 Expecting symbol =
```
### [Example 279](https://spec.commonmark.org/0.29/#example-279)

This markdown:

```markdown
-   foo

    notcode

-   foo

<!-- -->

    code

```

Should give output:

```html
<ul><li><p>foo</p><p>notcode</p></li><li><p>foo</p></li></ul><pre><code>code</code></pre>
```

But instead was:

```html
ERROR Problem at row 10 Expecting symbol =
```
### [Example 280](https://spec.commonmark.org/0.29/#example-280)

This markdown:

```markdown
- a
 - b
  - c
   - d
  - e
 - f
- g

```

Should give output:

```html
<ul><li>a</li><li>b</li><li>c</li><li>d</li><li>e</li><li>f</li><li>g</li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 281](https://spec.commonmark.org/0.29/#example-281)

This markdown:

```markdown
1. a

  2. b

   3. c

```

Should give output:

```html
<ol><li><p>a</p></li><li><p>b</p></li><li><p>c</p></li></ol>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
### [Example 282](https://spec.commonmark.org/0.29/#example-282)

This markdown:

```markdown
- a
 - b
  - c
   - d
    - e

```

Should give output:

```html
<ul><li>a</li><li>b</li><li>c</li><li>d - e</li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 283](https://spec.commonmark.org/0.29/#example-283)

This markdown:

```markdown
1. a

  2. b

    3. c

```

Should give output:

```html
<ol><li><p>a</p></li><li><p>b</p></li></ol><pre><code>3. c</code></pre>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
### [Example 284](https://spec.commonmark.org/0.29/#example-284)

This markdown:

```markdown
- a
- b

- c

```

Should give output:

```html
<ul><li><p>a</p></li><li><p>b</p></li><li><p>c</p></li></ul>
```

But instead was:

```html
<ul><li><p>a</p></li><li><p>b</p></li></ul><ul><li><p>c</p></li></ul>
```
### [Example 285](https://spec.commonmark.org/0.29/#example-285)

This markdown:

```markdown
* a
*

* c

```

Should give output:

```html
<ul><li><p>a</p></li><li></li><li><p>c</p></li></ul>
```

But instead was:

```html
<p><em>a</em></p><p><em>c</em></p>
```
### [Example 286](https://spec.commonmark.org/0.29/#example-286)

This markdown:

```markdown
- a
- b

  c
- d

```

Should give output:

```html
<ul><li><p>a</p></li><li><p>b</p><p>c</p></li><li><p>d</p></li></ul>
```

But instead was:

```html
ERROR Problem at row 6 Expecting --- Problem at row 6 Expecting *** Problem at row 6 Expecting ___
```
### [Example 287](https://spec.commonmark.org/0.29/#example-287)

This markdown:

```markdown
- a
- b

  [ref]: /url
- d

```

Should give output:

```html
<ul><li><p>a</p></li><li><p>b</p></li><li><p>d</p></li></ul>
```

But instead was:

```html
ERROR Problem at row 6 Expecting --- Problem at row 6 Expecting *** Problem at row 6 Expecting ___
```
### [Example 288](https://spec.commonmark.org/0.29/#example-288)

This markdown:

```markdown
- a
- ```
  b


  ```
- c

```

Should give output:

```html
<ul><li>a</li><li><pre><code>b</code></pre></li><li>c</li></ul>
```

But instead was:

```html
ERROR Problem at row 5 Expecting --- Problem at row 5 Expecting *** Problem at row 5 Expecting ___
```
### [Example 289](https://spec.commonmark.org/0.29/#example-289)

This markdown:

```markdown
- a
  - b

    c
- d

```

Should give output:

```html
<ul><li>a<ul><li><p>b</p><p>c</p></li></ul></li><li>d</li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 290](https://spec.commonmark.org/0.29/#example-290)

This markdown:

```markdown
* a
  > b
  >
* c

```

Should give output:

```html
<ul><li>a<blockquote><p>b</p></blockquote></li><li>c</li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 291](https://spec.commonmark.org/0.29/#example-291)

This markdown:

```markdown
- a
  > b
  ```
  c
  ```
- d

```

Should give output:

```html
<ul><li>a<blockquote><p>b</p></blockquote><pre><code>c</code></pre></li><li>d</li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 292](https://spec.commonmark.org/0.29/#example-292)

This markdown:

```markdown
- a

```

Should give output:

```html
<ul><li>a</li></ul>
```

But instead was:

```html
<ul><li><p>a</p></li></ul>
```
### [Example 293](https://spec.commonmark.org/0.29/#example-293)

This markdown:

```markdown
- a
  - b

```

Should give output:

```html
<ul><li>a<ul><li>b</li></ul></li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 294](https://spec.commonmark.org/0.29/#example-294)

This markdown:

```markdown
1. ```
   foo
   ```

   bar

```

Should give output:

```html
<ol><li><pre><code>foo</code></pre><p>bar</p></li></ol>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 295](https://spec.commonmark.org/0.29/#example-295)

This markdown:

```markdown
* foo
  * bar

  baz

```

Should give output:

```html
<ul><li><p>foo</p><ul><li>bar</li></ul><p>baz</p></li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 296](https://spec.commonmark.org/0.29/#example-296)

This markdown:

```markdown
- a
  - b
  - c

- d
  - e
  - f

```

Should give output:

```html
<ul><li><p>a</p><ul><li>b</li><li>c</li></ul></li><li><p>d</p><ul><li>e</li><li>f</li></ul></li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
## Tabs

### [Example 2](https://spec.commonmark.org/0.29/#example-2)

This markdown:

```markdown
  	foo	baz		bim

```

Should give output:

```html
<pre><code>foo baz bim</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 3](https://spec.commonmark.org/0.29/#example-3)

This markdown:

```markdown
    a	a
    ὐ	a

```

Should give output:

```html
<pre><code>a a ὐ a</code></pre>
```

But instead was:

```html
<pre><code>a a</code></pre><pre><code>ὐ a</code></pre>
```
### [Example 4](https://spec.commonmark.org/0.29/#example-4)

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
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 5](https://spec.commonmark.org/0.29/#example-5)

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
<ul><li><p>foo</p></li></ul><pre><code>bar</code></pre>
```
### [Example 6](https://spec.commonmark.org/0.29/#example-6)

This markdown:

```markdown
>		foo

```

Should give output:

```html
<blockquote><pre><code>foo</code></pre></blockquote>
```

But instead was:

```html
<p>&gt; foo</p>
```
### [Example 7](https://spec.commonmark.org/0.29/#example-7)

This markdown:

```markdown
-		foo

```

Should give output:

```html
<ul><li><pre><code>foo</code></pre></li></ul>
```

But instead was:

```html
<ul><li><p>foo</p></li></ul>
```
### [Example 8](https://spec.commonmark.org/0.29/#example-8)

This markdown:

```markdown
    foo
	bar

```

Should give output:

```html
<pre><code>foo bar</code></pre>
```

But instead was:

```html
<pre><code>foo</code></pre><pre><code>bar</code></pre>
```
### [Example 9](https://spec.commonmark.org/0.29/#example-9)

This markdown:

```markdown
 - foo
   - bar
	 - baz

```

Should give output:

```html
<ul><li>foo<ul><li>bar<ul><li>baz</li></ul></li></ul></li></ul>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 11](https://spec.commonmark.org/0.29/#example-11)

This markdown:

```markdown
*	*	*	

```

Should give output:

```html
<hr>
```

But instead was:

```html
<p><em></em><em></em></p>
```
## Soft line breaks

### [Example 646](https://spec.commonmark.org/0.29/#example-646)

This markdown:

```markdown
foo 
 baz

```

Should give output:

```html
<p>foo baz</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
## ATX headings

### [Example 33](https://spec.commonmark.org/0.29/#example-33)

This markdown:

```markdown
####### foo

```

Should give output:

```html
<p>####### foo</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting heading with &lt; 7 #&#39;s
```
### [Example 34](https://spec.commonmark.org/0.29/#example-34)

This markdown:

```markdown
#5 bolt

#hashtag

```

Should give output:

```html
<p>#5 bolt</p><p>#hashtag</p>
```

But instead was:

```html
<h1>5 bolt</h1><h1>hashtag</h1>
```
### [Example 35](https://spec.commonmark.org/0.29/#example-35)

This markdown:

```markdown
\## foo

```

Should give output:

```html
<p>## foo</p>
```

But instead was:

```html
<p>\## foo</p>
```
### [Example 36](https://spec.commonmark.org/0.29/#example-36)

This markdown:

```markdown
# foo *bar* \*baz\*

```

Should give output:

```html
<h1>foo<em>bar</em>*baz*</h1>
```

But instead was:

```html
<h1>foo<em>bar</em>\<em>baz\</em></h1>
```
### [Example 38](https://spec.commonmark.org/0.29/#example-38)

This markdown:

```markdown
 ### foo
  ## foo
   # foo

```

Should give output:

```html
<h3>foo</h3><h2>foo</h2><h1>foo</h1>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 40](https://spec.commonmark.org/0.29/#example-40)

This markdown:

```markdown
foo
    # bar

```

Should give output:

```html
<p>foo # bar</p>
```

But instead was:

```html
<p>foo</p><pre><code># bar</code></pre>
```
### [Example 41](https://spec.commonmark.org/0.29/#example-41)

This markdown:

```markdown
## foo ##
  ###   bar    ###

```

Should give output:

```html
<h2>foo</h2><h3>bar</h3>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 43](https://spec.commonmark.org/0.29/#example-43)

This markdown:

```markdown
### foo ###     

```

Should give output:

```html
<h3>foo</h3>
```

But instead was:

```html
<h3>foo ###</h3>
```
### [Example 45](https://spec.commonmark.org/0.29/#example-45)

This markdown:

```markdown
# foo#

```

Should give output:

```html
<h1>foo#</h1>
```

But instead was:

```html
<h1>foo</h1>
```
### [Example 46](https://spec.commonmark.org/0.29/#example-46)

This markdown:

```markdown
### foo \###
## foo #\##
# foo \#

```

Should give output:

```html
<h3>foo ###</h3><h2>foo ###</h2><h1>foo #</h1>
```

But instead was:

```html
<h3>foo \</h3><h2>foo #\</h2><h1>foo \</h1>
```
## Blank lines

### [Example 197](https://spec.commonmark.org/0.29/#example-197)

This markdown:

```markdown
  

aaa
  

# aaa

  

```

Should give output:

```html
<p>aaa</p><h1>aaa</h1>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## Block quotes

### [Example 198](https://spec.commonmark.org/0.29/#example-198)

This markdown:

```markdown
> # Foo
> bar
> baz

```

Should give output:

```html
<blockquote><h1>Foo</h1><p>bar baz</p></blockquote>
```

But instead was:

```html
<p>&gt; # Foo &gt; bar &gt; baz</p>
```
### [Example 199](https://spec.commonmark.org/0.29/#example-199)

This markdown:

```markdown
># Foo
>bar
> baz

```

Should give output:

```html
<blockquote><h1>Foo</h1><p>bar baz</p></blockquote>
```

But instead was:

```html
<p>&gt;# Foo &gt;bar &gt; baz</p>
```
### [Example 200](https://spec.commonmark.org/0.29/#example-200)

This markdown:

```markdown
   > # Foo
   > bar
 > baz

```

Should give output:

```html
<blockquote><h1>Foo</h1><p>bar baz</p></blockquote>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 201](https://spec.commonmark.org/0.29/#example-201)

This markdown:

```markdown
    > # Foo
    > bar
    > baz

```

Should give output:

```html
<pre><code>&gt; # Foo &gt; bar &gt; baz</code></pre>
```

But instead was:

```html
<pre><code>&gt; # Foo</code></pre><pre><code>&gt; bar</code></pre><pre><code>&gt; baz</code></pre>
```
### [Example 202](https://spec.commonmark.org/0.29/#example-202)

This markdown:

```markdown
> # Foo
> bar
baz

```

Should give output:

```html
<blockquote><h1>Foo</h1><p>bar baz</p></blockquote>
```

But instead was:

```html
<p>&gt; # Foo &gt; bar baz</p>
```
### [Example 203](https://spec.commonmark.org/0.29/#example-203)

This markdown:

```markdown
> bar
baz
> foo

```

Should give output:

```html
<blockquote><p>bar baz foo</p></blockquote>
```

But instead was:

```html
<p>&gt; bar baz &gt; foo</p>
```
### [Example 204](https://spec.commonmark.org/0.29/#example-204)

This markdown:

```markdown
> foo
---

```

Should give output:

```html
<blockquote><p>foo</p></blockquote><hr>
```

But instead was:

```html
<p>&gt; foo</p><hr>
```
### [Example 205](https://spec.commonmark.org/0.29/#example-205)

This markdown:

```markdown
> - foo
- bar

```

Should give output:

```html
<blockquote><ul><li>foo</li></ul></blockquote><ul><li>bar</li></ul>
```

But instead was:

```html
<p>&gt; - foo</p><ul><li><p>bar</p></li></ul>
```
### [Example 206](https://spec.commonmark.org/0.29/#example-206)

This markdown:

```markdown
>     foo
    bar

```

Should give output:

```html
<blockquote><pre><code>foo</code></pre></blockquote><pre><code>bar</code></pre>
```

But instead was:

```html
<p>&gt; foo</p><pre><code>bar</code></pre>
```
### [Example 207](https://spec.commonmark.org/0.29/#example-207)

This markdown:

```markdown
> ```
foo
```

```

Should give output:

```html
<blockquote><pre><code></code></pre></blockquote><p>foo</p><pre><code></code></pre>
```

But instead was:

```html
ERROR Problem at row 7 Expecting symbol ```
```
### [Example 208](https://spec.commonmark.org/0.29/#example-208)

This markdown:

```markdown
> foo
    - bar

```

Should give output:

```html
<blockquote><p>foo - bar</p></blockquote>
```

But instead was:

```html
<p>&gt; foo</p><pre><code>- bar</code></pre>
```
### [Example 209](https://spec.commonmark.org/0.29/#example-209)

This markdown:

```markdown
>

```

Should give output:

```html
<blockquote></blockquote>
```

But instead was:

```html
<p>&gt;</p>
```
### [Example 210](https://spec.commonmark.org/0.29/#example-210)

This markdown:

```markdown
>
>  
> 

```

Should give output:

```html
<blockquote></blockquote>
```

But instead was:

```html
<p>&gt; &gt; &gt;</p>
```
### [Example 211](https://spec.commonmark.org/0.29/#example-211)

This markdown:

```markdown
>
> foo
>  

```

Should give output:

```html
<blockquote><p>foo</p></blockquote>
```

But instead was:

```html
<p>&gt; &gt; foo &gt;</p>
```
### [Example 212](https://spec.commonmark.org/0.29/#example-212)

This markdown:

```markdown
> foo

> bar

```

Should give output:

```html
<blockquote><p>foo</p></blockquote><blockquote><p>bar</p></blockquote>
```

But instead was:

```html
<p>&gt; foo</p><p>&gt; bar</p>
```
### [Example 213](https://spec.commonmark.org/0.29/#example-213)

This markdown:

```markdown
> foo
> bar

```

Should give output:

```html
<blockquote><p>foo bar</p></blockquote>
```

But instead was:

```html
<p>&gt; foo &gt; bar</p>
```
### [Example 214](https://spec.commonmark.org/0.29/#example-214)

This markdown:

```markdown
> foo
>
> bar

```

Should give output:

```html
<blockquote><p>foo</p><p>bar</p></blockquote>
```

But instead was:

```html
<p>&gt; foo &gt; &gt; bar</p>
```
### [Example 215](https://spec.commonmark.org/0.29/#example-215)

This markdown:

```markdown
foo
> bar

```

Should give output:

```html
<p>foo</p><blockquote><p>bar</p></blockquote>
```

But instead was:

```html
<p>foo &gt; bar</p>
```
### [Example 216](https://spec.commonmark.org/0.29/#example-216)

This markdown:

```markdown
> aaa
***
> bbb

```

Should give output:

```html
<blockquote><p>aaa</p></blockquote><hr><blockquote><p>bbb</p></blockquote>
```

But instead was:

```html
<p>&gt; aaa</p><hr><p>&gt; bbb</p>
```
### [Example 217](https://spec.commonmark.org/0.29/#example-217)

This markdown:

```markdown
> bar
baz

```

Should give output:

```html
<blockquote><p>bar baz</p></blockquote>
```

But instead was:

```html
<p>&gt; bar baz</p>
```
### [Example 218](https://spec.commonmark.org/0.29/#example-218)

This markdown:

```markdown
> bar

baz

```

Should give output:

```html
<blockquote><p>bar</p></blockquote><p>baz</p>
```

But instead was:

```html
<p>&gt; bar</p><p>baz</p>
```
### [Example 219](https://spec.commonmark.org/0.29/#example-219)

This markdown:

```markdown
> bar
>
baz

```

Should give output:

```html
<blockquote><p>bar</p></blockquote><p>baz</p>
```

But instead was:

```html
<p>&gt; bar &gt; baz</p>
```
### [Example 220](https://spec.commonmark.org/0.29/#example-220)

This markdown:

```markdown
> > > foo
bar

```

Should give output:

```html
<blockquote><blockquote><blockquote><p>foo bar</p></blockquote></blockquote></blockquote>
```

But instead was:

```html
<p>&gt; &gt; &gt; foo bar</p>
```
### [Example 221](https://spec.commonmark.org/0.29/#example-221)

This markdown:

```markdown
>>> foo
> bar
>>baz

```

Should give output:

```html
<blockquote><blockquote><blockquote><p>foo bar baz</p></blockquote></blockquote></blockquote>
```

But instead was:

```html
<p>&gt;&gt;&gt; foo &gt; bar &gt;&gt;baz</p>
```
### [Example 222](https://spec.commonmark.org/0.29/#example-222)

This markdown:

```markdown
>     code

>    not code

```

Should give output:

```html
<blockquote><pre><code>code</code></pre></blockquote><blockquote><p>not code</p></blockquote>
```

But instead was:

```html
<p>&gt; code</p><p>&gt; not code</p>
```
## Indented code blocks

### [Example 77](https://spec.commonmark.org/0.29/#example-77)

This markdown:

```markdown
    a simple
      indented code block

```

Should give output:

```html
<pre><code>a simple indented code block</code></pre>
```

But instead was:

```html
<pre><code>a simple</code></pre><pre><code>indented code block</code></pre>
```
### [Example 78](https://spec.commonmark.org/0.29/#example-78)

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
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 79](https://spec.commonmark.org/0.29/#example-79)

This markdown:

```markdown
1.  foo

    - bar

```

Should give output:

```html
<ol><li><p>foo</p><ul><li>bar</li></ul></li></ol>
```

But instead was:

```html
<p>1. foo</p><pre><code>- bar</code></pre>
```
### [Example 80](https://spec.commonmark.org/0.29/#example-80)

This markdown:

```markdown
    <a/>
    *hi*

    - one

```

Should give output:

```html
<pre><code>&lt;a/&gt; *hi* - one</code></pre>
```

But instead was:

```html
<pre><code>&lt;a/&gt;</code></pre><pre><code>*hi*</code></pre><pre><code>- one</code></pre>
```
### [Example 81](https://spec.commonmark.org/0.29/#example-81)

This markdown:

```markdown
    chunk1

    chunk2
  
 
 
    chunk3

```

Should give output:

```html
<pre><code>chunk1 chunk2 chunk3</code></pre>
```

But instead was:

```html
ERROR Problem at row 6 Expecting --- Problem at row 6 Expecting *** Problem at row 6 Expecting ___
```
### [Example 82](https://spec.commonmark.org/0.29/#example-82)

This markdown:

```markdown
    chunk1
      
      chunk2

```

Should give output:

```html
<pre><code>chunk1 chunk2</code></pre>
```

But instead was:

```html
<pre><code>chunk1</code></pre><pre><code></code></pre><pre><code>chunk2</code></pre>
```
### [Example 83](https://spec.commonmark.org/0.29/#example-83)

This markdown:

```markdown
Foo
    bar


```

Should give output:

```html
<p>Foo bar</p>
```

But instead was:

```html
<p>Foo</p><pre><code>bar</code></pre>
```
### [Example 85](https://spec.commonmark.org/0.29/#example-85)

This markdown:

```markdown
# Heading
    foo
Heading
------
    foo
----

```

Should give output:

```html
<h1>Heading</h1><pre><code>foo</code></pre><h2>Heading</h2><pre><code>foo</code></pre><hr>
```

But instead was:

```html
<h1>Heading</h1><pre><code>foo</code></pre><p>Heading</p><hr><pre><code>foo</code></pre><hr>
```
### [Example 86](https://spec.commonmark.org/0.29/#example-86)

This markdown:

```markdown
        foo
    bar

```

Should give output:

```html
<pre><code>foo bar</code></pre>
```

But instead was:

```html
<pre><code>foo</code></pre><pre><code>bar</code></pre>
```
### [Example 87](https://spec.commonmark.org/0.29/#example-87)

This markdown:

```markdown

    
    foo
    


```

Should give output:

```html
<pre><code>foo</code></pre>
```

But instead was:

```html
<pre><code></code></pre><pre><code>foo</code></pre><pre><code></code></pre>
```
## Inlines

### [Example 297](https://spec.commonmark.org/0.29/#example-297)

This markdown:

```markdown
`hi`lo`

```

Should give output:

```html
<p><code>hi</code>lo`</p>
```

But instead was:

```html
<p><code>hi</code>lo</p>
```
## Setext headings

### [Example 50](https://spec.commonmark.org/0.29/#example-50)

This markdown:

```markdown
Foo *bar*
=========

Foo *bar*
---------

```

Should give output:

```html
<h1>Foo<em>bar</em></h1><h2>Foo<em>bar</em></h2>
```

But instead was:

```html
<p>Foo<em>bar</em>=========</p><p>Foo<em>bar</em></p><hr>
```
### [Example 51](https://spec.commonmark.org/0.29/#example-51)

This markdown:

```markdown
Foo *bar
baz*
====

```

Should give output:

```html
<h1>Foo<em>bar baz</em></h1>
```

But instead was:

```html
<p>Foo<em>bar baz</em>====</p>
```
### [Example 52](https://spec.commonmark.org/0.29/#example-52)

This markdown:

```markdown
  Foo *bar
baz*	
====

```

Should give output:

```html
<h1>Foo<em>bar baz</em></h1>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 53](https://spec.commonmark.org/0.29/#example-53)

This markdown:

```markdown
Foo
-------------------------

Foo
=

```

Should give output:

```html
<h2>Foo</h2><h1>Foo</h1>
```

But instead was:

```html
<p>Foo</p><hr><p>Foo =</p>
```
### [Example 54](https://spec.commonmark.org/0.29/#example-54)

This markdown:

```markdown
   Foo
---

  Foo
-----

  Foo
  ===

```

Should give output:

```html
<h2>Foo</h2><h2>Foo</h2><h1>Foo</h1>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 55](https://spec.commonmark.org/0.29/#example-55)

This markdown:

```markdown
    Foo
    ---

    Foo
---

```

Should give output:

```html
<pre><code>Foo --- Foo</code></pre><hr>
```

But instead was:

```html
<pre><code>Foo</code></pre><pre><code>---</code></pre><pre><code>Foo</code></pre><hr>
```
### [Example 56](https://spec.commonmark.org/0.29/#example-56)

This markdown:

```markdown
Foo
   ----      

```

Should give output:

```html
<h2>Foo</h2>
```

But instead was:

```html
<p>Foo</p><hr>
```
### [Example 57](https://spec.commonmark.org/0.29/#example-57)

This markdown:

```markdown
Foo
    ---

```

Should give output:

```html
<p>Foo ---</p>
```

But instead was:

```html
<p>Foo</p><pre><code>---</code></pre>
```
### [Example 58](https://spec.commonmark.org/0.29/#example-58)

This markdown:

```markdown
Foo
= =

Foo
--- -

```

Should give output:

```html
<p>Foo = =</p><p>Foo</p><hr>
```

But instead was:

```html
ERROR Problem at row 8 Expecting end Problem at row 8 Expecting newline
```
### [Example 59](https://spec.commonmark.org/0.29/#example-59)

This markdown:

```markdown
Foo  
-----

```

Should give output:

```html
<h2>Foo</h2>
```

But instead was:

```html
<p>Foo</p><hr>
```
### [Example 60](https://spec.commonmark.org/0.29/#example-60)

This markdown:

```markdown
Foo\
----

```

Should give output:

```html
<h2>Foo\</h2>
```

But instead was:

```html
<p>Foo\</p><hr>
```
### [Example 61](https://spec.commonmark.org/0.29/#example-61)

This markdown:

```markdown
`Foo
----
`

<a title="a lot
---
of dashes"/>

```

Should give output:

```html
<h2>`Foo</h2><p>`</p><h2>&lt;a title=&quot;a lot</h2><p>of dashes&quot;/&gt;</p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 62](https://spec.commonmark.org/0.29/#example-62)

This markdown:

```markdown
> Foo
---

```

Should give output:

```html
<blockquote><p>Foo</p></blockquote><hr>
```

But instead was:

```html
<p>&gt; Foo</p><hr>
```
### [Example 63](https://spec.commonmark.org/0.29/#example-63)

This markdown:

```markdown
> foo
bar
===

```

Should give output:

```html
<blockquote><p>foo bar ===</p></blockquote>
```

But instead was:

```html
<p>&gt; foo bar ===</p>
```
### [Example 64](https://spec.commonmark.org/0.29/#example-64)

This markdown:

```markdown
- Foo
---

```

Should give output:

```html
<ul><li>Foo</li></ul><hr>
```

But instead was:

```html
<ul><li><p>Foo</p></li><li><p>--</p></li></ul>
```
### [Example 65](https://spec.commonmark.org/0.29/#example-65)

This markdown:

```markdown
Foo
Bar
---

```

Should give output:

```html
<h2>Foo Bar</h2>
```

But instead was:

```html
<p>Foo Bar</p><hr>
```
### [Example 66](https://spec.commonmark.org/0.29/#example-66)

This markdown:

```markdown
---
Foo
---
Bar
---
Baz

```

Should give output:

```html
<hr><h2>Foo</h2><h2>Bar</h2><p>Baz</p>
```

But instead was:

```html
<hr><p>Foo</p><hr><p>Bar</p><hr><p>Baz</p>
```
### [Example 69](https://spec.commonmark.org/0.29/#example-69)

This markdown:

```markdown
- foo
-----

```

Should give output:

```html
<ul><li>foo</li></ul><hr>
```

But instead was:

```html
<ul><li><p>foo</p></li><li><p>----</p></li></ul>
```
### [Example 71](https://spec.commonmark.org/0.29/#example-71)

This markdown:

```markdown
> foo
-----

```

Should give output:

```html
<blockquote><p>foo</p></blockquote><hr>
```

But instead was:

```html
<p>&gt; foo</p><hr>
```
### [Example 72](https://spec.commonmark.org/0.29/#example-72)

This markdown:

```markdown
\> foo
------

```

Should give output:

```html
<h2>&gt; foo</h2>
```

But instead was:

```html
<p>\&gt; foo</p><hr>
```
### [Example 73](https://spec.commonmark.org/0.29/#example-73)

This markdown:

```markdown
Foo

bar
---
baz

```

Should give output:

```html
<p>Foo</p><h2>bar</h2><p>baz</p>
```

But instead was:

```html
<p>Foo</p><p>bar</p><hr><p>baz</p>
```
### [Example 75](https://spec.commonmark.org/0.29/#example-75)

This markdown:

```markdown
Foo
bar
* * *
baz

```

Should give output:

```html
<p>Foo bar</p><hr><p>baz</p>
```

But instead was:

```html
<p>Foo bar<em></em><em>baz</em></p>
```
### [Example 76](https://spec.commonmark.org/0.29/#example-76)

This markdown:

```markdown
Foo
bar
\---
baz

```

Should give output:

```html
<p>Foo bar --- baz</p>
```

But instead was:

```html
<p>Foo bar \--- baz</p>
```
## Backslash escapes

### [Example 298](https://spec.commonmark.org/0.29/#example-298)

This markdown:

```markdown
\!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~

```

Should give output:

```html
<p>!&quot;#$%&amp;&#39;()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
### [Example 300](https://spec.commonmark.org/0.29/#example-300)

This markdown:

```markdown
\*not emphasized*
\<br/> not a tag
\[not a link](/foo)
\`not code`
1\. not a list
\* not a list
\# not a heading
\[foo]: /url "not a reference"
\&ouml; not a character entity

```

Should give output:

```html
<p>*not emphasized* &lt;br/&gt; not a tag [not a link](/foo) `not code` 1. not a list * not a list # not a heading [foo]: /url &quot;not a reference&quot; &amp;ouml; not a character entity</p>
```

But instead was:

```html
ERROR Problem at row 19 Expecting Problem at row 1 Expecting symbol (
```
### [Example 301](https://spec.commonmark.org/0.29/#example-301)

This markdown:

```markdown
\\*emphasis*

```

Should give output:

```html
<p>\<em>emphasis</em></p>
```

But instead was:

```html
<p>\\<em>emphasis</em></p>
```
### [Example 302](https://spec.commonmark.org/0.29/#example-302)

This markdown:

```markdown
foo\
bar

```

Should give output:

```html
<p>foo<br>bar</p>
```

But instead was:

```html
<p>foo\ bar</p>
```
### [Example 303](https://spec.commonmark.org/0.29/#example-303)

This markdown:

```markdown
`` \[\` ``

```

Should give output:

```html
<p><code>\[\`</code></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol ]
```
### [Example 306](https://spec.commonmark.org/0.29/#example-306)

This markdown:

```markdown
<http://example.com?find=\*>

```

Should give output:

```html
<p><a href="http://example.com?find=%5C*">http://example.com?find=\*</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 307](https://spec.commonmark.org/0.29/#example-307)

This markdown:

```markdown
<a href="/bar\/)">

```

Should give output:

```html
<a href="/bar\/)">
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 308](https://spec.commonmark.org/0.29/#example-308)

This markdown:

```markdown
[foo](/bar\* "ti\*tle")

```

Should give output:

```html
<p><a href="/bar*" title="ti*tle">foo</a></p>
```

But instead was:

```html
<p><a href="/bar\* \" ti\*tle\""="">foo</a></p>
```
### [Example 309](https://spec.commonmark.org/0.29/#example-309)

This markdown:

```markdown
[foo]

[foo]: /bar\* "ti\*tle"

```

Should give output:

```html
<p><a href="/bar*" title="ti*tle">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 310](https://spec.commonmark.org/0.29/#example-310)

This markdown:

```markdown
``` foo\+bar
foo
```

```

Should give output:

```html
<pre><code class="language-foo+bar">foo</code></pre>
```

But instead was:

```html
<pre><code>foo</code></pre>
```
## Links

### [Example 481](https://spec.commonmark.org/0.29/#example-481)

This markdown:

```markdown
[link](/uri "title")

```

Should give output:

```html
<p><a href="/uri" title="title">link</a></p>
```

But instead was:

```html
<p><a href="/uri \" title\""="">link</a></p>
```
### [Example 484](https://spec.commonmark.org/0.29/#example-484)

This markdown:

```markdown
[link](<>)

```

Should give output:

```html
<p><a href="">link</a></p>
```

But instead was:

```html
<p><a href="&lt;&gt;">link</a></p>
```
### [Example 485](https://spec.commonmark.org/0.29/#example-485)

This markdown:

```markdown
[link](/my uri)

```

Should give output:

```html
<p>[link](/my uri)</p>
```

But instead was:

```html
<p><a href="/my uri">link</a></p>
```
### [Example 486](https://spec.commonmark.org/0.29/#example-486)

This markdown:

```markdown
[link](</my uri>)

```

Should give output:

```html
<p><a href="/my%20uri">link</a></p>
```

But instead was:

```html
<p><a href="&lt;/my uri&gt;">link</a></p>
```
### [Example 487](https://spec.commonmark.org/0.29/#example-487)

This markdown:

```markdown
[link](foo
bar)

```

Should give output:

```html
<p>[link](foo bar)</p>
```

But instead was:

```html
<p><a href="foo bar">link</a></p>
```
### [Example 488](https://spec.commonmark.org/0.29/#example-488)

This markdown:

```markdown
[link](<foo
bar>)

```

Should give output:

```html
<p>[link](<foo bar="">)</p>
```

But instead was:

```html
<p><a href="&lt;foo bar&gt;">link</a></p>
```
### [Example 489](https://spec.commonmark.org/0.29/#example-489)

This markdown:

```markdown
[a](<b)c>)

```

Should give output:

```html
<p><a href="b)c">a</a></p>
```

But instead was:

```html
<p><a href="&lt;b">a</a>c&gt;)</p>
```
### [Example 490](https://spec.commonmark.org/0.29/#example-490)

This markdown:

```markdown
[link](<foo\>)

```

Should give output:

```html
<p>[link](&lt;foo&gt;)</p>
```

But instead was:

```html
<p><a href="&lt;foo\&gt;">link</a></p>
```
### [Example 491](https://spec.commonmark.org/0.29/#example-491)

This markdown:

```markdown
[a](<b)c
[a](<b)c>
[a](<b>c)

```

Should give output:

```html
<p>[a](&lt;b)c [a](&lt;b)c&gt; [a](<b>c)</p>
```

But instead was:

```html
<p><a href="&lt;b">a</a>c<a href="&lt;b">a</a>c&gt;<a href="&lt;b&gt;c">a</a></p>
```
### [Example 492](https://spec.commonmark.org/0.29/#example-492)

This markdown:

```markdown
[link](\(foo\))

```

Should give output:

```html
<p><a href="(foo)">link</a></p>
```

But instead was:

```html
<p><a href="\(foo\">link</a>)</p>
```
### [Example 493](https://spec.commonmark.org/0.29/#example-493)

This markdown:

```markdown
[link](foo(and(bar)))

```

Should give output:

```html
<p><a href="foo(and(bar))">link</a></p>
```

But instead was:

```html
<p><a href="foo(and(bar">link</a>))</p>
```
### [Example 494](https://spec.commonmark.org/0.29/#example-494)

This markdown:

```markdown
[link](foo\(and\(bar\))

```

Should give output:

```html
<p><a href="foo(and(bar)">link</a></p>
```

But instead was:

```html
<p><a href="foo\(and\(bar\">link</a>)</p>
```
### [Example 495](https://spec.commonmark.org/0.29/#example-495)

This markdown:

```markdown
[link](<foo(and(bar)>)

```

Should give output:

```html
<p><a href="foo(and(bar)">link</a></p>
```

But instead was:

```html
<p><a href="&lt;foo(and(bar">link</a>&gt;)</p>
```
### [Example 496](https://spec.commonmark.org/0.29/#example-496)

This markdown:

```markdown
[link](foo\)\:)

```

Should give output:

```html
<p><a href="foo):">link</a></p>
```

But instead was:

```html
<p><a href="foo\">link</a>\:)</p>
```
### [Example 498](https://spec.commonmark.org/0.29/#example-498)

This markdown:

```markdown
[link](foo\bar)

```

Should give output:

```html
<p><a href="foo%5Cbar">link</a></p>
```

But instead was:

```html
<p><a href="foo\bar">link</a></p>
```
### [Example 499](https://spec.commonmark.org/0.29/#example-499)

This markdown:

```markdown
[link](foo%20b&auml;)

```

Should give output:

```html
<p><a href="foo%20b%C3%A4">link</a></p>
```

But instead was:

```html
<p><a href="foo%20bä">link</a></p>
```
### [Example 500](https://spec.commonmark.org/0.29/#example-500)

This markdown:

```markdown
[link]("title")

```

Should give output:

```html
<p><a href="%22title%22">link</a></p>
```

But instead was:

```html
<p><a href="\" title\""="">link</a></p>
```
### [Example 501](https://spec.commonmark.org/0.29/#example-501)

This markdown:

```markdown
[link](/url "title")
[link](/url 'title')
[link](/url (title))

```

Should give output:

```html
<p><a href="/url" title="title">link</a><a href="/url" title="title">link</a><a href="/url" title="title">link</a></p>
```

But instead was:

```html
<p><a href="/url \" title\""="">link</a><a href="/url &#39;title&#39;">link</a><a href="/url (title">link</a>)</p>
```
### [Example 502](https://spec.commonmark.org/0.29/#example-502)

This markdown:

```markdown
[link](/url "title \"&quot;")

```

Should give output:

```html
<p><a href="/url" title="title &quot;&quot;">link</a></p>
```

But instead was:

```html
<p><a \\"&quot;\""="" href="/url \" title="">link</a></p>
```
### [Example 503](https://spec.commonmark.org/0.29/#example-503)

This markdown:

```markdown
[link](/url "title")

```

Should give output:

```html
<p><a href="/url%C2%A0%22title%22">link</a></p>
```

But instead was:

```html
<p><a href="/url \" title\""="">link</a></p>
```
### [Example 504](https://spec.commonmark.org/0.29/#example-504)

This markdown:

```markdown
[link](/url "title "and" title")

```

Should give output:

```html
<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>
```

But instead was:

```html
<p><a \"and\"="" href="/url \" title="" title\""="">link</a></p>
```
### [Example 505](https://spec.commonmark.org/0.29/#example-505)

This markdown:

```markdown
[link](/url 'title "and" title')

```

Should give output:

```html
<p><a href="/url" title="title &quot;and&quot; title">link</a></p>
```

But instead was:

```html
<p><a and\"="" href="/url &#39;title \" title'"="">link</a></p>
```
### [Example 506](https://spec.commonmark.org/0.29/#example-506)

This markdown:

```markdown
[link](   /uri
  "title"  )

```

Should give output:

```html
<p><a href="/uri" title="title">link</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 507](https://spec.commonmark.org/0.29/#example-507)

This markdown:

```markdown
[link] (/uri)

```

Should give output:

```html
<p>[link] (/uri)</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
### [Example 508](https://spec.commonmark.org/0.29/#example-508)

This markdown:

```markdown
[link [foo [bar]]](/uri)

```

Should give output:

```html
<p><a href="/uri">link [foo [bar]]</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
### [Example 509](https://spec.commonmark.org/0.29/#example-509)

This markdown:

```markdown
[link] bar](/uri)

```

Should give output:

```html
<p>[link] bar](/uri)</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
### [Example 510](https://spec.commonmark.org/0.29/#example-510)

This markdown:

```markdown
[link [bar](/uri)

```

Should give output:

```html
<p>[link<a href="/uri">bar</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
### [Example 511](https://spec.commonmark.org/0.29/#example-511)

This markdown:

```markdown
[link \[bar](/uri)

```

Should give output:

```html
<p><a href="/uri">link [bar</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
### [Example 512](https://spec.commonmark.org/0.29/#example-512)

This markdown:

```markdown
[link *foo **bar** `#`*](/uri)

```

Should give output:

```html
<p><a href="/uri">link<em>foo<strong>bar</strong><code>#</code></em></a></p>
```

But instead was:

```html
<p><a href="/uri">link<em>foo</em><strong>bar</strong><em></em><em>#</em></a></p>
```
### [Example 513](https://spec.commonmark.org/0.29/#example-513)

This markdown:

```markdown
[![moon](moon.jpg)](/uri)

```

Should give output:

```html
<p><a href="/uri"><img alt="moon" src="moon.jpg"></a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
### [Example 514](https://spec.commonmark.org/0.29/#example-514)

This markdown:

```markdown
[foo [bar](/uri)](/uri)

```

Should give output:

```html
<p>[foo<a href="/uri">bar</a>](/uri)</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
### [Example 515](https://spec.commonmark.org/0.29/#example-515)

This markdown:

```markdown
[foo *[bar [baz](/uri)](/uri)*](/uri)

```

Should give output:

```html
<p>[foo<em>[bar<a href="/uri">baz</a>](/uri)</em>](/uri)</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
### [Example 516](https://spec.commonmark.org/0.29/#example-516)

This markdown:

```markdown
![[[foo](uri1)](uri2)](uri3)

```

Should give output:

```html
<p><img alt="[foo](uri2)" src="uri3"></p>
```

But instead was:

```html
<p><img src="uri1">](uri2)](uri3)</p>
```
### [Example 517](https://spec.commonmark.org/0.29/#example-517)

This markdown:

```markdown
*[foo*](/uri)

```

Should give output:

```html
<p>*<a href="/uri">foo*</a></p>
```

But instead was:

```html
<p><a href="/uri">foo</a></p>
```
### [Example 518](https://spec.commonmark.org/0.29/#example-518)

This markdown:

```markdown
[foo *bar](baz*)

```

Should give output:

```html
<p><a href="baz*">foo *bar</a></p>
```

But instead was:

```html
<p><a href="baz*">foo<em>bar</em></a></p>
```
### [Example 519](https://spec.commonmark.org/0.29/#example-519)

This markdown:

```markdown
*foo [bar* baz]

```

Should give output:

```html
<p><em>foo [bar</em>baz]</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
### [Example 520](https://spec.commonmark.org/0.29/#example-520)

This markdown:

```markdown
[foo <bar attr="](baz)">

```

Should give output:

```html
<p>[foo<bar attr="](baz)"></p>
```

But instead was:

```html
<p><a href="baz">foo &lt;bar attr=&quot;</a>&quot;&gt;</p>
```
### [Example 521](https://spec.commonmark.org/0.29/#example-521)

This markdown:

```markdown
[foo`](/uri)`

```

Should give output:

```html
<p>[foo<code>](/uri)</code></p>
```

But instead was:

```html
<p><a href="/uri">foo</a></p>
```
### [Example 522](https://spec.commonmark.org/0.29/#example-522)

This markdown:

```markdown
[foo<http://example.com/?search=](uri)>

```

Should give output:

```html
<p>[foo<a href="http://example.com/?search=%5D(uri)">http://example.com/?search=](uri)</a></p>
```

But instead was:

```html
<p><a href="uri">foo&lt;http://example.com/?search=</a>&gt;</p>
```
### [Example 523](https://spec.commonmark.org/0.29/#example-523)

This markdown:

```markdown
[foo][bar]

[bar]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 524](https://spec.commonmark.org/0.29/#example-524)

This markdown:

```markdown
[link [foo [bar]]][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri">link [foo [bar]]</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 525](https://spec.commonmark.org/0.29/#example-525)

This markdown:

```markdown
[link \[bar][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri">link [bar</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 526](https://spec.commonmark.org/0.29/#example-526)

This markdown:

```markdown
[link *foo **bar** `#`*][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri">link<em>foo<strong>bar</strong><code>#</code></em></a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 527](https://spec.commonmark.org/0.29/#example-527)

This markdown:

```markdown
[![moon](moon.jpg)][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri"><img alt="moon" src="moon.jpg"></a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 528](https://spec.commonmark.org/0.29/#example-528)

This markdown:

```markdown
[foo [bar](/uri)][ref]

[ref]: /uri

```

Should give output:

```html
<p>[foo<a href="/uri">bar</a>]<a href="/uri">ref</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 529](https://spec.commonmark.org/0.29/#example-529)

This markdown:

```markdown
[foo *bar [baz][ref]*][ref]

[ref]: /uri

```

Should give output:

```html
<p>[foo<em>bar<a href="/uri">baz</a></em>]<a href="/uri">ref</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 530](https://spec.commonmark.org/0.29/#example-530)

This markdown:

```markdown
*[foo*][ref]

[ref]: /uri

```

Should give output:

```html
<p>*<a href="/uri">foo*</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 531](https://spec.commonmark.org/0.29/#example-531)

This markdown:

```markdown
[foo *bar][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri">foo *bar</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 532](https://spec.commonmark.org/0.29/#example-532)

This markdown:

```markdown
[foo <bar attr="][ref]">

[ref]: /uri

```

Should give output:

```html
<p>[foo<bar attr="][ref]"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 533](https://spec.commonmark.org/0.29/#example-533)

This markdown:

```markdown
[foo`][ref]`

[ref]: /uri

```

Should give output:

```html
<p>[foo<code>][ref]</code></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 534](https://spec.commonmark.org/0.29/#example-534)

This markdown:

```markdown
[foo<http://example.com/?search=][ref]>

[ref]: /uri

```

Should give output:

```html
<p>[foo<a href="http://example.com/?search=%5D%5Bref%5D">http://example.com/?search=][ref]</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 535](https://spec.commonmark.org/0.29/#example-535)

This markdown:

```markdown
[foo][BaR]

[bar]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 536](https://spec.commonmark.org/0.29/#example-536)

This markdown:

```markdown
[Толпой][Толпой] is a Russian word.

[ТОЛПОЙ]: /url

```

Should give output:

```html
<p><a href="/url">Толпой</a>is a Russian word.</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 537](https://spec.commonmark.org/0.29/#example-537)

This markdown:

```markdown
[Foo
  bar]: /url

[Baz][Foo bar]

```

Should give output:

```html
<p><a href="/url">Baz</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 538](https://spec.commonmark.org/0.29/#example-538)

This markdown:

```markdown
[foo] [bar]

[bar]: /url "title"

```

Should give output:

```html
<p>[foo]<a href="/url" title="title">bar</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 539](https://spec.commonmark.org/0.29/#example-539)

This markdown:

```markdown
[foo]
[bar]

[bar]: /url "title"

```

Should give output:

```html
<p>[foo]<a href="/url" title="title">bar</a></p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
### [Example 540](https://spec.commonmark.org/0.29/#example-540)

This markdown:

```markdown
[foo]: /url1

[foo]: /url2

[bar][foo]

```

Should give output:

```html
<p><a href="/url1">bar</a></p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
### [Example 541](https://spec.commonmark.org/0.29/#example-541)

This markdown:

```markdown
[bar][foo\!]

[foo!]: /url

```

Should give output:

```html
<p>[bar][foo!]</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 542](https://spec.commonmark.org/0.29/#example-542)

This markdown:

```markdown
[foo][ref[]

[ref[]: /uri

```

Should give output:

```html
<p>[foo][ref[]</p><p>[ref[]: /uri</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 543](https://spec.commonmark.org/0.29/#example-543)

This markdown:

```markdown
[foo][ref[bar]]

[ref[bar]]: /uri

```

Should give output:

```html
<p>[foo][ref[bar]]</p><p>[ref[bar]]: /uri</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 544](https://spec.commonmark.org/0.29/#example-544)

This markdown:

```markdown
[[[foo]]]

[[[foo]]]: /url

```

Should give output:

```html
<p>[[[foo]]]</p><p>[[[foo]]]: /url</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 545](https://spec.commonmark.org/0.29/#example-545)

This markdown:

```markdown
[foo][ref\[]

[ref\[]: /uri

```

Should give output:

```html
<p><a href="/uri">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 546](https://spec.commonmark.org/0.29/#example-546)

This markdown:

```markdown
[bar\\]: /uri

[bar\\]

```

Should give output:

```html
<p><a href="/uri">bar\</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 547](https://spec.commonmark.org/0.29/#example-547)

This markdown:

```markdown
[]

[]: /uri

```

Should give output:

```html
<p>[]</p><p>[]: /uri</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 548](https://spec.commonmark.org/0.29/#example-548)

This markdown:

```markdown
[
 ]

[
 ]: /uri

```

Should give output:

```html
<p>[ ]</p><p>[ ]: /uri</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
### [Example 549](https://spec.commonmark.org/0.29/#example-549)

This markdown:

```markdown
[foo][]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 550](https://spec.commonmark.org/0.29/#example-550)

This markdown:

```markdown
[*foo* bar][]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title"><em>foo</em>bar</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 551](https://spec.commonmark.org/0.29/#example-551)

This markdown:

```markdown
[Foo][]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">Foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 552](https://spec.commonmark.org/0.29/#example-552)

This markdown:

```markdown
[foo] 
[]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a>[]</p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
### [Example 553](https://spec.commonmark.org/0.29/#example-553)

This markdown:

```markdown
[foo]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 554](https://spec.commonmark.org/0.29/#example-554)

This markdown:

```markdown
[*foo* bar]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title"><em>foo</em>bar</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 555](https://spec.commonmark.org/0.29/#example-555)

This markdown:

```markdown
[[*foo* bar]]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p>[<a href="/url" title="title"><em>foo</em>bar</a>]</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 556](https://spec.commonmark.org/0.29/#example-556)

This markdown:

```markdown
[[bar [foo]

[foo]: /url

```

Should give output:

```html
<p>[[bar<a href="/url">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 557](https://spec.commonmark.org/0.29/#example-557)

This markdown:

```markdown
[Foo]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">Foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 558](https://spec.commonmark.org/0.29/#example-558)

This markdown:

```markdown
[foo] bar

[foo]: /url

```

Should give output:

```html
<p><a href="/url">foo</a>bar</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 559](https://spec.commonmark.org/0.29/#example-559)

This markdown:

```markdown
\[foo]

[foo]: /url "title"

```

Should give output:

```html
<p>[foo]</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 560](https://spec.commonmark.org/0.29/#example-560)

This markdown:

```markdown
[foo*]: /url

*[foo*]

```

Should give output:

```html
<p>*<a href="/url">foo*</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 561](https://spec.commonmark.org/0.29/#example-561)

This markdown:

```markdown
[foo][bar]

[foo]: /url1
[bar]: /url2

```

Should give output:

```html
<p><a href="/url2">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
### [Example 562](https://spec.commonmark.org/0.29/#example-562)

This markdown:

```markdown
[foo][]

[foo]: /url1

```

Should give output:

```html
<p><a href="/url1">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 563](https://spec.commonmark.org/0.29/#example-563)

This markdown:

```markdown
[foo]()

[foo]: /url1

```

Should give output:

```html
<p><a href="">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 564](https://spec.commonmark.org/0.29/#example-564)

This markdown:

```markdown
[foo](not a link)

[foo]: /url1

```

Should give output:

```html
<p><a href="/url1">foo</a>(not a link)</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 565](https://spec.commonmark.org/0.29/#example-565)

This markdown:

```markdown
[foo][bar][baz]

[baz]: /url

```

Should give output:

```html
<p>[foo]<a href="/url">bar</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 566](https://spec.commonmark.org/0.29/#example-566)

This markdown:

```markdown
[foo][bar][baz]

[baz]: /url1
[bar]: /url2

```

Should give output:

```html
<p><a href="/url2">foo</a><a href="/url1">baz</a></p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
### [Example 567](https://spec.commonmark.org/0.29/#example-567)

This markdown:

```markdown
[foo][bar][baz]

[baz]: /url1
[foo]: /url2

```

Should give output:

```html
<p>[foo]<a href="/url1">bar</a></p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
## Code spans

### [Example 329](https://spec.commonmark.org/0.29/#example-329)

This markdown:

```markdown
`` foo ` bar ``

```

Should give output:

```html
<p><code>foo ` bar</code></p>
```

But instead was:

```html
<p>foo<code>bar</code></p>
```
### [Example 330](https://spec.commonmark.org/0.29/#example-330)

This markdown:

```markdown
` `` `

```

Should give output:

```html
<p><code>``</code></p>
```

But instead was:

```html
<p><code></code><code></code></p>
```
### [Example 331](https://spec.commonmark.org/0.29/#example-331)

This markdown:

```markdown
`  ``  `

```

Should give output:

```html
<p><code>``</code></p>
```

But instead was:

```html
<p><code></code><code></code></p>
```
### [Example 335](https://spec.commonmark.org/0.29/#example-335)

This markdown:

```markdown
``
foo
bar  
baz
``

```

Should give output:

```html
<p><code>foo bar baz</code></p>
```

But instead was:

```html
<p>foo bar baz</p>
```
### [Example 336](https://spec.commonmark.org/0.29/#example-336)

This markdown:

```markdown
``
foo 
``

```

Should give output:

```html
<p><code>foo</code></p>
```

But instead was:

```html
<p>foo</p>
```
### [Example 338](https://spec.commonmark.org/0.29/#example-338)

This markdown:

```markdown
`foo\`bar`

```

Should give output:

```html
<p><code>foo\</code>bar`</p>
```

But instead was:

```html
<p><code>foo\</code>bar</p>
```
### [Example 339](https://spec.commonmark.org/0.29/#example-339)

This markdown:

```markdown
``foo`bar``

```

Should give output:

```html
<p><code>foo`bar</code></p>
```

But instead was:

```html
<p>foo<code>bar</code></p>
```
### [Example 340](https://spec.commonmark.org/0.29/#example-340)

This markdown:

```markdown
` foo `` bar `

```

Should give output:

```html
<p><code>foo `` bar</code></p>
```

But instead was:

```html
<p><code>foo</code><code>bar</code></p>
```
### [Example 341](https://spec.commonmark.org/0.29/#example-341)

This markdown:

```markdown
*foo`*`

```

Should give output:

```html
<p>*foo<code>*</code></p>
```

But instead was:

```html
<p><em>foo</em></p>
```
### [Example 342](https://spec.commonmark.org/0.29/#example-342)

This markdown:

```markdown
[not a `link](/foo`)

```

Should give output:

```html
<p>[not a<code>link](/foo</code>)</p>
```

But instead was:

```html
<p><a href="/foo`">not a<code>link</code></a></p>
```
### [Example 343](https://spec.commonmark.org/0.29/#example-343)

This markdown:

```markdown
`<a href="`">`

```

Should give output:

```html
<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>
```

But instead was:

```html
<p><code>&lt;a href=&quot;</code>&quot;&gt;</p>
```
### [Example 344](https://spec.commonmark.org/0.29/#example-344)

This markdown:

```markdown
<a href="`">`

```

Should give output:

```html
<p><a href="`">`</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 345](https://spec.commonmark.org/0.29/#example-345)

This markdown:

```markdown
`<http://foo.bar.`baz>`

```

Should give output:

```html
<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>
```

But instead was:

```html
<p><code>&lt;http://foo.bar.</code>baz&gt;</p>
```
### [Example 346](https://spec.commonmark.org/0.29/#example-346)

This markdown:

```markdown
<http://foo.bar.`baz>`

```

Should give output:

```html
<p><a href="http://foo.bar.%60baz">http://foo.bar.`baz</a>`</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 347](https://spec.commonmark.org/0.29/#example-347)

This markdown:

```markdown
```foo``

```

Should give output:

```html
<p>```foo``</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol ```
```
### [Example 348](https://spec.commonmark.org/0.29/#example-348)

This markdown:

```markdown
`foo

```

Should give output:

```html
<p>`foo</p>
```

But instead was:

```html
<p><code>foo</code></p>
```
### [Example 349](https://spec.commonmark.org/0.29/#example-349)

This markdown:

```markdown
`foo``bar``

```

Should give output:

```html
<p>`foo<code>bar</code></p>
```

But instead was:

```html
<p><code>foo</code><code>bar</code></p>
```
## Emphasis and strong emphasis

### [Example 351](https://spec.commonmark.org/0.29/#example-351)

This markdown:

```markdown
a * foo bar*

```

Should give output:

```html
<p>a * foo bar*</p>
```

But instead was:

```html
<p>a<em>foo bar</em></p>
```
### [Example 352](https://spec.commonmark.org/0.29/#example-352)

This markdown:

```markdown
a*"foo"*

```

Should give output:

```html
<p>a*&quot;foo&quot;*</p>
```

But instead was:

```html
<p>a<em>&quot;foo&quot;</em></p>
```
### [Example 353](https://spec.commonmark.org/0.29/#example-353)

This markdown:

```markdown
* a *

```

Should give output:

```html
<p>* a *</p>
```

But instead was:

```html
<p><em>a</em></p>
```
### [Example 356](https://spec.commonmark.org/0.29/#example-356)

This markdown:

```markdown
_foo bar_

```

Should give output:

```html
<p><em>foo bar</em></p>
```

But instead was:

```html
<p>_foo bar_</p>
```
### [Example 363](https://spec.commonmark.org/0.29/#example-363)

This markdown:

```markdown
foo-_(bar)_

```

Should give output:

```html
<p>foo-<em>(bar)</em></p>
```

But instead was:

```html
<p>foo-_(bar)_</p>
```
### [Example 364](https://spec.commonmark.org/0.29/#example-364)

This markdown:

```markdown
_foo*

```

Should give output:

```html
<p>_foo*</p>
```

But instead was:

```html
<p>_foo</p>
```
### [Example 365](https://spec.commonmark.org/0.29/#example-365)

This markdown:

```markdown
*foo bar *

```

Should give output:

```html
<p>*foo bar *</p>
```

But instead was:

```html
<p><em>foo bar</em></p>
```
### [Example 366](https://spec.commonmark.org/0.29/#example-366)

This markdown:

```markdown
*foo bar
*

```

Should give output:

```html
<p>*foo bar *</p>
```

But instead was:

```html
<p><em>foo bar</em></p>
```
### [Example 367](https://spec.commonmark.org/0.29/#example-367)

This markdown:

```markdown
*(*foo)

```

Should give output:

```html
<p>*(*foo)</p>
```

But instead was:

```html
<p><em>(</em>foo)</p>
```
### [Example 368](https://spec.commonmark.org/0.29/#example-368)

This markdown:

```markdown
*(*foo*)*

```

Should give output:

```html
<p><em>(<em>foo</em>)</em></p>
```

But instead was:

```html
<p><em>(</em>foo<em>)</em></p>
```
### [Example 372](https://spec.commonmark.org/0.29/#example-372)

This markdown:

```markdown
_(_foo_)_

```

Should give output:

```html
<p><em>(<em>foo</em>)</em></p>
```

But instead was:

```html
<p>_(_foo_)_</p>
```
### [Example 375](https://spec.commonmark.org/0.29/#example-375)

This markdown:

```markdown
_foo_bar_baz_

```

Should give output:

```html
<p><em>foo_bar_baz</em></p>
```

But instead was:

```html
<p>_foo_bar_baz_</p>
```
### [Example 376](https://spec.commonmark.org/0.29/#example-376)

This markdown:

```markdown
_(bar)_.

```

Should give output:

```html
<p><em>(bar)</em>.</p>
```

But instead was:

```html
<p>_(bar)_.</p>
```
### [Example 378](https://spec.commonmark.org/0.29/#example-378)

This markdown:

```markdown
** foo bar**

```

Should give output:

```html
<p>** foo bar**</p>
```

But instead was:

```html
<p><strong>foo bar</strong></p>
```
### [Example 379](https://spec.commonmark.org/0.29/#example-379)

This markdown:

```markdown
a**"foo"**

```

Should give output:

```html
<p>a**&quot;foo&quot;**</p>
```

But instead was:

```html
<p>a<strong>&quot;foo&quot;</strong></p>
```
### [Example 381](https://spec.commonmark.org/0.29/#example-381)

This markdown:

```markdown
__foo bar__

```

Should give output:

```html
<p><strong>foo bar</strong></p>
```

But instead was:

```html
<p>__foo bar__</p>
```
### [Example 388](https://spec.commonmark.org/0.29/#example-388)

This markdown:

```markdown
__foo, __bar__, baz__

```

Should give output:

```html
<p><strong>foo,<strong>bar</strong>, baz</strong></p>
```

But instead was:

```html
<p>__foo, __bar__, baz__</p>
```
### [Example 389](https://spec.commonmark.org/0.29/#example-389)

This markdown:

```markdown
foo-__(bar)__

```

Should give output:

```html
<p>foo-<strong>(bar)</strong></p>
```

But instead was:

```html
<p>foo-__(bar)__</p>
```
### [Example 390](https://spec.commonmark.org/0.29/#example-390)

This markdown:

```markdown
**foo bar **

```

Should give output:

```html
<p>**foo bar **</p>
```

But instead was:

```html
<p><strong>foo bar</strong></p>
```
### [Example 391](https://spec.commonmark.org/0.29/#example-391)

This markdown:

```markdown
**(**foo)

```

Should give output:

```html
<p>**(**foo)</p>
```

But instead was:

```html
<p><strong>(</strong>foo)</p>
```
### [Example 392](https://spec.commonmark.org/0.29/#example-392)

This markdown:

```markdown
*(**foo**)*

```

Should give output:

```html
<p><em>(<strong>foo</strong>)</em></p>
```

But instead was:

```html
<p><em>(</em><strong>foo</strong><em>)</em></p>
```
### [Example 393](https://spec.commonmark.org/0.29/#example-393)

This markdown:

```markdown
**Gomphocarpus (*Gomphocarpus physocarpus*, syn.
*Asclepias physocarpa*)**

```

Should give output:

```html
<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.<em>Asclepias physocarpa</em>)</strong></p>
```

But instead was:

```html
<p><strong>Gomphocarpus (</strong><strong>Gomphocarpus physocarpus</strong><strong>, syn.</strong><strong>Asclepias physocarpa</strong><strong>)</strong></p>
```
### [Example 394](https://spec.commonmark.org/0.29/#example-394)

This markdown:

```markdown
**foo "*bar*" foo**

```

Should give output:

```html
<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>
```

But instead was:

```html
<p><strong>foo &quot;</strong><strong>bar</strong><strong>&quot; foo</strong></p>
```
### [Example 398](https://spec.commonmark.org/0.29/#example-398)

This markdown:

```markdown
_(__foo__)_

```

Should give output:

```html
<p><em>(<strong>foo</strong>)</em></p>
```

But instead was:

```html
<p>_(__foo__)_</p>
```
### [Example 401](https://spec.commonmark.org/0.29/#example-401)

This markdown:

```markdown
__foo__bar__baz__

```

Should give output:

```html
<p><strong>foo__bar__baz</strong></p>
```

But instead was:

```html
<p>__foo__bar__baz__</p>
```
### [Example 402](https://spec.commonmark.org/0.29/#example-402)

This markdown:

```markdown
__(bar)__.

```

Should give output:

```html
<p><strong>(bar)</strong>.</p>
```

But instead was:

```html
<p>__(bar)__.</p>
```
### [Example 403](https://spec.commonmark.org/0.29/#example-403)

This markdown:

```markdown
*foo [bar](/url)*

```

Should give output:

```html
<p><em>foo<a href="/url">bar</a></em></p>
```

But instead was:

```html
<p><em>foo</em><a href="/url">bar</a></p>
```
### [Example 405](https://spec.commonmark.org/0.29/#example-405)

This markdown:

```markdown
_foo __bar__ baz_

```

Should give output:

```html
<p><em>foo<strong>bar</strong>baz</em></p>
```

But instead was:

```html
<p>_foo __bar__ baz_</p>
```
### [Example 406](https://spec.commonmark.org/0.29/#example-406)

This markdown:

```markdown
_foo _bar_ baz_

```

Should give output:

```html
<p><em>foo<em>bar</em>baz</em></p>
```

But instead was:

```html
<p>_foo _bar_ baz_</p>
```
### [Example 407](https://spec.commonmark.org/0.29/#example-407)

This markdown:

```markdown
__foo_ bar_

```

Should give output:

```html
<p><em><em>foo</em>bar</em></p>
```

But instead was:

```html
<p>__foo_ bar_</p>
```
### [Example 408](https://spec.commonmark.org/0.29/#example-408)

This markdown:

```markdown
*foo *bar**

```

Should give output:

```html
<p><em>foo<em>bar</em></em></p>
```

But instead was:

```html
<p><em>foo</em>bar</p>
```
### [Example 409](https://spec.commonmark.org/0.29/#example-409)

This markdown:

```markdown
*foo **bar** baz*

```

Should give output:

```html
<p><em>foo<strong>bar</strong>baz</em></p>
```

But instead was:

```html
<p><em>foo</em><strong>bar</strong><em>baz</em></p>
```
### [Example 410](https://spec.commonmark.org/0.29/#example-410)

This markdown:

```markdown
*foo**bar**baz*

```

Should give output:

```html
<p><em>foo<strong>bar</strong>baz</em></p>
```

But instead was:

```html
<p><em>foo</em><strong>bar</strong><em>baz</em></p>
```
### [Example 411](https://spec.commonmark.org/0.29/#example-411)

This markdown:

```markdown
*foo**bar*

```

Should give output:

```html
<p><em>foo**bar</em></p>
```

But instead was:

```html
<p><em>foo</em><strong>bar</strong></p>
```
### [Example 412](https://spec.commonmark.org/0.29/#example-412)

This markdown:

```markdown
***foo** bar*

```

Should give output:

```html
<p><em><strong>foo</strong>bar</em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 413](https://spec.commonmark.org/0.29/#example-413)

This markdown:

```markdown
*foo **bar***

```

Should give output:

```html
<p><em>foo<strong>bar</strong></em></p>
```

But instead was:

```html
<p><em>foo</em><strong>bar</strong></p>
```
### [Example 414](https://spec.commonmark.org/0.29/#example-414)

This markdown:

```markdown
*foo**bar***

```

Should give output:

```html
<p><em>foo<strong>bar</strong></em></p>
```

But instead was:

```html
<p><em>foo</em><strong>bar</strong></p>
```
### [Example 415](https://spec.commonmark.org/0.29/#example-415)

This markdown:

```markdown
foo***bar***baz

```

Should give output:

```html
<p>foo<em><strong>bar</strong></em>baz</p>
```

But instead was:

```html
<p>foo<strong>bar</strong>baz</p>
```
### [Example 416](https://spec.commonmark.org/0.29/#example-416)

This markdown:

```markdown
foo******bar*********baz

```

Should give output:

```html
<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>
```

But instead was:

```html
<p>foo<strong>bar</strong><strong>baz</strong></p>
```
### [Example 417](https://spec.commonmark.org/0.29/#example-417)

This markdown:

```markdown
*foo **bar *baz* bim** bop*

```

Should give output:

```html
<p><em>foo<strong>bar<em>baz</em>bim</strong>bop</em></p>
```

But instead was:

```html
<p><em>foo</em><strong>bar</strong><strong>baz</strong><strong>bim</strong><em>bop</em></p>
```
### [Example 418](https://spec.commonmark.org/0.29/#example-418)

This markdown:

```markdown
*foo [*bar*](/url)*

```

Should give output:

```html
<p><em>foo<a href="/url"><em>bar</em></a></em></p>
```

But instead was:

```html
<p><em>foo</em><a href="/url"><em>bar</em></a></p>
```
### [Example 419](https://spec.commonmark.org/0.29/#example-419)

This markdown:

```markdown
** is not an empty emphasis

```

Should give output:

```html
<p>** is not an empty emphasis</p>
```

But instead was:

```html
<p><strong>is not an empty emphasis</strong></p>
```
### [Example 420](https://spec.commonmark.org/0.29/#example-420)

This markdown:

```markdown
**** is not an empty strong emphasis

```

Should give output:

```html
<p>**** is not an empty strong emphasis</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 421](https://spec.commonmark.org/0.29/#example-421)

This markdown:

```markdown
**foo [bar](/url)**

```

Should give output:

```html
<p><strong>foo<a href="/url">bar</a></strong></p>
```

But instead was:

```html
<p><strong>foo</strong><a href="/url">bar</a></p>
```
### [Example 423](https://spec.commonmark.org/0.29/#example-423)

This markdown:

```markdown
__foo _bar_ baz__

```

Should give output:

```html
<p><strong>foo<em>bar</em>baz</strong></p>
```

But instead was:

```html
<p>__foo _bar_ baz__</p>
```
### [Example 424](https://spec.commonmark.org/0.29/#example-424)

This markdown:

```markdown
__foo __bar__ baz__

```

Should give output:

```html
<p><strong>foo<strong>bar</strong>baz</strong></p>
```

But instead was:

```html
<p>__foo __bar__ baz__</p>
```
### [Example 425](https://spec.commonmark.org/0.29/#example-425)

This markdown:

```markdown
____foo__ bar__

```

Should give output:

```html
<p><strong><strong>foo</strong>bar</strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 426](https://spec.commonmark.org/0.29/#example-426)

This markdown:

```markdown
**foo **bar****

```

Should give output:

```html
<p><strong>foo<strong>bar</strong></strong></p>
```

But instead was:

```html
<p><strong>foo</strong>bar</p>
```
### [Example 427](https://spec.commonmark.org/0.29/#example-427)

This markdown:

```markdown
**foo *bar* baz**

```

Should give output:

```html
<p><strong>foo<em>bar</em>baz</strong></p>
```

But instead was:

```html
<p><strong>foo</strong><strong>bar</strong><strong>baz</strong></p>
```
### [Example 428](https://spec.commonmark.org/0.29/#example-428)

This markdown:

```markdown
**foo*bar*baz**

```

Should give output:

```html
<p><strong>foo<em>bar</em>baz</strong></p>
```

But instead was:

```html
<p><strong>foo</strong><strong>bar</strong><strong>baz</strong></p>
```
### [Example 429](https://spec.commonmark.org/0.29/#example-429)

This markdown:

```markdown
***foo* bar**

```

Should give output:

```html
<p><strong><em>foo</em>bar</strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 430](https://spec.commonmark.org/0.29/#example-430)

This markdown:

```markdown
**foo *bar***

```

Should give output:

```html
<p><strong>foo<em>bar</em></strong></p>
```

But instead was:

```html
<p><strong>foo</strong><strong>bar</strong></p>
```
### [Example 431](https://spec.commonmark.org/0.29/#example-431)

This markdown:

```markdown
**foo *bar **baz**
bim* bop**

```

Should give output:

```html
<p><strong>foo<em>bar<strong>baz</strong>bim</em>bop</strong></p>
```

But instead was:

```html
<p><strong>foo</strong><strong>bar</strong><em>baz</em><strong>bim</strong><strong>bop</strong></p>
```
### [Example 432](https://spec.commonmark.org/0.29/#example-432)

This markdown:

```markdown
**foo [*bar*](/url)**

```

Should give output:

```html
<p><strong>foo<a href="/url"><em>bar</em></a></strong></p>
```

But instead was:

```html
<p><strong>foo</strong><a href="/url"><em>bar</em></a></p>
```
### [Example 434](https://spec.commonmark.org/0.29/#example-434)

This markdown:

```markdown
____ is not an empty strong emphasis

```

Should give output:

```html
<p>____ is not an empty strong emphasis</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 435](https://spec.commonmark.org/0.29/#example-435)

This markdown:

```markdown
foo ***

```

Should give output:

```html
<p>foo ***</p>
```

But instead was:

```html
<p>foo</p>
```
### [Example 436](https://spec.commonmark.org/0.29/#example-436)

This markdown:

```markdown
foo *\**

```

Should give output:

```html
<p>foo<em>*</em></p>
```

But instead was:

```html
<p>foo<em>\</em></p>
```
### [Example 438](https://spec.commonmark.org/0.29/#example-438)

This markdown:

```markdown
foo *****

```

Should give output:

```html
<p>foo *****</p>
```

But instead was:

```html
<p>foo</p>
```
### [Example 439](https://spec.commonmark.org/0.29/#example-439)

This markdown:

```markdown
foo **\***

```

Should give output:

```html
<p>foo<strong>*</strong></p>
```

But instead was:

```html
<p>foo<strong>\</strong></p>
```
### [Example 441](https://spec.commonmark.org/0.29/#example-441)

This markdown:

```markdown
**foo*

```

Should give output:

```html
<p>*<em>foo</em></p>
```

But instead was:

```html
<p><strong>foo</strong></p>
```
### [Example 442](https://spec.commonmark.org/0.29/#example-442)

This markdown:

```markdown
*foo**

```

Should give output:

```html
<p><em>foo</em>*</p>
```

But instead was:

```html
<p><em>foo</em></p>
```
### [Example 443](https://spec.commonmark.org/0.29/#example-443)

This markdown:

```markdown
***foo**

```

Should give output:

```html
<p>*<strong>foo</strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 444](https://spec.commonmark.org/0.29/#example-444)

This markdown:

```markdown
****foo*

```

Should give output:

```html
<p>***<em>foo</em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 445](https://spec.commonmark.org/0.29/#example-445)

This markdown:

```markdown
**foo***

```

Should give output:

```html
<p><strong>foo</strong>*</p>
```

But instead was:

```html
<p><strong>foo</strong></p>
```
### [Example 446](https://spec.commonmark.org/0.29/#example-446)

This markdown:

```markdown
*foo****

```

Should give output:

```html
<p><em>foo</em>***</p>
```

But instead was:

```html
<p><em>foo</em></p>
```
### [Example 448](https://spec.commonmark.org/0.29/#example-448)

This markdown:

```markdown
foo _\__

```

Should give output:

```html
<p>foo<em>_</em></p>
```

But instead was:

```html
<p>foo _\__</p>
```
### [Example 449](https://spec.commonmark.org/0.29/#example-449)

This markdown:

```markdown
foo _*_

```

Should give output:

```html
<p>foo<em>*</em></p>
```

But instead was:

```html
<p>foo _<em>_</em></p>
```
### [Example 451](https://spec.commonmark.org/0.29/#example-451)

This markdown:

```markdown
foo __\___

```

Should give output:

```html
<p>foo<strong>_</strong></p>
```

But instead was:

```html
<p>foo __\___</p>
```
### [Example 452](https://spec.commonmark.org/0.29/#example-452)

This markdown:

```markdown
foo __*__

```

Should give output:

```html
<p>foo<strong>*</strong></p>
```

But instead was:

```html
<p>foo __<em>__</em></p>
```
### [Example 453](https://spec.commonmark.org/0.29/#example-453)

This markdown:

```markdown
__foo_

```

Should give output:

```html
<p>_<em>foo</em></p>
```

But instead was:

```html
<p>__foo_</p>
```
### [Example 454](https://spec.commonmark.org/0.29/#example-454)

This markdown:

```markdown
_foo__

```

Should give output:

```html
<p><em>foo</em>_</p>
```

But instead was:

```html
<p>_foo__</p>
```
### [Example 455](https://spec.commonmark.org/0.29/#example-455)

This markdown:

```markdown
___foo__

```

Should give output:

```html
<p>_<strong>foo</strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 456](https://spec.commonmark.org/0.29/#example-456)

This markdown:

```markdown
____foo_

```

Should give output:

```html
<p>___<em>foo</em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 457](https://spec.commonmark.org/0.29/#example-457)

This markdown:

```markdown
__foo___

```

Should give output:

```html
<p><strong>foo</strong>_</p>
```

But instead was:

```html
<p>__foo___</p>
```
### [Example 458](https://spec.commonmark.org/0.29/#example-458)

This markdown:

```markdown
_foo____

```

Should give output:

```html
<p><em>foo</em>___</p>
```

But instead was:

```html
<p>_foo____</p>
```
### [Example 460](https://spec.commonmark.org/0.29/#example-460)

This markdown:

```markdown
*_foo_*

```

Should give output:

```html
<p><em><em>foo</em></em></p>
```

But instead was:

```html
<p><em>_foo_</em></p>
```
### [Example 461](https://spec.commonmark.org/0.29/#example-461)

This markdown:

```markdown
__foo__

```

Should give output:

```html
<p><strong>foo</strong></p>
```

But instead was:

```html
<p>__foo__</p>
```
### [Example 462](https://spec.commonmark.org/0.29/#example-462)

This markdown:

```markdown
_*foo*_

```

Should give output:

```html
<p><em><em>foo</em></em></p>
```

But instead was:

```html
<p>_<em>foo</em>_</p>
```
### [Example 463](https://spec.commonmark.org/0.29/#example-463)

This markdown:

```markdown
****foo****

```

Should give output:

```html
<p><strong><strong>foo</strong></strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 464](https://spec.commonmark.org/0.29/#example-464)

This markdown:

```markdown
____foo____

```

Should give output:

```html
<p><strong><strong>foo</strong></strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 465](https://spec.commonmark.org/0.29/#example-465)

This markdown:

```markdown
******foo******

```

Should give output:

```html
<p><strong><strong><strong>foo</strong></strong></strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 466](https://spec.commonmark.org/0.29/#example-466)

This markdown:

```markdown
***foo***

```

Should give output:

```html
<p><em><strong>foo</strong></em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 467](https://spec.commonmark.org/0.29/#example-467)

This markdown:

```markdown
_____foo_____

```

Should give output:

```html
<p><em><strong><strong>foo</strong></strong></em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
### [Example 469](https://spec.commonmark.org/0.29/#example-469)

This markdown:

```markdown
*foo __bar *baz bim__ bam*

```

Should give output:

```html
<p><em>foo<strong>bar *baz bim</strong>bam</em></p>
```

But instead was:

```html
<p><em>foo __bar</em>baz bim__ bam</p>
```
### [Example 470](https://spec.commonmark.org/0.29/#example-470)

This markdown:

```markdown
**foo **bar baz**

```

Should give output:

```html
<p>**foo<strong>bar baz</strong></p>
```

But instead was:

```html
<p><strong>foo</strong>bar baz</p>
```
### [Example 471](https://spec.commonmark.org/0.29/#example-471)

This markdown:

```markdown
*foo *bar baz*

```

Should give output:

```html
<p>*foo<em>bar baz</em></p>
```

But instead was:

```html
<p><em>foo</em>bar baz</p>
```
### [Example 472](https://spec.commonmark.org/0.29/#example-472)

This markdown:

```markdown
*[bar*](/url)

```

Should give output:

```html
<p>*<a href="/url">bar*</a></p>
```

But instead was:

```html
<p><a href="/url">bar</a></p>
```
### [Example 474](https://spec.commonmark.org/0.29/#example-474)

This markdown:

```markdown
*<img src="foo" title="*"/>

```

Should give output:

```html
<p>*<img src="foo" title="*"></p>
```

But instead was:

```html
<p><em>&lt;img src=&quot;foo&quot; title=&quot;</em>&quot;/&gt;</p>
```
### [Example 475](https://spec.commonmark.org/0.29/#example-475)

This markdown:

```markdown
**<a href="**">

```

Should give output:

```html
<p>**<a href="**"></p>
```

But instead was:

```html
<p><strong>&lt;a href=&quot;</strong>&quot;&gt;</p>
```
### [Example 476](https://spec.commonmark.org/0.29/#example-476)

This markdown:

```markdown
__<a href="__">

```

Should give output:

```html
<p>__<a href="__"></p>
```

But instead was:

```html
<p>__&lt;a href=&quot;__&quot;&gt;</p>
```
### [Example 477](https://spec.commonmark.org/0.29/#example-477)

This markdown:

```markdown
*a `*`*

```

Should give output:

```html
<p><em>a<code>*</code></em></p>
```

But instead was:

```html
<p><em>a</em></p>
```
### [Example 478](https://spec.commonmark.org/0.29/#example-478)

This markdown:

```markdown
_a `_`_

```

Should give output:

```html
<p><em>a<code>_</code></em></p>
```

But instead was:

```html
<p>_a<code>_</code>_</p>
```
### [Example 479](https://spec.commonmark.org/0.29/#example-479)

This markdown:

```markdown
**a<http://foo.bar/?q=**>

```

Should give output:

```html
<p>**a<a href="http://foo.bar/?q=**">http://foo.bar/?q=**</a></p>
```

But instead was:

```html
<p><strong>a&lt;http://foo.bar/?q=</strong>&gt;</p>
```
### [Example 480](https://spec.commonmark.org/0.29/#example-480)

This markdown:

```markdown
__a<http://foo.bar/?q=__>

```

Should give output:

```html
<p>__a<a href="http://foo.bar/?q=__">http://foo.bar/?q=__</a></p>
```

But instead was:

```html
<p>__a&lt;http://foo.bar/?q=__&gt;</p>
```
## Raw HTML

### [Example 609](https://spec.commonmark.org/0.29/#example-609)

This markdown:

```markdown
<a><bab><c2c>

```

Should give output:

```html
<p><a><bab><c2c></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 610](https://spec.commonmark.org/0.29/#example-610)

This markdown:

```markdown
<a/><b2/>

```

Should give output:

```html
<p><a><b2></p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 611](https://spec.commonmark.org/0.29/#example-611)

This markdown:

```markdown
<a  /><b2
data="foo" >

```

Should give output:

```html
<p><a><b2 data="foo"></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol
```
### [Example 612](https://spec.commonmark.org/0.29/#example-612)

This markdown:

```markdown
<a foo="bar" bam = 'baz <em>"</em>'
_boolean zoop:33=zoop:33 />

```

Should give output:

```html
<p><a _boolean="" bam="baz &lt;em&gt;&quot;&lt;/em&gt;" foo="bar" zoop:33="zoop:33"></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol =
```
### [Example 613](https://spec.commonmark.org/0.29/#example-613)

This markdown:

```markdown
Foo <responsive-image src="foo.jpg" />

```

Should give output:

```html
<p>Foo<responsive-image src="foo.jpg"></p>
```

But instead was:

```html
<p>Foo &lt;responsive-image src=&quot;foo.jpg&quot; /&gt;</p>
```
### [Example 614](https://spec.commonmark.org/0.29/#example-614)

This markdown:

```markdown
<33> <__>

```

Should give output:

```html
<p>&lt;33&gt; &lt;__&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 615](https://spec.commonmark.org/0.29/#example-615)

This markdown:

```markdown
<a h*#ref="hi">

```

Should give output:

```html
<p>&lt;a h*#ref=&quot;hi&quot;&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 616](https://spec.commonmark.org/0.29/#example-616)

This markdown:

```markdown
<a href="hi'> <a href=hi'>

```

Should give output:

```html
<p>&lt;a href=&quot;hi&#39;&gt; &lt;a href=hi&#39;&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol &quot;
```
### [Example 617](https://spec.commonmark.org/0.29/#example-617)

This markdown:

```markdown
< a><
foo><bar/ >
<foo bar=baz
bim!bop />

```

Should give output:

```html
<p>&lt; a&gt;&lt; foo&gt;&lt;bar/ &gt; &lt;foo bar=baz bim!bop /&gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
### [Example 618](https://spec.commonmark.org/0.29/#example-618)

This markdown:

```markdown
<a href='bar'title=title>

```

Should give output:

```html
<p>&lt;a href=&#39;bar&#39;title=title&gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol &quot; Problem at row 1 Expecting symbol &#39;
```
### [Example 619](https://spec.commonmark.org/0.29/#example-619)

This markdown:

```markdown
</a></foo >

```

Should give output:

```html
<p></a></foo></p>
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
### [Example 620](https://spec.commonmark.org/0.29/#example-620)

This markdown:

```markdown
</a href="foo">

```

Should give output:

```html
<p>&lt;/a href=&quot;foo&quot;&gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
### [Example 621](https://spec.commonmark.org/0.29/#example-621)

This markdown:

```markdown
foo <!-- this is a
comment - with hyphen -->

```

Should give output:

```html
<p>foo</p>
```

But instead was:

```html
<p>foo &lt;!-- this is a comment - with hyphen --&gt;</p>
```
### [Example 624](https://spec.commonmark.org/0.29/#example-624)

This markdown:

```markdown
foo <?php echo $a; ?>

```

Should give output:

```html
<p>foo</p>
```

But instead was:

```html
<p>foo &lt;?php echo $a; ?&gt;</p>
```
### [Example 625](https://spec.commonmark.org/0.29/#example-625)

This markdown:

```markdown
foo <!ELEMENT br EMPTY>

```

Should give output:

```html
<p>foo</p>
```

But instead was:

```html
<p>foo &lt;!ELEMENT br EMPTY&gt;</p>
```
### [Example 626](https://spec.commonmark.org/0.29/#example-626)

This markdown:

```markdown
foo <![CDATA[>&<]]>

```

Should give output:

```html
<p>foo&amp;&lt;]]&gt;</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
### [Example 627](https://spec.commonmark.org/0.29/#example-627)

This markdown:

```markdown
foo <a href="&ouml;">

```

Should give output:

```html
<p>foo<a href="ö"></p>
```

But instead was:

```html
<p>foo &lt;a href=&quot;&amp;ouml;&quot;&gt;</p>
```
### [Example 628](https://spec.commonmark.org/0.29/#example-628)

This markdown:

```markdown
foo <a href="\*">

```

Should give output:

```html
<p>foo<a href="\*"></p>
```

But instead was:

```html
<p>foo &lt;a href=&quot;\<em>&quot;&gt;</em></p>
```
### [Example 629](https://spec.commonmark.org/0.29/#example-629)

This markdown:

```markdown
<a href="\"">

```

Should give output:

```html
<p>&lt;a href=&quot;&quot;&quot;&gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## Link reference definitions

### [Example 161](https://spec.commonmark.org/0.29/#example-161)

This markdown:

```markdown
[foo]: /url "title"

[foo]

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 162](https://spec.commonmark.org/0.29/#example-162)

This markdown:

```markdown
   [foo]: 
      /url  
           'the title'  

[foo]

```

Should give output:

```html
<p><a href="/url" title="the title">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 163](https://spec.commonmark.org/0.29/#example-163)

This markdown:

```markdown
[Foo*bar\]]:my_(url) 'title (with parens)'

[Foo*bar\]]

```

Should give output:

```html
<p><a href="my_(url)" title="title (with parens)">Foo*bar]</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 164](https://spec.commonmark.org/0.29/#example-164)

This markdown:

```markdown
[Foo bar]:
<my url>
'title'

[Foo bar]

```

Should give output:

```html
<p><a href="my%20url" title="title">Foo bar</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol =
```
### [Example 165](https://spec.commonmark.org/0.29/#example-165)

This markdown:

```markdown
[foo]: /url '
title
line1
line2
'

[foo]

```

Should give output:

```html
<p><a href="/url" title="
title
line1
line2
">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 14 Expecting Problem at row 1 Expecting symbol (
```
### [Example 166](https://spec.commonmark.org/0.29/#example-166)

This markdown:

```markdown
[foo]: /url 'title

with blank line'

[foo]

```

Should give output:

```html
<p>[foo]: /url &#39;title</p><p>with blank line&#39;</p><p>[foo]</p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
### [Example 167](https://spec.commonmark.org/0.29/#example-167)

This markdown:

```markdown
[foo]:
/url

[foo]

```

Should give output:

```html
<p><a href="/url">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
### [Example 168](https://spec.commonmark.org/0.29/#example-168)

This markdown:

```markdown
[foo]:

[foo]

```

Should give output:

```html
<p>[foo]:</p><p>[foo]</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 169](https://spec.commonmark.org/0.29/#example-169)

This markdown:

```markdown
[foo]: <>

[foo]

```

Should give output:

```html
<p><a href="">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 170](https://spec.commonmark.org/0.29/#example-170)

This markdown:

```markdown
[foo]: <bar>(baz)

[foo]

```

Should give output:

```html
<p>[foo]:<bar>(baz)</p><p>[foo]</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 171](https://spec.commonmark.org/0.29/#example-171)

This markdown:

```markdown
[foo]: /url\bar\*baz "foo\"bar\baz"

[foo]

```

Should give output:

```html
<p><a href="/url%5Cbar*baz" title="foo&quot;bar\baz">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 172](https://spec.commonmark.org/0.29/#example-172)

This markdown:

```markdown
[foo]

[foo]: url

```

Should give output:

```html
<p><a href="url">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 173](https://spec.commonmark.org/0.29/#example-173)

This markdown:

```markdown
[foo]

[foo]: first
[foo]: second

```

Should give output:

```html
<p><a href="first">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
### [Example 174](https://spec.commonmark.org/0.29/#example-174)

This markdown:

```markdown
[FOO]: /url

[Foo]

```

Should give output:

```html
<p><a href="/url">Foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 175](https://spec.commonmark.org/0.29/#example-175)

This markdown:

```markdown
[ΑΓΩ]: /φου

[αγω]

```

Should give output:

```html
<p><a href="/%CF%86%CE%BF%CF%85">αγω</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 176](https://spec.commonmark.org/0.29/#example-176)

This markdown:

```markdown
[foo]: /url

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
### [Example 177](https://spec.commonmark.org/0.29/#example-177)

This markdown:

```markdown
[
foo
]: /url
bar

```

Should give output:

```html
<p>bar</p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
### [Example 178](https://spec.commonmark.org/0.29/#example-178)

This markdown:

```markdown
[foo]: /url "title" ok

```

Should give output:

```html
<p>[foo]: /url &quot;title&quot; ok</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
### [Example 179](https://spec.commonmark.org/0.29/#example-179)

This markdown:

```markdown
[foo]: /url
"title" ok

```

Should give output:

```html
<p>&quot;title&quot; ok</p>
```

But instead was:

```html
ERROR Problem at row 5 Expecting Problem at row 1 Expecting symbol (
```
### [Example 180](https://spec.commonmark.org/0.29/#example-180)

This markdown:

```markdown
    [foo]: /url "title"

[foo]

```

Should give output:

```html
<pre><code>[foo]: /url &quot;title&quot;</code></pre><p>[foo]</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 181](https://spec.commonmark.org/0.29/#example-181)

This markdown:

```markdown
```
[foo]: /url
```

[foo]

```

Should give output:

```html
<pre><code>[foo]: /url</code></pre><p>[foo]</p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
### [Example 182](https://spec.commonmark.org/0.29/#example-182)

This markdown:

```markdown
Foo
[bar]: /baz

[bar]

```

Should give output:

```html
<p>Foo [bar]: /baz</p><p>[bar]</p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
### [Example 183](https://spec.commonmark.org/0.29/#example-183)

This markdown:

```markdown
# [Foo]
[foo]: /url
> bar

```

Should give output:

```html
<h1><a href="/url">Foo</a></h1><blockquote><p>bar</p></blockquote>
```

But instead was:

```html
ERROR Problem at row 7 Expecting Problem at row 1 Expecting symbol (
```
### [Example 184](https://spec.commonmark.org/0.29/#example-184)

This markdown:

```markdown
[foo]: /url
bar
===
[foo]

```

Should give output:

```html
<h1>bar</h1><p><a href="/url">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
### [Example 185](https://spec.commonmark.org/0.29/#example-185)

This markdown:

```markdown
[foo]: /url
===
[foo]

```

Should give output:

```html
<p>===<a href="/url">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 7 Expecting Problem at row 1 Expecting symbol (
```
### [Example 186](https://spec.commonmark.org/0.29/#example-186)

This markdown:

```markdown
[foo]: /foo-url "foo"
[bar]: /bar-url
  "bar"
[baz]: /baz-url

[foo],
[bar],
[baz]

```

Should give output:

```html
<p><a href="/foo-url" title="foo">foo</a>,<a href="/bar-url" title="bar">bar</a>,<a href="/baz-url">baz</a></p>
```

But instead was:

```html
ERROR Problem at row 5 Expecting --- Problem at row 5 Expecting *** Problem at row 5 Expecting ___
```
### [Example 187](https://spec.commonmark.org/0.29/#example-187)

This markdown:

```markdown
[foo]

> [foo]: /url

```

Should give output:

```html
<p><a href="/url">foo</a></p><blockquote></blockquote>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 188](https://spec.commonmark.org/0.29/#example-188)

This markdown:

```markdown
[foo]: /url

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
## Fenced code blocks

### [Example 91](https://spec.commonmark.org/0.29/#example-91)

This markdown:

```markdown
``
foo
``

```

Should give output:

```html
<p><code>foo</code></p>
```

But instead was:

```html
<p>foo</p>
```
### [Example 94](https://spec.commonmark.org/0.29/#example-94)

This markdown:

```markdown
````
aaa
```
``````

```

Should give output:

```html
<pre><code>aaa ```</code></pre>
```

But instead was:

```html
ERROR Problem at row 8 Expecting symbol ```
```
### [Example 95](https://spec.commonmark.org/0.29/#example-95)

This markdown:

```markdown
~~~~
aaa
~~~
~~~~

```

Should give output:

```html
<pre><code>aaa ~~~</code></pre>
```

But instead was:

```html
ERROR Problem at row 8 Expecting symbol ~~~
```
### [Example 96](https://spec.commonmark.org/0.29/#example-96)

This markdown:

```markdown
```

```

Should give output:

```html
<pre><code></code></pre>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol ```
```
### [Example 97](https://spec.commonmark.org/0.29/#example-97)

This markdown:

```markdown
`````

```
aaa

```

Should give output:

```html
<pre><code>``` aaa</code></pre>
```

But instead was:

```html
<pre><code></code></pre><p>aaa</p>
```
### [Example 98](https://spec.commonmark.org/0.29/#example-98)

This markdown:

```markdown
> ```
> aaa

bbb

```

Should give output:

```html
<blockquote><pre><code>aaa</code></pre></blockquote><p>bbb</p>
```

But instead was:

```html
<p>&gt;<code>&gt; aaa</code></p><p>bbb</p>
```
### [Example 100](https://spec.commonmark.org/0.29/#example-100)

This markdown:

```markdown
```
```

```

Should give output:

```html
<pre><code></code></pre>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol ```
```
### [Example 101](https://spec.commonmark.org/0.29/#example-101)

This markdown:

```markdown
 ```
 aaa
aaa
```

```

Should give output:

```html
<pre><code>aaa aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 102](https://spec.commonmark.org/0.29/#example-102)

This markdown:

```markdown
  ```
aaa
  aaa
aaa
  ```

```

Should give output:

```html
<pre><code>aaa aaa aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 103](https://spec.commonmark.org/0.29/#example-103)

This markdown:

```markdown
   ```
   aaa
    aaa
  aaa
   ```

```

Should give output:

```html
<pre><code>aaa aaa aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 104](https://spec.commonmark.org/0.29/#example-104)

This markdown:

```markdown
    ```
    aaa
    ```

```

Should give output:

```html
<pre><code>``` aaa ```</code></pre>
```

But instead was:

```html
<pre><code>```</code></pre><pre><code>aaa</code></pre><pre><code>```</code></pre>
```
### [Example 105](https://spec.commonmark.org/0.29/#example-105)

This markdown:

```markdown
```
aaa
  ```

```

Should give output:

```html
<pre><code>aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol ```
```
### [Example 106](https://spec.commonmark.org/0.29/#example-106)

This markdown:

```markdown
   ```
aaa
  ```

```

Should give output:

```html
<pre><code>aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 107](https://spec.commonmark.org/0.29/#example-107)

This markdown:

```markdown
```
aaa
    ```

```

Should give output:

```html
<pre><code>aaa ```</code></pre>
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol ```
```
### [Example 108](https://spec.commonmark.org/0.29/#example-108)

This markdown:

```markdown
``` ```
aaa

```

Should give output:

```html
<p><code></code>aaa</p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol ```
```
### [Example 109](https://spec.commonmark.org/0.29/#example-109)

This markdown:

```markdown
~~~~~~
aaa
~~~ ~~

```

Should give output:

```html
<pre><code>aaa ~~~ ~~</code></pre>
```

But instead was:

```html
ERROR Problem at row 5 Expecting --- Problem at row 5 Expecting *** Problem at row 5 Expecting ___
```
### [Example 111](https://spec.commonmark.org/0.29/#example-111)

This markdown:

```markdown
foo
---
~~~
bar
~~~
# baz

```

Should give output:

```html
<h2>foo</h2><pre><code>bar</code></pre><h1>baz</h1>
```

But instead was:

```html
<p>foo</p><hr><pre><code>bar</code></pre><h1>baz</h1>
```
### [Example 112](https://spec.commonmark.org/0.29/#example-112)

This markdown:

```markdown
```ruby
def foo(x)
  return 3
end
```

```

Should give output:

```html
<pre><code class="language-ruby">def foo(x) return 3 end</code></pre>
```

But instead was:

```html
<pre><code>def foo(x) return 3 end</code></pre>
```
### [Example 113](https://spec.commonmark.org/0.29/#example-113)

This markdown:

```markdown
~~~~    ruby startline=3 $%@#$
def foo(x)
  return 3
end
~~~~~~~

```

Should give output:

```html
<pre><code class="language-ruby">def foo(x) return 3 end</code></pre>
```

But instead was:

```html
ERROR Problem at row 9 Expecting symbol ~~~
```
### [Example 114](https://spec.commonmark.org/0.29/#example-114)

This markdown:

```markdown
````;
````

```

Should give output:

```html
<pre><code class="language-;"></code></pre>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol ```
```
### [Example 115](https://spec.commonmark.org/0.29/#example-115)

This markdown:

```markdown
``` aa ```
foo

```

Should give output:

```html
<p><code>aa</code>foo</p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol ```
```
### [Example 116](https://spec.commonmark.org/0.29/#example-116)

This markdown:

```markdown
~~~ aa ``` ~~~
foo
~~~

```

Should give output:

```html
<pre><code class="language-aa">foo</code></pre>
```

But instead was:

```html
<pre><code>foo</code></pre>
```
## Entity and numeric character references

### [Example 311](https://spec.commonmark.org/0.29/#example-311)

This markdown:

```markdown
&nbsp; &amp; &copy; &AElig; &Dcaron;
&frac34; &HilbertSpace; &DifferentialD;
&ClockwiseContourIntegral; &ngE;

```

Should give output:

```html
<p>&amp; © Æ Ď ¾ ℋ ⅆ ∲ ≧̸</p>
```

But instead was:

```html
<p>&amp;nbsp; &amp;amp; &amp;copy; &amp;AElig; &amp;Dcaron; &amp;frac34; &amp;HilbertSpace; &amp;DifferentialD; &amp;ClockwiseContourIntegral; &amp;ngE;</p>
```
### [Example 312](https://spec.commonmark.org/0.29/#example-312)

This markdown:

```markdown
&#35; &#1234; &#992; &#0;

```

Should give output:

```html
<p># Ӓ Ϡ �</p>
```

But instead was:

```html
<p>&amp;#35; &amp;#1234; &amp;#992; &amp;#0;</p>
```
### [Example 313](https://spec.commonmark.org/0.29/#example-313)

This markdown:

```markdown
&#X22; &#XD06; &#xcab;

```

Should give output:

```html
<p>&quot; ആ ಫ</p>
```

But instead was:

```html
<p>&amp;#X22; &amp;#XD06; &amp;#xcab;</p>
```
### [Example 317](https://spec.commonmark.org/0.29/#example-317)

This markdown:

```markdown
<a href="&ouml;&ouml;.html">

```

Should give output:

```html
<a href="öö.html">
```

But instead was:

```html
ERROR Problem at row 1 No entity named &quot;ö&quot; found.
```
### [Example 318](https://spec.commonmark.org/0.29/#example-318)

This markdown:

```markdown
[foo](/f&ouml;&ouml; "f&ouml;&ouml;")

```

Should give output:

```html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>
```

But instead was:

```html
<p><a f&ouml;&ouml;\""="" href="/föö \">foo</a></p>
```
### [Example 319](https://spec.commonmark.org/0.29/#example-319)

This markdown:

```markdown
[foo]

[foo]: /f&ouml;&ouml; "f&ouml;&ouml;"

```

Should give output:

```html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 320](https://spec.commonmark.org/0.29/#example-320)

This markdown:

```markdown
``` f&ouml;&ouml;
foo
```

```

Should give output:

```html
<pre><code class="language-föö">foo</code></pre>
```

But instead was:

```html
<pre><code>foo</code></pre>
```
### [Example 323](https://spec.commonmark.org/0.29/#example-323)

This markdown:

```markdown
&#42;foo&#42;
*foo*

```

Should give output:

```html
<p>*foo*<em>foo</em></p>
```

But instead was:

```html
<p>&amp;#42;foo&amp;#42;<em>foo</em></p>
```
### [Example 324](https://spec.commonmark.org/0.29/#example-324)

This markdown:

```markdown
&#42; foo

* foo

```

Should give output:

```html
<p>* foo</p><ul><li>foo</li></ul>
```

But instead was:

```html
<p>&amp;#42; foo</p><p><em>foo</em></p>
```
### [Example 325](https://spec.commonmark.org/0.29/#example-325)

This markdown:

```markdown
foo&#10;&#10;bar

```

Should give output:

```html
<p>foo bar</p>
```

But instead was:

```html
<p>foo&amp;#10;&amp;#10;bar</p>
```
### [Example 326](https://spec.commonmark.org/0.29/#example-326)

This markdown:

```markdown
&#9;foo

```

Should give output:

```html
<p>foo</p>
```

But instead was:

```html
<p>&amp;#9;foo</p>
```
### [Example 327](https://spec.commonmark.org/0.29/#example-327)

This markdown:

```markdown
[a](url &quot;tit&quot;)

```

Should give output:

```html
<p>[a](url &quot;tit&quot;)</p>
```

But instead was:

```html
<p><a href="url &quot;tit&quot;">a</a></p>
```
## Thematic breaks

### [Example 16](https://spec.commonmark.org/0.29/#example-16)

This markdown:

```markdown
--
**
__

```

Should give output:

```html
<p>-- ** __</p>
```

But instead was:

```html
<ul><li><p>-</p></li></ul><p><strong>__</strong></p>
```
### [Example 19](https://spec.commonmark.org/0.29/#example-19)

This markdown:

```markdown
Foo
    ***

```

Should give output:

```html
<p>Foo ***</p>
```

But instead was:

```html
<p>Foo</p><pre><code>***</code></pre>
```
### [Example 21](https://spec.commonmark.org/0.29/#example-21)

This markdown:

```markdown
 - - -

```

Should give output:

```html
<hr>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 22](https://spec.commonmark.org/0.29/#example-22)

This markdown:

```markdown
 **  * ** * ** * **

```

Should give output:

```html
<hr>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 23](https://spec.commonmark.org/0.29/#example-23)

This markdown:

```markdown
-     -      -      -

```

Should give output:

```html
<hr>
```

But instead was:

```html
<ul><li><p>- - -</p></li></ul>
```
### [Example 24](https://spec.commonmark.org/0.29/#example-24)

This markdown:

```markdown
- - - -    

```

Should give output:

```html
<hr>
```

But instead was:

```html
<ul><li><p>- - -</p></li></ul>
```
### [Example 25](https://spec.commonmark.org/0.29/#example-25)

This markdown:

```markdown
_ _ _ _ a

a------

---a---

```

Should give output:

```html
<p>_ _ _ _ a</p><p>a------</p><p>---a---</p>
```

But instead was:

```html
ERROR Problem at row 7 Expecting end Problem at row 7 Expecting newline
```
### [Example 26](https://spec.commonmark.org/0.29/#example-26)

This markdown:

```markdown
 *-*

```

Should give output:

```html
<p><em>-</em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 27](https://spec.commonmark.org/0.29/#example-27)

This markdown:

```markdown
- foo
***
- bar

```

Should give output:

```html
<ul><li>foo</li></ul><hr><ul><li>bar</li></ul>
```

But instead was:

```html
<ul><li><p>foo</p></li></ul><hr><ul><li><p>bar</p></li></ul>
```
### [Example 29](https://spec.commonmark.org/0.29/#example-29)

This markdown:

```markdown
Foo
---
bar

```

Should give output:

```html
<h2>Foo</h2><p>bar</p>
```

But instead was:

```html
<p>Foo</p><hr><p>bar</p>
```
### [Example 30](https://spec.commonmark.org/0.29/#example-30)

This markdown:

```markdown
* Foo
* * *
* Bar

```

Should give output:

```html
<ul><li>Foo</li></ul><hr><ul><li>Bar</li></ul>
```

But instead was:

```html
<p><em>Foo</em><em></em><em>Bar</em></p>
```
### [Example 31](https://spec.commonmark.org/0.29/#example-31)

This markdown:

```markdown
- Foo
- * * *

```

Should give output:

```html
<ul><li>Foo</li><li><hr></li></ul>
```

But instead was:

```html
<ul><li><p>Foo</p></li><li><p><em></em></p></li></ul>
```
## Hard line breaks

### [Example 630](https://spec.commonmark.org/0.29/#example-630)

This markdown:

```markdown
foo  
baz

```

Should give output:

```html
<p>foo<br>baz</p>
```

But instead was:

```html
<p>foo baz</p>
```
### [Example 631](https://spec.commonmark.org/0.29/#example-631)

This markdown:

```markdown
foo\
baz

```

Should give output:

```html
<p>foo<br>baz</p>
```

But instead was:

```html
<p>foo\ baz</p>
```
### [Example 632](https://spec.commonmark.org/0.29/#example-632)

This markdown:

```markdown
foo       
baz

```

Should give output:

```html
<p>foo<br>baz</p>
```

But instead was:

```html
<p>foo baz</p>
```
### [Example 633](https://spec.commonmark.org/0.29/#example-633)

This markdown:

```markdown
foo  
     bar

```

Should give output:

```html
<p>foo<br>bar</p>
```

But instead was:

```html
<p>foo</p><pre><code>bar</code></pre>
```
### [Example 634](https://spec.commonmark.org/0.29/#example-634)

This markdown:

```markdown
foo\
     bar

```

Should give output:

```html
<p>foo<br>bar</p>
```

But instead was:

```html
<p>foo\</p><pre><code>bar</code></pre>
```
### [Example 635](https://spec.commonmark.org/0.29/#example-635)

This markdown:

```markdown
*foo  
bar*

```

Should give output:

```html
<p><em>foo<br>bar</em></p>
```

But instead was:

```html
<p><em>foo bar</em></p>
```
### [Example 636](https://spec.commonmark.org/0.29/#example-636)

This markdown:

```markdown
*foo\
bar*

```

Should give output:

```html
<p><em>foo<br>bar</em></p>
```

But instead was:

```html
<p><em>foo\ bar</em></p>
```
### [Example 639](https://spec.commonmark.org/0.29/#example-639)

This markdown:

```markdown
<a href="foo  
bar">

```

Should give output:

```html
<p><a href="foo  
bar"></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol
```
### [Example 640](https://spec.commonmark.org/0.29/#example-640)

This markdown:

```markdown
<a href="foo\
bar">

```

Should give output:

```html
<p><a href="foo\
bar"></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol
```
## Paragraphs

### [Example 192](https://spec.commonmark.org/0.29/#example-192)

This markdown:

```markdown
  aaa
 bbb

```

Should give output:

```html
<p>aaa bbb</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 193](https://spec.commonmark.org/0.29/#example-193)

This markdown:

```markdown
aaa
             bbb
                                       ccc

```

Should give output:

```html
<p>aaa bbb ccc</p>
```

But instead was:

```html
<p>aaa</p><pre><code>bbb</code></pre><pre><code>ccc</code></pre>
```
### [Example 194](https://spec.commonmark.org/0.29/#example-194)

This markdown:

```markdown
   aaa
bbb

```

Should give output:

```html
<p>aaa bbb</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 196](https://spec.commonmark.org/0.29/#example-196)

This markdown:

```markdown
aaa     
bbb     

```

Should give output:

```html
<p>aaa<br>bbb</p>
```

But instead was:

```html
<p>aaa bbb</p>
```
## Images

### [Example 568](https://spec.commonmark.org/0.29/#example-568)

This markdown:

```markdown
![foo](/url "title")

```

Should give output:

```html
<p><img alt="foo" src="/url" title="title"></p>
```

But instead was:

```html
<p><img src="/url \" title\""=""></p>
```
### [Example 569](https://spec.commonmark.org/0.29/#example-569)

This markdown:

```markdown
![foo *bar*]

[foo *bar*]: train.jpg "train & tracks"

```

Should give output:

```html
<p><img alt="foo bar" src="train.jpg" title="train &amp; tracks"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 570](https://spec.commonmark.org/0.29/#example-570)

This markdown:

```markdown
![foo ![bar](/url)](/url2)

```

Should give output:

```html
<p><img alt="foo bar" src="/url2"></p>
```

But instead was:

```html
<p><img src="/url">](/url2)</p>
```
### [Example 571](https://spec.commonmark.org/0.29/#example-571)

This markdown:

```markdown
![foo [bar](/url)](/url2)

```

Should give output:

```html
<p><img alt="foo bar" src="/url2"></p>
```

But instead was:

```html
<p><img src="/url">](/url2)</p>
```
### [Example 572](https://spec.commonmark.org/0.29/#example-572)

This markdown:

```markdown
![foo *bar*][]

[foo *bar*]: train.jpg "train & tracks"

```

Should give output:

```html
<p><img alt="foo bar" src="train.jpg" title="train &amp; tracks"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 573](https://spec.commonmark.org/0.29/#example-573)

This markdown:

```markdown
![foo *bar*][foobar]

[FOOBAR]: train.jpg "train & tracks"

```

Should give output:

```html
<p><img alt="foo bar" src="train.jpg" title="train &amp; tracks"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 574](https://spec.commonmark.org/0.29/#example-574)

This markdown:

```markdown
![foo](train.jpg)

```

Should give output:

```html
<p><img alt="foo" src="train.jpg"></p>
```

But instead was:

```html
<p><img src="train.jpg"></p>
```
### [Example 575](https://spec.commonmark.org/0.29/#example-575)

This markdown:

```markdown
My ![foo bar](/path/to/train.jpg  "title"   )

```

Should give output:

```html
<p>My<img alt="foo bar" src="/path/to/train.jpg" title="title"></p>
```

But instead was:

```html
<p>My<img "="" src="/path/to/train.jpg  \" title\"=""></p>
```
### [Example 576](https://spec.commonmark.org/0.29/#example-576)

This markdown:

```markdown
![foo](<url>)

```

Should give output:

```html
<p><img alt="foo" src="url"></p>
```

But instead was:

```html
<p><img src="&lt;url&gt;"></p>
```
### [Example 577](https://spec.commonmark.org/0.29/#example-577)

This markdown:

```markdown
![](/url)

```

Should give output:

```html
<p><img alt="" src="/url"></p>
```

But instead was:

```html

```
### [Example 578](https://spec.commonmark.org/0.29/#example-578)

This markdown:

```markdown
![foo][bar]

[bar]: /url

```

Should give output:

```html
<p><img alt="foo" src="/url"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 579](https://spec.commonmark.org/0.29/#example-579)

This markdown:

```markdown
![foo][bar]

[BAR]: /url

```

Should give output:

```html
<p><img alt="foo" src="/url"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 580](https://spec.commonmark.org/0.29/#example-580)

This markdown:

```markdown
![foo][]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="foo" src="/url" title="title"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 581](https://spec.commonmark.org/0.29/#example-581)

This markdown:

```markdown
![*foo* bar][]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p><img alt="foo bar" src="/url" title="title"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 582](https://spec.commonmark.org/0.29/#example-582)

This markdown:

```markdown
![Foo][]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="Foo" src="/url" title="title"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 583](https://spec.commonmark.org/0.29/#example-583)

This markdown:

```markdown
![foo] 
[]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="foo" src="/url" title="title">[]</p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
### [Example 584](https://spec.commonmark.org/0.29/#example-584)

This markdown:

```markdown
![foo]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="foo" src="/url" title="title"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 585](https://spec.commonmark.org/0.29/#example-585)

This markdown:

```markdown
![*foo* bar]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p><img alt="foo bar" src="/url" title="title"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 586](https://spec.commonmark.org/0.29/#example-586)

This markdown:

```markdown
![[foo]]

[[foo]]: /url "title"

```

Should give output:

```html
<p>![[foo]]</p><p>[[foo]]: /url &quot;title&quot;</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 587](https://spec.commonmark.org/0.29/#example-587)

This markdown:

```markdown
![Foo]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="Foo" src="/url" title="title"></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 588](https://spec.commonmark.org/0.29/#example-588)

This markdown:

```markdown
!\[foo]

[foo]: /url "title"

```

Should give output:

```html
<p>![foo]</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
### [Example 589](https://spec.commonmark.org/0.29/#example-589)

This markdown:

```markdown
\![foo]

[foo]: /url "title"

```

Should give output:

```html
<p>!<a href="/url" title="title">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## Autolinks

### [Example 590](https://spec.commonmark.org/0.29/#example-590)

This markdown:

```markdown
<http://foo.bar.baz>

```

Should give output:

```html
<p><a href="http://foo.bar.baz">http://foo.bar.baz</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 591](https://spec.commonmark.org/0.29/#example-591)

This markdown:

```markdown
<http://foo.bar.baz/test?q=hello&id=22&boolean>

```

Should give output:

```html
<p><a href="http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 592](https://spec.commonmark.org/0.29/#example-592)

This markdown:

```markdown
<irc://foo.bar:2233/baz>

```

Should give output:

```html
<p><a href="irc://foo.bar:2233/baz">irc://foo.bar:2233/baz</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 593](https://spec.commonmark.org/0.29/#example-593)

This markdown:

```markdown
<MAILTO:FOO@BAR.BAZ>

```

Should give output:

```html
<p><a href="MAILTO:FOO@BAR.BAZ">MAILTO:FOO@BAR.BAZ</a></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 594](https://spec.commonmark.org/0.29/#example-594)

This markdown:

```markdown
<a+b+c:d>

```

Should give output:

```html
<p><a href="a+b+c:d">a+b+c:d</a></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 595](https://spec.commonmark.org/0.29/#example-595)

This markdown:

```markdown
<made-up-scheme://foo,bar>

```

Should give output:

```html
<p><a href="made-up-scheme://foo,bar">made-up-scheme://foo,bar</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 596](https://spec.commonmark.org/0.29/#example-596)

This markdown:

```markdown
<http://../>

```

Should give output:

```html
<p><a href="http://../">http://../</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 597](https://spec.commonmark.org/0.29/#example-597)

This markdown:

```markdown
<localhost:5001/foo>

```

Should give output:

```html
<p><a href="localhost:5001/foo">localhost:5001/foo</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 598](https://spec.commonmark.org/0.29/#example-598)

This markdown:

```markdown
<http://foo.bar/baz bim>

```

Should give output:

```html
<p>&lt;http://foo.bar/baz bim&gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 599](https://spec.commonmark.org/0.29/#example-599)

This markdown:

```markdown
<http://example.com/\[\>

```

Should give output:

```html
<p><a href="http://example.com/%5C%5B%5C">http://example.com/\[\</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
### [Example 600](https://spec.commonmark.org/0.29/#example-600)

This markdown:

```markdown
<foo@bar.example.com>

```

Should give output:

```html
<p><a href="mailto:foo@bar.example.com">foo@bar.example.com</a></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 601](https://spec.commonmark.org/0.29/#example-601)

This markdown:

```markdown
<foo+special@Bar.baz-bar0.com>

```

Should give output:

```html
<p><a href="mailto:foo+special@Bar.baz-bar0.com">foo+special@Bar.baz-bar0.com</a></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 602](https://spec.commonmark.org/0.29/#example-602)

This markdown:

```markdown
<foo\+@bar.example.com>

```

Should give output:

```html
<p>&lt;foo+@bar.example.com&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 603](https://spec.commonmark.org/0.29/#example-603)

This markdown:

```markdown
<>

```

Should give output:

```html
<p>&lt;&gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
### [Example 604](https://spec.commonmark.org/0.29/#example-604)

This markdown:

```markdown
< http://foo.bar >

```

Should give output:

```html
<p>&lt; http://foo.bar &gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
### [Example 605](https://spec.commonmark.org/0.29/#example-605)

This markdown:

```markdown
<m:abc>

```

Should give output:

```html
<p>&lt;m:abc&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
### [Example 606](https://spec.commonmark.org/0.29/#example-606)

This markdown:

```markdown
<foo.bar.baz>

```

Should give output:

```html
<p>&lt;foo.bar.baz&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
## HTML blocks

### [Example 118](https://spec.commonmark.org/0.29/#example-118)

This markdown:

```markdown
<table><tr><td>
<pre>
**Hello**,

_world_.
</pre>
</td></tr></table>

```

Should give output:

```html
<table><tr><td><pre>**Hello**,<p><em>world</em>.</pre></p></td></tr></table>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 119](https://spec.commonmark.org/0.29/#example-119)

This markdown:

```markdown
<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>

okay.

```

Should give output:

```html
<table><tr><td>hi</td></tr></table><p>okay.</p>
```

But instead was:

```html
ERROR Problem at row 7 Expecting Problem at row 2 Expecting --- Problem at row 2 Expecting *** Problem at row 2 Expecting ___
```
### [Example 120](https://spec.commonmark.org/0.29/#example-120)

This markdown:

```markdown
 <div>
  *hello*
         <foo><a>

```

Should give output:

```html
<div>*hello*<foo><a>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 121](https://spec.commonmark.org/0.29/#example-121)

This markdown:

```markdown
</div>
*foo*

```

Should give output:

```html
</div>*foo*
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
### [Example 122](https://spec.commonmark.org/0.29/#example-122)

This markdown:

```markdown
<DIV CLASS="foo">

*Markdown*

</DIV>

```

Should give output:

```html
<div class="foo"><p><em>Markdown</em></p></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 123](https://spec.commonmark.org/0.29/#example-123)

This markdown:

```markdown
<div id="foo"
  class="bar">
</div>

```

Should give output:

```html
<div class="bar" id="foo"></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 124](https://spec.commonmark.org/0.29/#example-124)

This markdown:

```markdown
<div id="foo" class="bar
  baz">
</div>

```

Should give output:

```html
<div class="bar
 baz" id="foo"></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 125](https://spec.commonmark.org/0.29/#example-125)

This markdown:

```markdown
<div>
*foo*

*bar*

```

Should give output:

```html
<div>*foo*<p><em>bar</em></p>
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol
```
### [Example 126](https://spec.commonmark.org/0.29/#example-126)

This markdown:

```markdown
<div id="foo"
*hi*

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol =
```
### [Example 127](https://spec.commonmark.org/0.29/#example-127)

This markdown:

```markdown
<div class
foo

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol =
```
### [Example 128](https://spec.commonmark.org/0.29/#example-128)

This markdown:

```markdown
<div *???-&&&-<---
*foo*

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol =
```
### [Example 129](https://spec.commonmark.org/0.29/#example-129)

This markdown:

```markdown
<div><a href="bar">*foo*</a></div>

```

Should give output:

```html
<div><a href="bar">*foo*</a></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 130](https://spec.commonmark.org/0.29/#example-130)

This markdown:

```markdown
<table><tr><td>
foo
</td></tr></table>

```

Should give output:

```html
<table><tr><td>foo</td></tr></table>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 131](https://spec.commonmark.org/0.29/#example-131)

This markdown:

```markdown
<div></div>
``` c
int x = 33;
```

```

Should give output:

```html
<div></div>``` c int x = 33; ```
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 132](https://spec.commonmark.org/0.29/#example-132)

This markdown:

```markdown
<a href="foo">
*bar*
</a>

```

Should give output:

```html
<a href="foo">*bar*</a>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 133](https://spec.commonmark.org/0.29/#example-133)

This markdown:

```markdown
<Warning>
*bar*
</Warning>

```

Should give output:

```html
<warning>*bar*</warning>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 134](https://spec.commonmark.org/0.29/#example-134)

This markdown:

```markdown
<i class="foo">
*bar*
</i>

```

Should give output:

```html
<i class="foo">*bar*</i>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 135](https://spec.commonmark.org/0.29/#example-135)

This markdown:

```markdown
</ins>
*bar*

```

Should give output:

```html
</ins>*bar*
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
### [Example 136](https://spec.commonmark.org/0.29/#example-136)

This markdown:

```markdown
<del>
*foo*
</del>

```

Should give output:

```html
<del>*foo*</del>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 137](https://spec.commonmark.org/0.29/#example-137)

This markdown:

```markdown
<del>

*foo*

</del>

```

Should give output:

```html
<del><p><em>foo</em></p></del>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 138](https://spec.commonmark.org/0.29/#example-138)

This markdown:

```markdown
<del>*foo*</del>

```

Should give output:

```html
<p><del><em>foo</em></del></p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 139](https://spec.commonmark.org/0.29/#example-139)

This markdown:

```markdown
<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
okay

```

Should give output:

```html
<pre language="haskell"><code>import Text.HTML.TagSoup main :: IO () main = print $ parseTags tags</code></pre><p>okay</p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 140](https://spec.commonmark.org/0.29/#example-140)

This markdown:

```markdown
<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
</script>
okay

```

Should give output:

```html
<script type="text/javascript">// JavaScript example document.getElementById(&quot;demo&quot;).innerHTML = &quot;Hello JavaScript!&quot;;</script><p>okay</p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 141](https://spec.commonmark.org/0.29/#example-141)

This markdown:

```markdown
<style
  type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
okay

```

Should give output:

```html
<style type="text/css">h1 {color:red;} p {color:blue;}</style><p>okay</p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 142](https://spec.commonmark.org/0.29/#example-142)

This markdown:

```markdown
<style
  type="text/css">

foo

```

Should give output:

```html
<style type="text/css">foo
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol
```
### [Example 143](https://spec.commonmark.org/0.29/#example-143)

This markdown:

```markdown
> <div>
> foo

bar

```

Should give output:

```html
<blockquote><div>foo</blockquote><p>bar</p>
```

But instead was:

```html
<p>&gt; &lt;div&gt; &gt; foo</p><p>bar</p>
```
### [Example 144](https://spec.commonmark.org/0.29/#example-144)

This markdown:

```markdown
- <div>
- foo

```

Should give output:

```html
<ul><li><div></li><li>foo</li></ul>
```

But instead was:

```html
<ul><li><p>&lt;div&gt;</p></li><li><p>foo</p></li></ul>
```
### [Example 145](https://spec.commonmark.org/0.29/#example-145)

This markdown:

```markdown
<style>p{color:red;}</style>
*foo*

```

Should give output:

```html
<style>p{color:red;}</style><p><em>foo</em></p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 146](https://spec.commonmark.org/0.29/#example-146)

This markdown:

```markdown
<!-- foo -->*bar*
*baz*

```

Should give output:

```html
*bar*<p><em>baz</em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol =
```
### [Example 147](https://spec.commonmark.org/0.29/#example-147)

This markdown:

```markdown
<script>
foo
</script>1. *bar*

```

Should give output:

```html
<script>foo</script>1. *bar*
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 148](https://spec.commonmark.org/0.29/#example-148)

This markdown:

```markdown
<!-- Foo

bar
   baz -->
okay

```

Should give output:

```html
<p>okay</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol =
```
### [Example 149](https://spec.commonmark.org/0.29/#example-149)

This markdown:

```markdown
<?php

  echo '>';

?>
okay

```

Should give output:

```html
&#39;; ?&gt;<p>okay</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol =
```
### [Example 150](https://spec.commonmark.org/0.29/#example-150)

This markdown:

```markdown
<!DOCTYPE html>

```

Should give output:

```html
<!DOCTYPE HTML>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol =
```
### [Example 151](https://spec.commonmark.org/0.29/#example-151)

This markdown:

```markdown
<![CDATA[
function matchwo(a,b)
{
  if (a < b && a < 0) then {
    return 1;

  } else {

    return 0;
  }
}
]]>
okay

```

Should give output:

```html
<p>okay</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol =
```
### [Example 152](https://spec.commonmark.org/0.29/#example-152)

This markdown:

```markdown
  <!-- foo -->

    <!-- foo -->

```

Should give output:

```html
<pre><code>&lt;!-- foo --&gt;</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 153](https://spec.commonmark.org/0.29/#example-153)

This markdown:

```markdown
  <div>

    <div>

```

Should give output:

```html
<div><pre><code>&lt;div&gt;</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
### [Example 154](https://spec.commonmark.org/0.29/#example-154)

This markdown:

```markdown
Foo
<div>
bar
</div>

```

Should give output:

```html
<p>Foo</p><div>bar</div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 155](https://spec.commonmark.org/0.29/#example-155)

This markdown:

```markdown
<div>
bar
</div>
*foo*

```

Should give output:

```html
<div>bar</div>*foo*
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 156](https://spec.commonmark.org/0.29/#example-156)

This markdown:

```markdown
Foo
<a href="bar">
baz

```

Should give output:

```html
<p>Foo<a href="bar">baz</p>
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol
```
### [Example 157](https://spec.commonmark.org/0.29/#example-157)

This markdown:

```markdown
<div>

*Emphasized* text.

</div>

```

Should give output:

```html
<div><p><em>Emphasized</em>text.</p></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 158](https://spec.commonmark.org/0.29/#example-158)

This markdown:

```markdown
<div>
*Emphasized* text.
</div>

```

Should give output:

```html
<div>*Emphasized* text.</div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 159](https://spec.commonmark.org/0.29/#example-159)

This markdown:

```markdown
<table>

<tr>

<td>
Hi
</td>

</tr>

</table>

```

Should give output:

```html
<table><tr><td>Hi</td></tr></table>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
### [Example 160](https://spec.commonmark.org/0.29/#example-160)

This markdown:

```markdown
<table>

  <tr>

    <td>
      Hi
    </td>

  </tr>

</table>

```

Should give output:

```html
<table><tr><pre><code>&lt;td&gt; Hi &lt;/td&gt;</code></pre></tr></table>
```

But instead was:

```html
ERROR Problem at row 11 Expecting Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```

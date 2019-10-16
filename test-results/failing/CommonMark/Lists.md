# CommonMark - Lists

## [Example 271](https://spec.commonmark.org/0.29/#example-271)

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
<ul><li><p>foo</p></li><li><p>bar</p></li></ul><ul><li><p>baz</p></li></ul>
```
## [Example 272](https://spec.commonmark.org/0.29/#example-272)

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
## [Example 273](https://spec.commonmark.org/0.29/#example-273)

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
## [Example 275](https://spec.commonmark.org/0.29/#example-275)

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
## [Example 276](https://spec.commonmark.org/0.29/#example-276)

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
## [Example 277](https://spec.commonmark.org/0.29/#example-277)

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
## [Example 278](https://spec.commonmark.org/0.29/#example-278)

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
## [Example 279](https://spec.commonmark.org/0.29/#example-279)

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
## [Example 280](https://spec.commonmark.org/0.29/#example-280)

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
## [Example 281](https://spec.commonmark.org/0.29/#example-281)

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
## [Example 282](https://spec.commonmark.org/0.29/#example-282)

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
## [Example 283](https://spec.commonmark.org/0.29/#example-283)

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
## [Example 284](https://spec.commonmark.org/0.29/#example-284)

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
## [Example 285](https://spec.commonmark.org/0.29/#example-285)

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
<ul><li><p>a</p></li><li><p></p></li></ul><ul><li><p>c</p></li></ul>
```
## [Example 286](https://spec.commonmark.org/0.29/#example-286)

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
## [Example 287](https://spec.commonmark.org/0.29/#example-287)

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
## [Example 288](https://spec.commonmark.org/0.29/#example-288)

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
## [Example 289](https://spec.commonmark.org/0.29/#example-289)

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
## [Example 290](https://spec.commonmark.org/0.29/#example-290)

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
## [Example 291](https://spec.commonmark.org/0.29/#example-291)

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
## [Example 292](https://spec.commonmark.org/0.29/#example-292)

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
## [Example 293](https://spec.commonmark.org/0.29/#example-293)

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
## [Example 294](https://spec.commonmark.org/0.29/#example-294)

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
## [Example 295](https://spec.commonmark.org/0.29/#example-295)

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
## [Example 296](https://spec.commonmark.org/0.29/#example-296)

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

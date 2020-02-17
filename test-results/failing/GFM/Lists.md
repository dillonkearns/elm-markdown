# GFM - Lists

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
<ul><li>foo</li></ul><ul><li>bar</li></ul><ul><li>baz</li></ul>
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
<ul><li>foo</li></ul><p>- bar</p><pre><code>- baz</code></pre><pre><code>bim</code></pre>
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
<ul><li>a</li></ul><p>- b - c - d - e - f</p><ul><li>g</li></ul>
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
<ol><li>a</li></ol><p>2. b</p><p>3. c</p>
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
<ul><li>a</li></ul><p>- b - c - d</p><pre><code>- e</code></pre>
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
<ol><li>a</li></ol><p>2. b</p><pre><code>3. c</code></pre>
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
<ul><li>a</li><li>b</li></ul><ul><li>c</li></ul>
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
<ul><li>a</li><li></li></ul><ul><li>c</li></ul>
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
<ul><li>a</li><li>b</li></ul><p>c</p><ul><li>d</li></ul>
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
ERROR Problem at row 10 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 13 Expecting Problem at row 1 Expecting ``
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
<ul><li>a</li></ul><p>- b</p><pre><code>c</code></pre><ul><li>d</li></ul>
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
<ul><li>a</li></ul><blockquote><p>b</p></blockquote><ul><li>c</li></ul>
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
<ul><li>a</li></ul><blockquote><p>b</p><pre><code>c</code></pre></blockquote><ul><li>d</li></ul>
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
<ul><li>a</li></ul><p>- b</p>
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
ERROR Problem at row 10 Expecting Problem at row 1 Expecting ``
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
<ul><li>foo</li></ul><p>* bar</p><p>baz</p>
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
<ul><li>a</li></ul><p>- b - c</p><ul><li>d</li></ul><p>- e - f</p>
```

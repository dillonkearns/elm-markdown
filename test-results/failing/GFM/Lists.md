# GFM - Lists

## [Example 274](https://github.github.com/gfm/#example-274)

This markdown:

```markdown
The number of windows in my house is
14.  The number of doors is 6.

```

Should give output:

```html
<p>The number of windows in my house is 14. The number of doors is 6.</p>
```

But instead was:

```html
<p>The number of windows in my house is</p><ol start="14"><li>The number of doors is 6.</li></ol>
```
## [Example 276](https://github.github.com/gfm/#example-276)

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
## [Example 277](https://github.github.com/gfm/#example-277)

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
## [Example 278](https://github.github.com/gfm/#example-278)

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
## [Example 279](https://github.github.com/gfm/#example-279)

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
## [Example 280](https://github.github.com/gfm/#example-280)

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
## [Example 281](https://github.github.com/gfm/#example-281)

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
## [Example 282](https://github.github.com/gfm/#example-282)

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
## [Example 283](https://github.github.com/gfm/#example-283)

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
## [Example 284](https://github.github.com/gfm/#example-284)

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
## [Example 285](https://github.github.com/gfm/#example-285)

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
<ul><li>a</li></ul><ul><li>c</li></ul>
```
## [Example 286](https://github.github.com/gfm/#example-286)

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
## [Example 287](https://github.github.com/gfm/#example-287)

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
## [Example 288](https://github.github.com/gfm/#example-288)

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
## [Example 289](https://github.github.com/gfm/#example-289)

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
## [Example 290](https://github.github.com/gfm/#example-290)

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
## [Example 291](https://github.github.com/gfm/#example-291)

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
## [Example 293](https://github.github.com/gfm/#example-293)

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
## [Example 294](https://github.github.com/gfm/#example-294)

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
## [Example 295](https://github.github.com/gfm/#example-295)

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
## [Example 296](https://github.github.com/gfm/#example-296)

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

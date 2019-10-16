# GFM - Thematic breaks

## [Example 14](https://github.github.com/gfm/#example-14)

This markdown:

```markdown
+++

```

Should give output:

```html
<p>+++</p>
```

But instead was:

```html
<ul><li><p>++</p></li></ul>
```
## [Example 16](https://github.github.com/gfm/#example-16)

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
<ul><li><p>-</p></li></ul><ul><li><p></p></li></ul><p>__</p>
```
## [Example 19](https://github.github.com/gfm/#example-19)

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
## [Example 21](https://github.github.com/gfm/#example-21)

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
## [Example 22](https://github.github.com/gfm/#example-22)

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
## [Example 23](https://github.github.com/gfm/#example-23)

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
## [Example 24](https://github.github.com/gfm/#example-24)

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
## [Example 25](https://github.github.com/gfm/#example-25)

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
## [Example 26](https://github.github.com/gfm/#example-26)

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
## [Example 27](https://github.github.com/gfm/#example-27)

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
## [Example 29](https://github.github.com/gfm/#example-29)

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
## [Example 30](https://github.github.com/gfm/#example-30)

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
<ul><li><p>Foo</p></li><li><p><em></em></p></li><li><p>Bar</p></li></ul>
```
## [Example 31](https://github.github.com/gfm/#example-31)

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

# GFM - Tabs

## [Example 2](https://github.github.com/gfm/#example-2)

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
## [Example 3](https://github.github.com/gfm/#example-3)

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
## [Example 4](https://github.github.com/gfm/#example-4)

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
## [Example 5](https://github.github.com/gfm/#example-5)

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
## [Example 6](https://github.github.com/gfm/#example-6)

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
## [Example 7](https://github.github.com/gfm/#example-7)

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
<p>- foo</p>
```
## [Example 8](https://github.github.com/gfm/#example-8)

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
## [Example 9](https://github.github.com/gfm/#example-9)

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
## [Example 11](https://github.github.com/gfm/#example-11)

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

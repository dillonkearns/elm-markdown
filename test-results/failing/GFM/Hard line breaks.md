# GFM - Hard line breaks

## [Example 630](https://github.github.com/gfm/#example-630)

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
## [Example 631](https://github.github.com/gfm/#example-631)

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
## [Example 632](https://github.github.com/gfm/#example-632)

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
## [Example 633](https://github.github.com/gfm/#example-633)

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
## [Example 634](https://github.github.com/gfm/#example-634)

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
## [Example 635](https://github.github.com/gfm/#example-635)

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
## [Example 636](https://github.github.com/gfm/#example-636)

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
## [Example 639](https://github.github.com/gfm/#example-639)

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
## [Example 640](https://github.github.com/gfm/#example-640)

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

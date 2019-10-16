# CommonMark - Hard line breaks

## [Example 630](https://spec.commonmark.org/0.29/#example-630)

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
## [Example 631](https://spec.commonmark.org/0.29/#example-631)

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
## [Example 632](https://spec.commonmark.org/0.29/#example-632)

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
## [Example 633](https://spec.commonmark.org/0.29/#example-633)

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
## [Example 634](https://spec.commonmark.org/0.29/#example-634)

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
## [Example 635](https://spec.commonmark.org/0.29/#example-635)

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
<ul><li><p>foo</p></li></ul><p>bar</p>
```
## [Example 636](https://spec.commonmark.org/0.29/#example-636)

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
<ul><li><p>foo\</p></li></ul><p>bar</p>
```
## [Example 639](https://spec.commonmark.org/0.29/#example-639)

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
## [Example 640](https://spec.commonmark.org/0.29/#example-640)

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

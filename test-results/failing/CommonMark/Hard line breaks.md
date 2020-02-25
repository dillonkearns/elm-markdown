# CommonMark - Hard line breaks

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

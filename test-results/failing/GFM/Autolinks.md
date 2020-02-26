# GFM - Autolinks

## [Example 598](https://spec.commonmark.org/0.29/#example-598)

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
## [Example 600](https://spec.commonmark.org/0.29/#example-600)

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
## [Example 601](https://spec.commonmark.org/0.29/#example-601)

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
## [Example 602](https://spec.commonmark.org/0.29/#example-602)

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
## [Example 603](https://spec.commonmark.org/0.29/#example-603)

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
## [Example 604](https://spec.commonmark.org/0.29/#example-604)

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
## [Example 606](https://spec.commonmark.org/0.29/#example-606)

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

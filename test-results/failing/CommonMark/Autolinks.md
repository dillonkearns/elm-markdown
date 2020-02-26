# CommonMark - Autolinks

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

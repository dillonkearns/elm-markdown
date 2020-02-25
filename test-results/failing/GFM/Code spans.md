# GFM - Code spans

## [Example 344](https://spec.commonmark.org/0.29/#example-344)

This markdown:

```markdown
<a href="`">`

```

Should give output:

```html
<p><a href="`">`</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
## [Example 346](https://spec.commonmark.org/0.29/#example-346)

This markdown:

```markdown
<http://foo.bar.`baz>`

```

Should give output:

```html
<p><a href="http://foo.bar.%60baz">http://foo.bar.`baz</a>`</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```

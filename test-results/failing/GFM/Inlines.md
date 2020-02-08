# GFM - Inlines

## [Example 297](https://spec.commonmark.org/0.29/#example-297)

This markdown:

```markdown
`hi`lo`

```

Should give output:

```html
<p><code>hi</code>lo`</p>
```

But instead was:

```html
<p><code>hi</code>lo</p>
```

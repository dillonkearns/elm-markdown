# GFM - Inlines

## [Example 297](https://github.github.com/gfm/#example-297)

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

# GFM - Precedence

## [Example 12](https://github.github.com/gfm/#example-12)

This markdown:

```markdown
- `one
- two`

```

Should give output:

```html
<ul><li>`one</li><li>two`</li></ul>
```

But instead was:

```html
<ul><li><code>one</code></li><li>two</li></ul>
```

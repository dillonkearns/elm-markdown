# GFM - [extension] Strikethrough

## [Example 491](https://github.github.com/gfm/#example-491)

This markdown:

```markdown
~~Hi~~ Hello, world!
```

Should give output:

```html
<p><del>Hi</del>Hello, world!</p>
```

But instead was:

```html
<p>~~Hi~~ Hello, world!</p>
```

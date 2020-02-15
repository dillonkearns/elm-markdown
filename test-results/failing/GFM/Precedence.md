# GFM - Precedence

## [Example 12](https://spec.commonmark.org/0.29/#example-12)

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
ERROR Problem at row 5 Expecting Problem at row 1 Expecting `
```

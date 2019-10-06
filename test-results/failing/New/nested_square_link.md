# New - nested_square_link

## Example undefined

This markdown:

```markdown
[the `]` character](/url)

[the \` character](/url)

```

Should give output:

```html
<p><a href="/url">the<code>]</code>character</a></p><p><a href="/url">the ` character</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```

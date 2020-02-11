# New - lazy_blockquotes

## Example undefined

This markdown:

```markdown
> hi there
bud

```

Should give output:

```html
<blockquote><p>hi there bud</p></blockquote>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```

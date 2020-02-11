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
<blockquote><p>hi there</p></blockquote><p>bud</p>
```

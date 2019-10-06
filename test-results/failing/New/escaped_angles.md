# New - escaped_angles

## Example undefined

This markdown:

```markdown
\>

```

Should give output:

```html
<p>&gt;</p>
```

But instead was:

```html
<p>\&gt;</p>
```

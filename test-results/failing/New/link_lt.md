# New - link_lt

## Example undefined

This markdown:

```markdown
[URL](<test)

```

Should give output:

```html
<p><a href="%3Ctest">URL</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol &gt;
```

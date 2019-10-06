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
<p><a href="&lt;test">URL</a></p>
```

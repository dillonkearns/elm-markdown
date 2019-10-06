# New - not_a_link

## Example undefined

This markdown:

```markdown
\[test](not a link)

```

Should give output:

```html
<p>[test](not a link)</p>
```

But instead was:

```html
<p>\<a href="not a link">test</a></p>
```

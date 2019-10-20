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
ERROR Problem at row 3 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
```

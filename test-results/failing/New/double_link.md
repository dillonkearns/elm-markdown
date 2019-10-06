# New - double_link

## Example undefined

This markdown:

```markdown
<p>Already linked: <a href="http://example.com/">http://example.com/</a>.</p>

Already linked: [http://example.com/](http://example.com/).

Already linked: <a href="http://example.com/">**http://example.com/**</a>.

```

Should give output:

```html
<p>Already linked:<a href="http://example.com/">http://example.com/</a>.</p><p>Already linked:<a href="http://example.com/">http://example.com/</a>.</p><p>Already linked:<a href="http://example.com/"><strong>http://example.com/</strong></a>.</p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```

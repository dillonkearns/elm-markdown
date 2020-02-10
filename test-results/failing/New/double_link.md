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
ERROR oneOf failed parsing this value:<p>Parsing failed in the following 2 ways: (1) Expected a but was p (2) Expected div but was p (3) Expected th but was p (4) Expected pre but was p (5) Expected td but was p (6) Expected tr but was p (7) Expected table but was p
```

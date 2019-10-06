# New - links

## Example undefined

This markdown:

```markdown
Link: [constructor][].

[One](https://example.com/1) ([Two](https://example.com/2)) [Three](https://example.com/3)

[constructor]: https://example.org/

```

Should give output:

```html
<p>Link:<a href="https://example.org/">constructor</a>.</p><p><a href="https://example.com/1">One</a>(<a href="https://example.com/2">Two</a>)<a href="https://example.com/3">Three</a></p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```

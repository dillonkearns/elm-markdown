# New - autolink_lines

## Example undefined

This markdown:

```markdown
hello world
<http://example.com>

```

Should give output:

```html
<p>hello world<a href="http://example.com">http://example.com</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol /&gt; Problem at row 3 Expecting symbol &gt;
```

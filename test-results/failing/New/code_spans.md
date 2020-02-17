# New - code_spans

## Example undefined

This markdown:

```markdown
`someone@example.com`

``*test`*
```

Should give output:

```html
<p><code>someone@example.com</code></p><p>``<em>test`</em></p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting Problem at row 1 Expecting ``
```

# Security - sanitizer_bypass_remove_generic

## Example undefined

This markdown:

```markdown
<a>a2<a2t>a2</a> b <c>c</c> d
# ![text](URL)
```

Should give output:

```html
<p>a2a2 b c d</p><h1 id="text"><img alt="text" src="URL"></h1>
```

But instead was:

```html
<p>&lt;a&gt;a2&lt;a2t&gt;a2&lt;/a&gt; b<c><p>c</p></c>d</p><h1><img alt="text" src="URL"></h1>
```

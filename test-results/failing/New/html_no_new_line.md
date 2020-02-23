# New - html_no_new_line

## Example undefined

This markdown:

```markdown
<img src='sdfg'>
```

Should give output:

```html
<img src="sdfg">
```

But instead was:

```html
<p>&lt;img src=&#39;sdfg&#39;&gt;</p>
```

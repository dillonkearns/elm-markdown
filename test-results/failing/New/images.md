# New - images

## Example undefined

This markdown:

```markdown
![Image](javascript:alert)

![Image](vbscript:alert)

![Image](javascript&colon;alert&#40;1&#41;)

![Image](javascript&#58document;alert&#40;1&#41;)

![Image](data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K)

```

Should give output:

```html
<p>Image</p><p>Image</p><p>Image</p><p>Image</p><p>Image</p>
```

But instead was:

```html
<p>!<a href="javascript:alert">Image</a></p><p>!<a href="vbscript:alert">Image</a></p><p>!<a href="javascript:alert(1)">Image</a></p><p>!<a href="javascript:document;alert(1)">Image</a></p><p>!<a href="data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K">Image</a></p>
```

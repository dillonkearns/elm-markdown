# New - sanitize_links

## Example undefined

This markdown:

````````````markdown
[URL](javascript:alert)

[URL](vbscript:alert)

[URL](javascript&colon;alert&#40;1&#41;)

[URL](javascript&#58document;alert&#40;1&#41;)

[URL](data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K)

````````````

Should give output:

````````````html
<p>URL</p><p>URL</p><p>URL</p><p>URL</p><p>URL</p>
````````````

But instead was:

````````````html
<p><a href="javascript:alert">URL</a></p><p><a href="vbscript:alert">URL</a></p><p><a href="javascript:alert(1)">URL</a></p><p><a href="javascript:document;alert(1)">URL</a></p><p><a href="data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K">URL</a></p>
````````````

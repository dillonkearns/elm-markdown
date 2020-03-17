# New - images

## Example undefined

This markdown:

````````````markdown
![Image](javascript:alert)

![Image](vbscript:alert)

![Image](javascript&colon;alert&#40;1&#41;)

![Image](javascript&#58document;alert&#40;1&#41;)

![Image](data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K)

````````````

Should give output:

````````````html
<p>Image</p><p>Image</p><p>Image</p><p>Image</p><p>Image</p>
````````````

But instead was:

````````````html
<p><img alt="Image" src="javascript:alert"></p><p><img alt="Image" src="vbscript:alert"></p><p><img alt="Image" src="javascript:alert(1)"></p><p><img alt="Image" src="javascript:document;alert(1)"></p><p><img alt="Image" src="data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K"></p>
````````````

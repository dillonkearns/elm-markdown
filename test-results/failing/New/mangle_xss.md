# New - mangle_xss

## Example undefined

This markdown:

````````````markdown
<<svg/onload="alert(1)"//@x>

<bar"onclick="alert('XSS')"@foo>

````````````

Should give output:

````````````html
<p>&lt;&lt;svg/onload=&quot;alert(1)&quot;//@x&gt;</p><p>&lt;bar&quot;onclick=&quot;alert(&#39;XSS&#39;)&quot;@foo&gt;</p>
````````````

But instead was:

````````````html
ERROR Problem at row 1 Expecting at least 1 tag name character
````````````

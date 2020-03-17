# Security - sanitizer_bypass_remove_generic

## Example undefined

This markdown:

````````````markdown
<a>a2<a2t>a2</a> b <c>c</c> d
# ![text](URL)
````````````

Should give output:

````````````html
<p>a2a2 b c d</p><h1 id="text"><img alt="text" src="URL"></h1>
````````````

But instead was:

````````````html
ERROR Problem at row 1 tag name mismatch: a2t and a
````````````

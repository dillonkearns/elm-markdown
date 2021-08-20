# Original - tabs

## Example undefined

This markdown:

````````````markdown
+	this is a list item
	indented with tabs

+   this is a list item
    indented with spaces

Code:

	this code block is indented by one tab

And:

		this code block is indented by two tabs

And:

	+	this is an example list item
		indented with tabs
	
	+   this is an example list item
	    indented with spaces

````````````

Should give output:

````````````html
<ul><li><p>this is a list item indented with tabs</p></li><li><p>this is a list item indented with spaces</p></li></ul><p>Code:</p><pre><code>this code block is indented by one tab</code></pre><p>And:</p><pre><code>this code block is indented by two tabs</code></pre><p>And:</p><pre><code>+ this is an example list item indented with tabs + this is an example list item indented with spaces</code></pre>
````````````

But instead was:

````````````html
<ul><li>this is a list item</li></ul><pre><code>indented with tabs</code></pre><ul><li>this is a list item indented with spaces</li></ul><p>Code:</p><pre><code>this code block is indented by one tab</code></pre><p>And:</p><pre><code>this code block is indented by two tabs</code></pre><p>And:</p><pre><code>+ this is an example list item indented with tabs + this is an example list item indented with spaces</code></pre>
````````````

# Original - inline_html_simple

## Example undefined

This markdown:

````````````markdown
Here's a simple block:

<div>
	foo
</div>

This should be a code block, though:

	<div>
		foo
	</div>

As should this:

	<div>foo</div>

Now, nested:

<div>
	<div>
		<div>
			foo
		</div>
	</div>
</div>

This should just be an HTML comment:

<!-- Comment -->

Multiline:

<!--
Blah
Blah
-->

Code block:

	<!-- Comment -->

Just plain comment, with trailing spaces on the line:

<!-- foo -->   

Code:

	<hr />
	
Hr's:

<hr>

<hr/>

<hr />

<hr>   

<hr/>  

<hr /> 

<hr class="foo" id="bar" />

<hr class="foo" id="bar"/>

<hr class="foo" id="bar" >


````````````

Should give output:

````````````html
<p>Here&#39;s a simple block:</p><div>foo</div><p>This should be a code block, though:</p><pre><code>&lt;div&gt; foo &lt;/div&gt;</code></pre><p>As should this:</p><pre><code>&lt;div&gt;foo&lt;/div&gt;</code></pre><p>Now, nested:</p><div><div><div>foo</div></div></div><p>This should just be an HTML comment:</p><p>Multiline:</p><p>Code block:</p><pre><code>&lt;!-- Comment --&gt;</code></pre><p>Just plain comment, with trailing spaces on the line:</p><p>Code:</p><pre><code>&lt;hr /&gt;</code></pre><p>Hr&#39;s:</p><hr><hr><hr><hr><hr><hr><hr class="foo" id="bar"><hr class="foo" id="bar"><hr class="foo" id="bar">
````````````

But instead was:

````````````html
ERROR Problem at row 70 Expecting symbol
````````````

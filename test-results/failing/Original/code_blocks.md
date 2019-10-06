# Original - code_blocks

## Example undefined

This markdown:

```markdown
	code block on the first line
	
Regular text.

    code block indented by spaces

Regular text.

	the lines in this block  
	all contain trailing spaces  

Regular Text.

	code block on the last line

```

Should give output:

```html
<pre><code>code block on the first line</code></pre><p>Regular text.</p><pre><code>code block indented by spaces</code></pre><p>Regular text.</p><pre><code>the lines in this block all contain trailing spaces</code></pre><p>Regular Text.</p><pre><code>code block on the last line</code></pre>
```

But instead was:

```html
<pre><code>code block on the first line</code></pre><pre><code></code></pre><p>Regular text.</p><pre><code>code block indented by spaces</code></pre><p>Regular text.</p><pre><code>the lines in this block</code></pre><pre><code>all contain trailing spaces</code></pre><p>Regular Text.</p><pre><code>code block on the last line</code></pre>
```

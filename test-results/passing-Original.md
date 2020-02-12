# Original

## blockquotes_with_code_blocks

### Example undefined

This markdown:


```markdown
> Example:
> 
>     sub status {
>         print "working";
>     }
> 
> Or:
> 
>     sub status {
>         return "working";
>     }

```

Gives this correct output:


```html
<blockquote>
  <p>Example:</p>

<pre><code>sub status {
    print "working";
}
</code></pre>
  
  <p>Or:</p>

<pre><code>sub status {
    return "working";
}
</code></pre>
</blockquote>

```

## code_blocks

### Example undefined

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

Gives this correct output:


```html
<pre><code>code block on the first line
</code></pre>

<p>Regular text.</p>

<pre><code>code block indented by spaces
</code></pre>

<p>Regular text.</p>

<pre><code>the lines in this block  
all contain trailing spaces  
</code></pre>

<p>Regular Text.</p>

<pre><code>code block on the last line
</code></pre>

```

## nested_blockquotes

### Example undefined

This markdown:


```markdown
> foo
>
> > bar
>
> foo

```

Gives this correct output:


```html
<blockquote>
  <p>foo</p>
  
  <blockquote>
    <p>bar</p>
  </blockquote>
  
  <p>foo</p>
</blockquote>

```


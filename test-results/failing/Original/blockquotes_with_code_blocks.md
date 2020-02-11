# Original - blockquotes_with_code_blocks

## Example undefined

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

Should give output:

```html
<blockquote><p>Example:</p><pre><code>sub status { print &quot;working&quot;; }</code></pre><p>Or:</p><pre><code>sub status { return &quot;working&quot;; }</code></pre></blockquote>
```

But instead was:

```html
<blockquote><p>Example:</p><blockquote><blockquote><pre><code>sub status {</code></pre><blockquote><pre><code>print &quot;working&quot;;</code></pre><blockquote><pre><code>}</code></pre><blockquote><blockquote><p>Or:</p><blockquote><blockquote><pre><code>sub status {</code></pre><blockquote><pre><code>return &quot;working&quot;;</code></pre><blockquote><pre><code>}</code></pre></blockquote></blockquote></blockquote></blockquote></blockquote></blockquote></blockquote></blockquote></blockquote></blockquote></blockquote>
```

# Original - tidyness

## Example undefined

This markdown:

```markdown
> A list within a blockquote:
> 
> *	asterisk 1
> *	asterisk 2
> *	asterisk 3

```

Should give output:

```html
<blockquote><p>A list within a blockquote:</p><ul><li>asterisk 1</li><li>asterisk 2</li><li>asterisk 3</li></ul></blockquote>
```

But instead was:

```html
<p>&gt; A list within a blockquote: &gt; &gt;<em>asterisk 1 &gt;</em>asterisk 2 &gt;<em>asterisk 3</em></p>
```

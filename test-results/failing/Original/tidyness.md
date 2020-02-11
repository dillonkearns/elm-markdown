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
<blockquote><p>A list within a blockquote:</p><blockquote><blockquote><p><em>asterisk 1</em></p><blockquote><p><em>asterisk 2</em></p><blockquote><p><em>asterisk 3</em></p></blockquote></blockquote></blockquote></blockquote></blockquote>
```

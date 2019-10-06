# Original - nested_blockquotes

## Example undefined

This markdown:

```markdown
> foo
>
> > bar
>
> foo

```

Should give output:

```html
<blockquote><p>foo</p><blockquote><p>bar</p></blockquote><p>foo</p></blockquote>
```

But instead was:

```html
<p>&gt; foo &gt; &gt; &gt; bar &gt; &gt; foo</p>
```

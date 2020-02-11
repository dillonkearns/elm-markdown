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
<blockquote><p>foo</p><blockquote><blockquote><blockquote><p>bar</p><blockquote><blockquote><p>foo</p></blockquote></blockquote></blockquote></blockquote></blockquote></blockquote>
```

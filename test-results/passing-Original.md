# Original

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


# New

## adjacent_lists

### Example undefined

This markdown:


```markdown
* This should be
* An unordered list

1. This should be
2. An unordered list

```

Gives this correct output:


```html
<ul>
<li>This should be</li>
<li>An unordered list</li>
</ul>

<ol>
<li>This should be</li>
<li>An unordered list</li>
</ol>

```

## escaped_angles

### Example undefined

This markdown:


```markdown
\>

```

Gives this correct output:


```html
<p>&gt;</p>

```

## lazy_blockquotes

### Example undefined

This markdown:


```markdown
> hi there
bud

```

Gives this correct output:


```html
<blockquote>
  <p>hi there
bud</p>
</blockquote>

```

## not_a_link

### Example undefined

This markdown:


```markdown
\[test](not a link)

```

Gives this correct output:


```html
<p>[test](not a link)</p>

```


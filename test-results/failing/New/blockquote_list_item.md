# New - blockquote_list_item

## Example undefined

This markdown:

```markdown
This fails in markdown.pl and upskirt:

* hello
  > world

```

Should give output:

```html
<p>This fails in markdown.pl and upskirt:</p><ul><li>hello<blockquote><p>world</p></blockquote></li></ul>
```

But instead was:

```html
<p>This fails in markdown.pl and upskirt:</p><ul><li>hello</li></ul><blockquote><p>world</p></blockquote>
```

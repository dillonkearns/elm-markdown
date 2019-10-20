# New - links_paren

## Example undefined

This markdown:

```markdown
([one](http://example.com/1)) ([two](http://example.com/2))

([one](http://example.com/1))  ([two](http://example.com/2))

([one](http://example.com/1 "a")) ([two](http://example.com/2 "b"))

```

Should give output:

```html
<p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p><p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p><p>(<a href="http://example.com/1" title="a">one</a>) (<a href="http://example.com/2" title="b">two</a>)</p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
```

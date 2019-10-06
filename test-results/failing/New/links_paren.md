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
<p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p><p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p><p>(<a a\""="" href="http://example.com/1 \">one</a>) (<a b\""="" href="http://example.com/2 \">two</a>)</p>
```

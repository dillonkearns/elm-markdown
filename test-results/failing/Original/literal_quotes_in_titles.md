# Original - literal_quotes_in_titles

## Example undefined

This markdown:

```markdown
Foo [bar][].

Foo [bar](/url/ "Title with "quotes" inside").


  [bar]: /url/ "Title with "quotes" inside"


```

Should give output:

```html
<p>Foo<a href="/url/" title="Title with &quot;quotes&quot; inside">bar</a>.</p><p>Foo<a href="/url/" title="Title with &quot;quotes&quot; inside">bar</a>.</p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting --- Problem at row 8 Expecting *** Problem at row 8 Expecting ___
```

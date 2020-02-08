# GFM - Soft line breaks

## [Example 646](https://spec.commonmark.org/0.29/#example-646)

This markdown:

```markdown
foo 
 baz

```

Should give output:

```html
<p>foo baz</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```

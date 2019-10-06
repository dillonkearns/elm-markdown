# CommonMark - Blank lines

## [Example 197](https://spec.commonmark.org/0.29/#example-197)

This markdown:

```markdown
  

aaa
  

# aaa

  

```

Should give output:

```html
<p>aaa</p><h1>aaa</h1>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```

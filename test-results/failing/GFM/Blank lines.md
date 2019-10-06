# GFM - Blank lines

## [Example 197](https://github.github.com/gfm/#example-197)

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

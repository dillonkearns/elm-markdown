# GFM - Code spans

## [Example 344](https://spec.commonmark.org/0.29/#example-344)

This markdown:

```markdown
<a href="`">`

```

Should give output:

```html
<p><a href="`">`</p>
```

But instead was:

```html
<p>&lt;a href=&quot;<code>&quot;&gt;</code></p>
```
## [Example 347](https://spec.commonmark.org/0.29/#example-347)

This markdown:

```markdown
```foo``

```

Should give output:

```html
<p>```foo``</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol ```
```

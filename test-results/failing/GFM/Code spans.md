# GFM - Code spans

## [Example 330](https://spec.commonmark.org/0.29/#example-330)

This markdown:

```markdown
` `` `

```

Should give output:

```html
<p><code>``</code></p>
```

But instead was:

```html
<p><code></code><code></code></p>
```
## [Example 331](https://spec.commonmark.org/0.29/#example-331)

This markdown:

```markdown
`  ``  `

```

Should give output:

```html
<p><code>``</code></p>
```

But instead was:

```html
<p><code></code><code></code></p>
```
## [Example 338](https://spec.commonmark.org/0.29/#example-338)

This markdown:

```markdown
`foo\`bar`

```

Should give output:

```html
<p><code>foo\</code>bar`</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting `
```
## [Example 340](https://spec.commonmark.org/0.29/#example-340)

This markdown:

```markdown
` foo `` bar `

```

Should give output:

```html
<p><code>foo `` bar</code></p>
```

But instead was:

```html
<p><code>foo</code><code>bar</code></p>
```
## [Example 341](https://spec.commonmark.org/0.29/#example-341)

This markdown:

```markdown
*foo`*`

```

Should give output:

```html
<p>*foo<code>*</code></p>
```

But instead was:

```html
<p><em><code>*</code></em></p>
```
## [Example 343](https://spec.commonmark.org/0.29/#example-343)

This markdown:

```markdown
`<a href="`">`

```

Should give output:

```html
<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting `
```
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
ERROR Problem at row 2 Expecting symbol
```
## [Example 345](https://spec.commonmark.org/0.29/#example-345)

This markdown:

```markdown
`<http://foo.bar.`baz>`

```

Should give output:

```html
<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting `
```
## [Example 346](https://spec.commonmark.org/0.29/#example-346)

This markdown:

```markdown
<http://foo.bar.`baz>`

```

Should give output:

```html
<p><a href="http://foo.bar.%60baz">http://foo.bar.`baz</a>`</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
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
## [Example 348](https://spec.commonmark.org/0.29/#example-348)

This markdown:

```markdown
`foo

```

Should give output:

```html
<p>`foo</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting `
```
## [Example 349](https://spec.commonmark.org/0.29/#example-349)

This markdown:

```markdown
`foo``bar``

```

Should give output:

```html
<p>`foo<code>bar</code></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting `
```

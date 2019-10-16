# CommonMark - Code spans

## [Example 329](https://spec.commonmark.org/0.29/#example-329)

This markdown:

```markdown
`` foo ` bar ``

```

Should give output:

```html
<p><code>foo ` bar</code></p>
```

But instead was:

```html
<p>foo<code>bar</code></p>
```
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
## [Example 335](https://spec.commonmark.org/0.29/#example-335)

This markdown:

```markdown
``
foo
bar  
baz
``

```

Should give output:

```html
<p><code>foo bar baz</code></p>
```

But instead was:

```html
<p>foo bar baz</p>
```
## [Example 336](https://spec.commonmark.org/0.29/#example-336)

This markdown:

```markdown
``
foo 
``

```

Should give output:

```html
<p><code>foo</code></p>
```

But instead was:

```html
<p>foo</p>
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
<p><code>foo\</code>bar</p>
```
## [Example 339](https://spec.commonmark.org/0.29/#example-339)

This markdown:

```markdown
``foo`bar``

```

Should give output:

```html
<p><code>foo`bar</code></p>
```

But instead was:

```html
<p>foo<code>bar</code></p>
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
<ul><li><p>foo</p></li></ul>
```
## [Example 342](https://spec.commonmark.org/0.29/#example-342)

This markdown:

```markdown
[not a `link](/foo`)

```

Should give output:

```html
<p>[not a<code>link](/foo</code>)</p>
```

But instead was:

```html
<p><a href="/foo`">not a<code>link</code></a></p>
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
<p><code>&lt;a href=&quot;</code>&quot;&gt;</p>
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
<p><code>&lt;http://foo.bar.</code>baz&gt;</p>
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
<p><code>foo</code></p>
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
<p><code>foo</code><code>bar</code></p>
```

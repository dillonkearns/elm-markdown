# GFM - Code spans

## [Example 329](https://github.github.com/gfm/#example-329)

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
## [Example 330](https://github.github.com/gfm/#example-330)

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
## [Example 331](https://github.github.com/gfm/#example-331)

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
## [Example 335](https://github.github.com/gfm/#example-335)

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
## [Example 336](https://github.github.com/gfm/#example-336)

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
## [Example 338](https://github.github.com/gfm/#example-338)

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
## [Example 339](https://github.github.com/gfm/#example-339)

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
## [Example 340](https://github.github.com/gfm/#example-340)

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
## [Example 341](https://github.github.com/gfm/#example-341)

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
## [Example 342](https://github.github.com/gfm/#example-342)

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
## [Example 343](https://github.github.com/gfm/#example-343)

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
## [Example 344](https://github.github.com/gfm/#example-344)

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
## [Example 345](https://github.github.com/gfm/#example-345)

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
## [Example 346](https://github.github.com/gfm/#example-346)

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
## [Example 347](https://github.github.com/gfm/#example-347)

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
## [Example 348](https://github.github.com/gfm/#example-348)

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
## [Example 349](https://github.github.com/gfm/#example-349)

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

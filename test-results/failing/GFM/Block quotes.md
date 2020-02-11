# GFM - Block quotes

## [Example 207](https://spec.commonmark.org/0.29/#example-207)

This markdown:

```markdown
> ```
foo
```

```

Should give output:

```html
<blockquote><pre><code></code></pre></blockquote><p>foo</p><pre><code></code></pre>
```

But instead was:

```html
ERROR Problem at row 7 Expecting symbol ```
```
## [Example 208](https://spec.commonmark.org/0.29/#example-208)

This markdown:

```markdown
> foo
    - bar

```

Should give output:

```html
<blockquote><p>foo - bar</p></blockquote>
```

But instead was:

```html
<blockquote><p>foo</p></blockquote><pre><code>- bar</code></pre>
```
## [Example 214](https://spec.commonmark.org/0.29/#example-214)

This markdown:

```markdown
> foo
>
> bar

```

Should give output:

```html
<blockquote><p>foo</p><p>bar</p></blockquote>
```

But instead was:

```html
<blockquote><p>foo bar</p></blockquote>
```
## [Example 219](https://spec.commonmark.org/0.29/#example-219)

This markdown:

```markdown
> bar
>
baz

```

Should give output:

```html
<blockquote><p>bar</p></blockquote><p>baz</p>
```

But instead was:

```html
<blockquote><p>bar baz</p></blockquote>
```
## [Example 222](https://spec.commonmark.org/0.29/#example-222)

This markdown:

```markdown
>     code

>    not code

```

Should give output:

```html
<blockquote><pre><code>code</code></pre></blockquote><blockquote><p>not code</p></blockquote>
```

But instead was:

```html
ERROR Problem at row 6 Expecting TODO
```

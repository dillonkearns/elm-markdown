# CommonMark - Block quotes

## [Example 201](https://spec.commonmark.org/0.29/#example-201)

This markdown:

```markdown
    > # Foo
    > bar
    > baz

```

Should give output:

```html
<pre><code>&gt; # Foo &gt; bar &gt; baz</code></pre>
```

But instead was:

```html
<pre><code>&gt; # Foo</code></pre><pre><code>&gt; bar</code></pre><pre><code>&gt; baz</code></pre>
```
## [Example 202](https://spec.commonmark.org/0.29/#example-202)

This markdown:

```markdown
> # Foo
> bar
baz

```

Should give output:

```html
<blockquote><h1>Foo</h1><p>bar baz</p></blockquote>
```

But instead was:

```html
<blockquote><h1>Foo</h1><p>bar</p></blockquote><p>baz</p>
```
## [Example 203](https://spec.commonmark.org/0.29/#example-203)

This markdown:

```markdown
> bar
baz
> foo

```

Should give output:

```html
<blockquote><p>bar baz foo</p></blockquote>
```

But instead was:

```html
<blockquote><p>bar</p></blockquote><p>baz</p><blockquote><p>foo</p></blockquote>
```
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
## [Example 217](https://spec.commonmark.org/0.29/#example-217)

This markdown:

```markdown
> bar
baz

```

Should give output:

```html
<blockquote><p>bar baz</p></blockquote>
```

But instead was:

```html
<blockquote><p>bar</p></blockquote><p>baz</p>
```
## [Example 220](https://spec.commonmark.org/0.29/#example-220)

This markdown:

```markdown
> > > foo
bar

```

Should give output:

```html
<blockquote><blockquote><blockquote><p>foo bar</p></blockquote></blockquote></blockquote>
```

But instead was:

```html
<blockquote><blockquote><blockquote><p>foo</p></blockquote></blockquote></blockquote><p>bar</p>
```
## [Example 221](https://spec.commonmark.org/0.29/#example-221)

This markdown:

```markdown
>>> foo
> bar
>>baz

```

Should give output:

```html
<blockquote><blockquote><blockquote><p>foo bar baz</p></blockquote></blockquote></blockquote>
```

But instead was:

```html
<blockquote><blockquote><blockquote><p>foo</p></blockquote></blockquote><p>bar</p><blockquote><p>baz</p></blockquote></blockquote>
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

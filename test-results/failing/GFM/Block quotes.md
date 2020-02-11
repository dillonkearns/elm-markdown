# GFM - Block quotes

## [Example 198](https://spec.commonmark.org/0.29/#example-198)

This markdown:

```markdown
> # Foo
> bar
> baz

```

Should give output:

```html
<blockquote><h1>Foo</h1><p>bar baz</p></blockquote>
```

But instead was:

```html
<blockquote><h1>Foo</h1><blockquote><p>bar</p><blockquote><p>baz</p></blockquote></blockquote></blockquote>
```
## [Example 199](https://spec.commonmark.org/0.29/#example-199)

This markdown:

```markdown
># Foo
>bar
> baz

```

Should give output:

```html
<blockquote><h1>Foo</h1><p>bar baz</p></blockquote>
```

But instead was:

```html
<blockquote><h1>Foo</h1><blockquote><p>bar</p><blockquote><p>baz</p></blockquote></blockquote></blockquote>
```
## [Example 200](https://spec.commonmark.org/0.29/#example-200)

This markdown:

```markdown
   > # Foo
   > bar
 > baz

```

Should give output:

```html
<blockquote><h1>Foo</h1><p>bar baz</p></blockquote>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
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
<blockquote><h1>Foo</h1><blockquote><p>bar baz</p></blockquote></blockquote>
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
<blockquote><p>bar baz</p><blockquote><p>foo</p></blockquote></blockquote>
```
## [Example 204](https://spec.commonmark.org/0.29/#example-204)

This markdown:

```markdown
> foo
---

```

Should give output:

```html
<blockquote><p>foo</p></blockquote><hr>
```

But instead was:

```html
<blockquote><p>foo</p><hr></blockquote>
```
## [Example 205](https://spec.commonmark.org/0.29/#example-205)

This markdown:

```markdown
> - foo
- bar

```

Should give output:

```html
<blockquote><ul><li>foo</li></ul></blockquote><ul><li>bar</li></ul>
```

But instead was:

```html
<blockquote><ul><li>foo</li><li>bar</li></ul></blockquote>
```
## [Example 206](https://spec.commonmark.org/0.29/#example-206)

This markdown:

```markdown
>     foo
    bar

```

Should give output:

```html
<blockquote><pre><code>foo</code></pre></blockquote><pre><code>bar</code></pre>
```

But instead was:

```html
<blockquote><pre><code>foo</code></pre><pre><code>bar</code></pre></blockquote>
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
<blockquote><pre><code>foo</code></pre></blockquote>
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
<blockquote><p>foo</p><pre><code>- bar</code></pre></blockquote>
```
## [Example 210](https://spec.commonmark.org/0.29/#example-210)

This markdown:

```markdown
>
>  
> 

```

Should give output:

```html
<blockquote></blockquote>
```

But instead was:

```html
ERROR Problem at row 2 Expecting --- Problem at row 2 Expecting *** Problem at row 2 Expecting ___
```
## [Example 211](https://spec.commonmark.org/0.29/#example-211)

This markdown:

```markdown
>
> foo
>  

```

Should give output:

```html
<blockquote><p>foo</p></blockquote>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
## [Example 212](https://spec.commonmark.org/0.29/#example-212)

This markdown:

```markdown
> foo

> bar

```

Should give output:

```html
<blockquote><p>foo</p></blockquote><blockquote><p>bar</p></blockquote>
```

But instead was:

```html
<blockquote><p>foo</p><blockquote><p>bar</p></blockquote></blockquote>
```
## [Example 213](https://spec.commonmark.org/0.29/#example-213)

This markdown:

```markdown
> foo
> bar

```

Should give output:

```html
<blockquote><p>foo bar</p></blockquote>
```

But instead was:

```html
<blockquote><p>foo</p><blockquote><p>bar</p></blockquote></blockquote>
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
<blockquote><p>foo</p><blockquote><blockquote><p>bar</p></blockquote></blockquote></blockquote>
```
## [Example 216](https://spec.commonmark.org/0.29/#example-216)

This markdown:

```markdown
> aaa
***
> bbb

```

Should give output:

```html
<blockquote><p>aaa</p></blockquote><hr><blockquote><p>bbb</p></blockquote>
```

But instead was:

```html
<blockquote><p>aaa</p><hr><blockquote><p>bbb</p></blockquote></blockquote>
```
## [Example 218](https://spec.commonmark.org/0.29/#example-218)

This markdown:

```markdown
> bar

baz

```

Should give output:

```html
<blockquote><p>bar</p></blockquote><p>baz</p>
```

But instead was:

```html
<blockquote><p>bar</p><p>baz</p></blockquote>
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
<blockquote><p>bar</p><blockquote><p>baz</p></blockquote></blockquote>
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
<blockquote><blockquote><blockquote><p>foo</p><blockquote><p>bar</p><blockquote><blockquote><p>baz</p></blockquote></blockquote></blockquote></blockquote></blockquote></blockquote>
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
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```

# GFM - Block quotes

## [Example 198](https://github.github.com/gfm/#example-198)

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
<p>&gt; # Foo &gt; bar &gt; baz</p>
```
## [Example 199](https://github.github.com/gfm/#example-199)

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
<p>&gt;# Foo &gt;bar &gt; baz</p>
```
## [Example 200](https://github.github.com/gfm/#example-200)

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
## [Example 201](https://github.github.com/gfm/#example-201)

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
## [Example 202](https://github.github.com/gfm/#example-202)

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
<p>&gt; # Foo &gt; bar baz</p>
```
## [Example 203](https://github.github.com/gfm/#example-203)

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
<p>&gt; bar baz &gt; foo</p>
```
## [Example 204](https://github.github.com/gfm/#example-204)

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
<p>&gt; foo</p><hr>
```
## [Example 205](https://github.github.com/gfm/#example-205)

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
<p>&gt; - foo</p><ul><li><p>bar</p></li></ul>
```
## [Example 206](https://github.github.com/gfm/#example-206)

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
<p>&gt; foo</p><pre><code>bar</code></pre>
```
## [Example 207](https://github.github.com/gfm/#example-207)

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
## [Example 208](https://github.github.com/gfm/#example-208)

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
<p>&gt; foo</p><pre><code>- bar</code></pre>
```
## [Example 209](https://github.github.com/gfm/#example-209)

This markdown:

```markdown
>

```

Should give output:

```html
<blockquote></blockquote>
```

But instead was:

```html
<p>&gt;</p>
```
## [Example 210](https://github.github.com/gfm/#example-210)

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
<p>&gt; &gt; &gt;</p>
```
## [Example 211](https://github.github.com/gfm/#example-211)

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
<p>&gt; &gt; foo &gt;</p>
```
## [Example 212](https://github.github.com/gfm/#example-212)

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
<p>&gt; foo</p><p>&gt; bar</p>
```
## [Example 213](https://github.github.com/gfm/#example-213)

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
<p>&gt; foo &gt; bar</p>
```
## [Example 214](https://github.github.com/gfm/#example-214)

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
<p>&gt; foo &gt; &gt; bar</p>
```
## [Example 215](https://github.github.com/gfm/#example-215)

This markdown:

```markdown
foo
> bar

```

Should give output:

```html
<p>foo</p><blockquote><p>bar</p></blockquote>
```

But instead was:

```html
<p>foo &gt; bar</p>
```
## [Example 216](https://github.github.com/gfm/#example-216)

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
<p>&gt; aaa</p><hr><p>&gt; bbb</p>
```
## [Example 217](https://github.github.com/gfm/#example-217)

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
<p>&gt; bar baz</p>
```
## [Example 218](https://github.github.com/gfm/#example-218)

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
<p>&gt; bar</p><p>baz</p>
```
## [Example 219](https://github.github.com/gfm/#example-219)

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
<p>&gt; bar &gt; baz</p>
```
## [Example 220](https://github.github.com/gfm/#example-220)

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
<p>&gt; &gt; &gt; foo bar</p>
```
## [Example 221](https://github.github.com/gfm/#example-221)

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
<p>&gt;&gt;&gt; foo &gt; bar &gt;&gt;baz</p>
```
## [Example 222](https://github.github.com/gfm/#example-222)

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
<p>&gt; code</p><p>&gt; not code</p>
```

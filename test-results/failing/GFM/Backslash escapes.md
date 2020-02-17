# GFM - Backslash escapes

## [Example 299](https://spec.commonmark.org/0.29/#example-299)

This markdown:

```markdown
\	\A\a\ \3\φ\«

```

Should give output:

```html
<p>\ \A\a\ \3\φ\«</p>
```

But instead was:

```html
<p>Aa 3φ«</p>
```
## [Example 300](https://spec.commonmark.org/0.29/#example-300)

This markdown:

```markdown
\*not emphasized*
\<br/> not a tag
\[not a link](/foo)
\`not code`
1\. not a list
\* not a list
\# not a heading
\[foo]: /url "not a reference"
\&ouml; not a character entity

```

Should give output:

```html
<p>*not emphasized* &lt;br/&gt; not a tag [not a link](/foo) `not code` 1. not a list * not a list # not a heading [foo]: /url &quot;not a reference&quot; &amp;ouml; not a character entity</p>
```

But instead was:

```html
ERROR Problem at row 19 Expecting Problem at row 1 Expecting `
```
## [Example 302](https://spec.commonmark.org/0.29/#example-302)

This markdown:

```markdown
foo\
bar

```

Should give output:

```html
<p>foo<br>bar</p>
```

But instead was:

```html
<p>foo bar</p>
```
## [Example 306](https://spec.commonmark.org/0.29/#example-306)

This markdown:

```markdown
<http://example.com?find=\*>

```

Should give output:

```html
<p><a href="http://example.com?find=%5C*">http://example.com?find=\*</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## [Example 307](https://spec.commonmark.org/0.29/#example-307)

This markdown:

```markdown
<a href="/bar\/)">

```

Should give output:

```html
<a href="/bar\/)">
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
## [Example 308](https://spec.commonmark.org/0.29/#example-308)

This markdown:

```markdown
[foo](/bar\* "ti\*tle")

```

Should give output:

```html
<p><a href="/bar*" title="ti*tle">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
```
## [Example 309](https://spec.commonmark.org/0.29/#example-309)

This markdown:

```markdown
[foo]

[foo]: /bar\* "ti\*tle"

```

Should give output:

```html
<p><a href="/bar*" title="ti*tle">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 310](https://spec.commonmark.org/0.29/#example-310)

This markdown:

```markdown
``` foo\+bar
foo
```

```

Should give output:

```html
<pre><code class="language-foo+bar">foo</code></pre>
```

But instead was:

```html
<pre><code>foo</code></pre>
```

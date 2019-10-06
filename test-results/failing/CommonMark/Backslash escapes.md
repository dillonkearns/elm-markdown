# CommonMark - Backslash escapes

## [Example 298](https://spec.commonmark.org/0.29/#example-298)

This markdown:

```markdown
\!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~

```

Should give output:

```html
<p>!&quot;#$%&amp;&#39;()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 19 Expecting Problem at row 1 Expecting symbol (
```
## [Example 301](https://spec.commonmark.org/0.29/#example-301)

This markdown:

```markdown
\\*emphasis*

```

Should give output:

```html
<p>\<em>emphasis</em></p>
```

But instead was:

```html
<p>\\<em>emphasis</em></p>
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
<p>foo\ bar</p>
```
## [Example 303](https://spec.commonmark.org/0.29/#example-303)

This markdown:

```markdown
`` \[\` ``

```

Should give output:

```html
<p><code>\[\`</code></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol ]
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
<p><a href="/bar\* \" ti\*tle\""="">foo</a></p>
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

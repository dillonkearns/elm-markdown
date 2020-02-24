# CommonMark - Link reference definitions

## [Example 162](https://spec.commonmark.org/0.29/#example-162)

This markdown:

```markdown
   [foo]: 
      /url  
           'the title'  

[foo]

```

Should give output:

```html
<p><a href="/url" title="the title">foo</a></p>
```

But instead was:

```html
<p>[foo]:</p><pre><code>/url &#39;the title&#39;</code></pre><p>[foo]</p>
```
## [Example 163](https://spec.commonmark.org/0.29/#example-163)

This markdown:

```markdown
[Foo*bar\]]:my_(url) 'title (with parens)'

[Foo*bar\]]

```

Should give output:

```html
<p><a href="my_(url)" title="title (with parens)">Foo*bar]</a></p>
```

But instead was:

```html
<p>[Foo*bar]]:my_(url) &#39;title (with parens)&#39;</p><p>[Foo*bar]]</p>
```
## [Example 164](https://spec.commonmark.org/0.29/#example-164)

This markdown:

```markdown
[Foo bar]:
<my url>
'title'

[Foo bar]

```

Should give output:

```html
<p><a href="my%20url" title="title">Foo bar</a></p>
```

But instead was:

```html
<p>[Foo bar]</p>
```
## [Example 166](https://spec.commonmark.org/0.29/#example-166)

This markdown:

```markdown
[foo]: /url 'title

with blank line'

[foo]

```

Should give output:

```html
<p>[foo]: /url &#39;title</p><p>with blank line&#39;</p><p>[foo]</p>
```

But instead was:

```html
<p><a href="/url" title="title

with blank line">foo</a></p>
```
## [Example 168](https://spec.commonmark.org/0.29/#example-168)

This markdown:

```markdown
[foo]:

[foo]

```

Should give output:

```html
<p>[foo]:</p><p>[foo]</p>
```

But instead was:

```html

```
## [Example 170](https://spec.commonmark.org/0.29/#example-170)

This markdown:

```markdown
[foo]: <bar>(baz)

[foo]

```

Should give output:

```html
<p>[foo]:<bar>(baz)</p><p>[foo]</p>
```

But instead was:

```html
<p>(baz)</p><p><a href="bar">foo</a></p>
```
## [Example 171](https://spec.commonmark.org/0.29/#example-171)

This markdown:

```markdown
[foo]: /url\bar\*baz "foo\"bar\baz"

[foo]

```

Should give output:

```html
<p><a href="/url%5Cbar*baz" title="foo&quot;bar\baz">foo</a></p>
```

But instead was:

```html
<p>bar\baz&quot;</p><p><a href="/url%5Cbar*baz" title="foo\">foo</a></p>
```
## [Example 173](https://spec.commonmark.org/0.29/#example-173)

This markdown:

```markdown
[foo]

[foo]: first
[foo]: second

```

Should give output:

```html
<p><a href="first">foo</a></p>
```

But instead was:

```html
<p><a href="second">foo</a></p>
```
## [Example 174](https://spec.commonmark.org/0.29/#example-174)

This markdown:

```markdown
[FOO]: /url

[Foo]

```

Should give output:

```html
<p><a href="/url">Foo</a></p>
```

But instead was:

```html
<p>[Foo]</p>
```
## [Example 175](https://spec.commonmark.org/0.29/#example-175)

This markdown:

```markdown
[ΑΓΩ]: /φου

[αγω]

```

Should give output:

```html
<p><a href="/%CF%86%CE%BF%CF%85">αγω</a></p>
```

But instead was:

```html
<p>[αγω]</p>
```
## [Example 178](https://spec.commonmark.org/0.29/#example-178)

This markdown:

```markdown
[foo]: /url "title" ok

```

Should give output:

```html
<p>[foo]: /url &quot;title&quot; ok</p>
```

But instead was:

```html
<p>ok</p>
```
## [Example 179](https://spec.commonmark.org/0.29/#example-179)

This markdown:

```markdown
[foo]: /url
"title" ok

```

Should give output:

```html
<p>&quot;title&quot; ok</p>
```

But instead was:

```html
<p>ok</p>
```
## [Example 182](https://spec.commonmark.org/0.29/#example-182)

This markdown:

```markdown
Foo
[bar]: /baz

[bar]

```

Should give output:

```html
<p>Foo [bar]: /baz</p><p>[bar]</p>
```

But instead was:

```html
<p>Foo<a href="/baz">bar</a></p>
```
## [Example 184](https://spec.commonmark.org/0.29/#example-184)

This markdown:

```markdown
[foo]: /url
bar
===
[foo]

```

Should give output:

```html
<h1>bar</h1><p><a href="/url">foo</a></p>
```

But instead was:

```html
<p>bar ===<a href="/url">foo</a></p>
```
## [Example 187](https://spec.commonmark.org/0.29/#example-187)

This markdown:

```markdown
[foo]

> [foo]: /url

```

Should give output:

```html
<p><a href="/url">foo</a></p><blockquote></blockquote>
```

But instead was:

```html
<p>[foo]</p><blockquote></blockquote>
```

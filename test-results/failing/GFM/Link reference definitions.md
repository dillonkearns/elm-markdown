# GFM - Link reference definitions

## [Example 161](https://github.github.com/gfm/#example-161)

This markdown:

```markdown
[foo]: /url "title"

[foo]

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 162](https://github.github.com/gfm/#example-162)

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
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 163](https://github.github.com/gfm/#example-163)

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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 164](https://github.github.com/gfm/#example-164)

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
ERROR Problem at row 3 Expecting symbol =
```
## [Example 165](https://github.github.com/gfm/#example-165)

This markdown:

```markdown
[foo]: /url '
title
line1
line2
'

[foo]

```

Should give output:

```html
<p><a href="/url" title="
title
line1
line2
">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 14 Expecting Problem at row 1 Expecting symbol (
```
## [Example 166](https://github.github.com/gfm/#example-166)

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
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
## [Example 167](https://github.github.com/gfm/#example-167)

This markdown:

```markdown
[foo]:
/url

[foo]

```

Should give output:

```html
<p><a href="/url">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
## [Example 168](https://github.github.com/gfm/#example-168)

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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 169](https://github.github.com/gfm/#example-169)

This markdown:

```markdown
[foo]: <>

[foo]

```

Should give output:

```html
<p><a href="">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 170](https://github.github.com/gfm/#example-170)

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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 171](https://github.github.com/gfm/#example-171)

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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 172](https://github.github.com/gfm/#example-172)

This markdown:

```markdown
[foo]

[foo]: url

```

Should give output:

```html
<p><a href="url">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 173](https://github.github.com/gfm/#example-173)

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
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
## [Example 174](https://github.github.com/gfm/#example-174)

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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 175](https://github.github.com/gfm/#example-175)

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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 176](https://github.github.com/gfm/#example-176)

This markdown:

```markdown
[foo]: /url

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
## [Example 177](https://github.github.com/gfm/#example-177)

This markdown:

```markdown
[
foo
]: /url
bar

```

Should give output:

```html
<p>bar</p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
## [Example 178](https://github.github.com/gfm/#example-178)

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
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
## [Example 179](https://github.github.com/gfm/#example-179)

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
ERROR Problem at row 5 Expecting Problem at row 1 Expecting symbol (
```
## [Example 180](https://github.github.com/gfm/#example-180)

This markdown:

```markdown
    [foo]: /url "title"

[foo]

```

Should give output:

```html
<pre><code>[foo]: /url &quot;title&quot;</code></pre><p>[foo]</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 181](https://github.github.com/gfm/#example-181)

This markdown:

```markdown
```
[foo]: /url
```

[foo]

```

Should give output:

```html
<pre><code>[foo]: /url</code></pre><p>[foo]</p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
## [Example 182](https://github.github.com/gfm/#example-182)

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
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
## [Example 183](https://github.github.com/gfm/#example-183)

This markdown:

```markdown
# [Foo]
[foo]: /url
> bar

```

Should give output:

```html
<h1><a href="/url">Foo</a></h1><blockquote><p>bar</p></blockquote>
```

But instead was:

```html
ERROR Problem at row 7 Expecting Problem at row 1 Expecting symbol (
```
## [Example 184](https://github.github.com/gfm/#example-184)

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
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
## [Example 185](https://github.github.com/gfm/#example-185)

This markdown:

```markdown
[foo]: /url
===
[foo]

```

Should give output:

```html
<p>===<a href="/url">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 7 Expecting Problem at row 1 Expecting symbol (
```
## [Example 186](https://github.github.com/gfm/#example-186)

This markdown:

```markdown
[foo]: /foo-url "foo"
[bar]: /bar-url
  "bar"
[baz]: /baz-url

[foo],
[bar],
[baz]

```

Should give output:

```html
<p><a href="/foo-url" title="foo">foo</a>,<a href="/bar-url" title="bar">bar</a>,<a href="/baz-url">baz</a></p>
```

But instead was:

```html
ERROR Problem at row 5 Expecting --- Problem at row 5 Expecting *** Problem at row 5 Expecting ___
```
## [Example 187](https://github.github.com/gfm/#example-187)

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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 188](https://github.github.com/gfm/#example-188)

This markdown:

```markdown
[foo]: /url

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```

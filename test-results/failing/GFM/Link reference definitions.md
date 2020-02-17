# GFM - Link reference definitions

## [Example 161](https://spec.commonmark.org/0.29/#example-161)

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
<p>[foo]: /url &quot;title&quot;</p><p>[foo]</p>
```
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
ERROR Problem at row 3 Expecting symbol =
```
## [Example 165](https://spec.commonmark.org/0.29/#example-165)

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
<p>[foo]: /url &#39; title line1 line2 &#39;</p><p>[foo]</p>
```
## [Example 167](https://spec.commonmark.org/0.29/#example-167)

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
<p>[foo]: /url</p><p>[foo]</p>
```
## [Example 169](https://spec.commonmark.org/0.29/#example-169)

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
<p>[foo]: &lt;&gt;</p><p>[foo]</p>
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
ERROR oneOf failed parsing this value:<bar>Parsing failed in the following 2 ways: (1) Expected a but was bar (2) Expected div but was bar (3) Expected th but was bar (4) Expected pre but was bar (5) Expected td but was bar (6) Expected tr but was bar (7) Expected table but was bar
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
<p>[foo]: /url\bar*baz &quot;foo&quot;bar\baz&quot;</p><p>[foo]</p>
```
## [Example 172](https://spec.commonmark.org/0.29/#example-172)

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
<p>[foo]</p><p>[foo]: url</p>
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
<p>[foo]</p><p>[foo]: first [foo]: second</p>
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
<p>[FOO]: /url</p><p>[Foo]</p>
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
<p>[ΑΓΩ]: /φου</p><p>[αγω]</p>
```
## [Example 176](https://spec.commonmark.org/0.29/#example-176)

This markdown:

```markdown
[foo]: /url

```

Should give output:

```html

```

But instead was:

```html
<p>[foo]: /url</p>
```
## [Example 177](https://spec.commonmark.org/0.29/#example-177)

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
<p>[ foo ]: /url bar</p>
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
<p>[foo]: /url &quot;title&quot; ok</p>
```
## [Example 183](https://spec.commonmark.org/0.29/#example-183)

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
<h1>[Foo]</h1><p>[foo]: /url</p><blockquote><p>bar</p></blockquote>
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
<p>[foo]: /url bar === [foo]</p>
```
## [Example 185](https://spec.commonmark.org/0.29/#example-185)

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
<p>[foo]: /url === [foo]</p>
```
## [Example 186](https://spec.commonmark.org/0.29/#example-186)

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
<p>[foo]: /foo-url &quot;foo&quot; [bar]: /bar-url &quot;bar&quot; [baz]: /baz-url</p><p>[foo], [bar], [baz]</p>
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
<p>[foo]</p><blockquote><p>[foo]: /url</p></blockquote>
```
## [Example 188](https://spec.commonmark.org/0.29/#example-188)

This markdown:

```markdown
[foo]: /url

```

Should give output:

```html

```

But instead was:

```html
<p>[foo]: /url</p>
```

# GFM - Links

## [Example 486](https://spec.commonmark.org/0.29/#example-486)

This markdown:

```markdown
[link](</my uri>)

```

Should give output:

```html
<p><a href="/my%20uri">link</a></p>
```

But instead was:

```html
<p>[link](&lt;/my uri&gt;)</p>
```
## [Example 488](https://spec.commonmark.org/0.29/#example-488)

This markdown:

```markdown
[link](<foo
bar>)

```

Should give output:

```html
<p>[link](<foo bar="">)</p>
```

But instead was:

```html
<p>[link](&lt;foo bar&gt;)</p>
```
## [Example 490](https://spec.commonmark.org/0.29/#example-490)

This markdown:

```markdown
[link](<foo\>)

```

Should give output:

```html
<p>[link](&lt;foo&gt;)</p>
```

But instead was:

```html
<p><a href="foo%5C">link</a></p>
```
## [Example 491](https://spec.commonmark.org/0.29/#example-491)

This markdown:

```markdown
[a](<b)c
[a](<b)c>
[a](<b>c)

```

Should give output:

```html
<p>[a](&lt;b)c [a](&lt;b)c&gt; [a](<b>c)</p>
```

But instead was:

```html
<p><a href="%3Cb">a</a>c<a href="%3Cb">a</a>c&gt;<a href="%3Cb%3Ec">a</a></p>
```
## [Example 493](https://spec.commonmark.org/0.29/#example-493)

This markdown:

```markdown
[link](foo(and(bar)))

```

Should give output:

```html
<p><a href="foo(and(bar))">link</a></p>
```

But instead was:

```html
<p>[link](foo(and(bar)))</p>
```
## [Example 502](https://spec.commonmark.org/0.29/#example-502)

This markdown:

```markdown
[link](/url "title \"&quot;")

```

Should give output:

```html
<p><a href="/url" title="title &quot;&quot;">link</a></p>
```

But instead was:

```html
<p><a \""="" href="/url" title="title \">link</a></p>
```
## [Example 505](https://spec.commonmark.org/0.29/#example-505)

This markdown:

```markdown
[link](/url 'title "and" title')

```

Should give output:

```html
<p><a href="/url" title="title &quot;and&quot; title">link</a></p>
```

But instead was:

```html
<p><a and\"="" href="/url" title="title \" title"="">link</a></p>
```
## [Example 520](https://spec.commonmark.org/0.29/#example-520)

This markdown:

```markdown
[foo <bar attr="](baz)">

```

Should give output:

```html
<p>[foo<bar attr="](baz)"></p>
```

But instead was:

```html
<p><a href="baz">foo &lt;bar attr=&quot;</a>&quot;&gt;</p>
```
## [Example 523](https://spec.commonmark.org/0.29/#example-523)

This markdown:

```markdown
[foo][bar]

[bar]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
<p>[foo][bar]</p><p>[bar]: /url &quot;title&quot;</p>
```
## [Example 524](https://spec.commonmark.org/0.29/#example-524)

This markdown:

```markdown
[link [foo [bar]]][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri">link [foo [bar]]</a></p>
```

But instead was:

```html
<p>[link [foo [bar]]][ref]</p><p>[ref]: /uri</p>
```
## [Example 525](https://spec.commonmark.org/0.29/#example-525)

This markdown:

```markdown
[link \[bar][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri">link [bar</a></p>
```

But instead was:

```html
<p>[link [bar][ref]</p><p>[ref]: /uri</p>
```
## [Example 526](https://spec.commonmark.org/0.29/#example-526)

This markdown:

```markdown
[link *foo **bar** `#`*][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri">link<em>foo<strong>bar</strong><code>#</code></em></a></p>
```

But instead was:

```html
<p>[link<em>foo<strong>bar</strong><code>#</code></em>][ref]</p><p>[ref]: /uri</p>
```
## [Example 527](https://spec.commonmark.org/0.29/#example-527)

This markdown:

```markdown
[![moon](moon.jpg)][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri"><img alt="moon" src="moon.jpg"></a></p>
```

But instead was:

```html
<p>[<img alt="moon" src="moon.jpg">][ref]</p><p>[ref]: /uri</p>
```
## [Example 528](https://spec.commonmark.org/0.29/#example-528)

This markdown:

```markdown
[foo [bar](/uri)][ref]

[ref]: /uri

```

Should give output:

```html
<p>[foo<a href="/uri">bar</a>]<a href="/uri">ref</a></p>
```

But instead was:

```html
<p>[foo<a href="/uri">bar</a>][ref]</p><p>[ref]: /uri</p>
```
## [Example 529](https://spec.commonmark.org/0.29/#example-529)

This markdown:

```markdown
[foo *bar [baz][ref]*][ref]

[ref]: /uri

```

Should give output:

```html
<p>[foo<em>bar<a href="/uri">baz</a></em>]<a href="/uri">ref</a></p>
```

But instead was:

```html
<p>[foo<em>bar [baz][ref]</em>][ref]</p><p>[ref]: /uri</p>
```
## [Example 530](https://spec.commonmark.org/0.29/#example-530)

This markdown:

```markdown
*[foo*][ref]

[ref]: /uri

```

Should give output:

```html
<p>*<a href="/uri">foo*</a></p>
```

But instead was:

```html
<p><em>[foo</em>][ref]</p><p>[ref]: /uri</p>
```
## [Example 531](https://spec.commonmark.org/0.29/#example-531)

This markdown:

```markdown
[foo *bar][ref]

[ref]: /uri

```

Should give output:

```html
<p><a href="/uri">foo *bar</a></p>
```

But instead was:

```html
<p>[foo *bar][ref]</p><p>[ref]: /uri</p>
```
## [Example 532](https://spec.commonmark.org/0.29/#example-532)

This markdown:

```markdown
[foo <bar attr="][ref]">

[ref]: /uri

```

Should give output:

```html
<p>[foo<bar attr="][ref]"></p>
```

But instead was:

```html
<p>[foo &lt;bar attr=&quot;][ref]&quot;&gt;</p><p>[ref]: /uri</p>
```
## [Example 533](https://spec.commonmark.org/0.29/#example-533)

This markdown:

```markdown
[foo`][ref]`

[ref]: /uri

```

Should give output:

```html
<p>[foo<code>][ref]</code></p>
```

But instead was:

```html
<p>[foo<code>][ref]</code></p><p>[ref]: /uri</p>
```
## [Example 534](https://spec.commonmark.org/0.29/#example-534)

This markdown:

```markdown
[foo<http://example.com/?search=][ref]>

[ref]: /uri

```

Should give output:

```html
<p>[foo<a href="http://example.com/?search=%5D%5Bref%5D">http://example.com/?search=][ref]</a></p>
```

But instead was:

```html
<p>[foo<a href="http://example.com/?search=%5D%5Bref%5D">http://example.com/?search=][ref]</a></p><p>[ref]: /uri</p>
```
## [Example 535](https://spec.commonmark.org/0.29/#example-535)

This markdown:

```markdown
[foo][BaR]

[bar]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
<p>[foo][BaR]</p><p>[bar]: /url &quot;title&quot;</p>
```
## [Example 536](https://spec.commonmark.org/0.29/#example-536)

This markdown:

```markdown
[Толпой][Толпой] is a Russian word.

[ТОЛПОЙ]: /url

```

Should give output:

```html
<p><a href="/url">Толпой</a>is a Russian word.</p>
```

But instead was:

```html
<p>[Толпой][Толпой] is a Russian word.</p><p>[ТОЛПОЙ]: /url</p>
```
## [Example 537](https://spec.commonmark.org/0.29/#example-537)

This markdown:

```markdown
[Foo
  bar]: /url

[Baz][Foo bar]

```

Should give output:

```html
<p><a href="/url">Baz</a></p>
```

But instead was:

```html
<p>[Foo bar]: /url</p><p>[Baz][Foo bar]</p>
```
## [Example 538](https://spec.commonmark.org/0.29/#example-538)

This markdown:

```markdown
[foo] [bar]

[bar]: /url "title"

```

Should give output:

```html
<p>[foo]<a href="/url" title="title">bar</a></p>
```

But instead was:

```html
<p>[foo] [bar]</p><p>[bar]: /url &quot;title&quot;</p>
```
## [Example 539](https://spec.commonmark.org/0.29/#example-539)

This markdown:

```markdown
[foo]
[bar]

[bar]: /url "title"

```

Should give output:

```html
<p>[foo]<a href="/url" title="title">bar</a></p>
```

But instead was:

```html
<p>[foo] [bar]</p><p>[bar]: /url &quot;title&quot;</p>
```
## [Example 540](https://spec.commonmark.org/0.29/#example-540)

This markdown:

```markdown
[foo]: /url1

[foo]: /url2

[bar][foo]

```

Should give output:

```html
<p><a href="/url1">bar</a></p>
```

But instead was:

```html
<p>[foo]: /url1</p><p>[foo]: /url2</p><p>[bar][foo]</p>
```
## [Example 541](https://spec.commonmark.org/0.29/#example-541)

This markdown:

```markdown
[bar][foo\!]

[foo!]: /url

```

Should give output:

```html
<p>[bar][foo!]</p>
```

But instead was:

```html
<p>[bar][foo!]</p><p>[foo!]: /url</p>
```
## [Example 545](https://spec.commonmark.org/0.29/#example-545)

This markdown:

```markdown
[foo][ref\[]

[ref\[]: /uri

```

Should give output:

```html
<p><a href="/uri">foo</a></p>
```

But instead was:

```html
<p>[foo][ref[]</p><p>[ref[]: /uri</p>
```
## [Example 546](https://spec.commonmark.org/0.29/#example-546)

This markdown:

```markdown
[bar\\]: /uri

[bar\\]

```

Should give output:

```html
<p><a href="/uri">bar\</a></p>
```

But instead was:

```html
<p>[bar\]: /uri</p><p>[bar\]</p>
```
## [Example 549](https://spec.commonmark.org/0.29/#example-549)

This markdown:

```markdown
[foo][]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
<p>[foo][]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 550](https://spec.commonmark.org/0.29/#example-550)

This markdown:

```markdown
[*foo* bar][]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title"><em>foo</em>bar</a></p>
```

But instead was:

```html
<p>[<em>foo</em>bar][]</p><p>[<em>foo</em>bar]: /url &quot;title&quot;</p>
```
## [Example 551](https://spec.commonmark.org/0.29/#example-551)

This markdown:

```markdown
[Foo][]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">Foo</a></p>
```

But instead was:

```html
<p>[Foo][]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 552](https://spec.commonmark.org/0.29/#example-552)

This markdown:

```markdown
[foo] 
[]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a>[]</p>
```

But instead was:

```html
<p>[foo] []</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 553](https://spec.commonmark.org/0.29/#example-553)

This markdown:

```markdown
[foo]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">foo</a></p>
```

But instead was:

```html
<p>[foo]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 554](https://spec.commonmark.org/0.29/#example-554)

This markdown:

```markdown
[*foo* bar]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title"><em>foo</em>bar</a></p>
```

But instead was:

```html
<p>[<em>foo</em>bar]</p><p>[<em>foo</em>bar]: /url &quot;title&quot;</p>
```
## [Example 555](https://spec.commonmark.org/0.29/#example-555)

This markdown:

```markdown
[[*foo* bar]]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p>[<a href="/url" title="title"><em>foo</em>bar</a>]</p>
```

But instead was:

```html
<p>[[<em>foo</em>bar]]</p><p>[<em>foo</em>bar]: /url &quot;title&quot;</p>
```
## [Example 556](https://spec.commonmark.org/0.29/#example-556)

This markdown:

```markdown
[[bar [foo]

[foo]: /url

```

Should give output:

```html
<p>[[bar<a href="/url">foo</a></p>
```

But instead was:

```html
<p>[[bar [foo]</p><p>[foo]: /url</p>
```
## [Example 557](https://spec.commonmark.org/0.29/#example-557)

This markdown:

```markdown
[Foo]

[foo]: /url "title"

```

Should give output:

```html
<p><a href="/url" title="title">Foo</a></p>
```

But instead was:

```html
<p>[Foo]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 558](https://spec.commonmark.org/0.29/#example-558)

This markdown:

```markdown
[foo] bar

[foo]: /url

```

Should give output:

```html
<p><a href="/url">foo</a>bar</p>
```

But instead was:

```html
<p>[foo] bar</p><p>[foo]: /url</p>
```
## [Example 559](https://spec.commonmark.org/0.29/#example-559)

This markdown:

```markdown
\[foo]

[foo]: /url "title"

```

Should give output:

```html
<p>[foo]</p>
```

But instead was:

```html
<p>[foo]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 560](https://spec.commonmark.org/0.29/#example-560)

This markdown:

```markdown
[foo*]: /url

*[foo*]

```

Should give output:

```html
<p>*<a href="/url">foo*</a></p>
```

But instead was:

```html
<p>[foo*]: /url</p><p><em>[foo</em>]</p>
```
## [Example 561](https://spec.commonmark.org/0.29/#example-561)

This markdown:

```markdown
[foo][bar]

[foo]: /url1
[bar]: /url2

```

Should give output:

```html
<p><a href="/url2">foo</a></p>
```

But instead was:

```html
<p>[foo][bar]</p><p>[foo]: /url1 [bar]: /url2</p>
```
## [Example 562](https://spec.commonmark.org/0.29/#example-562)

This markdown:

```markdown
[foo][]

[foo]: /url1

```

Should give output:

```html
<p><a href="/url1">foo</a></p>
```

But instead was:

```html
<p>[foo][]</p><p>[foo]: /url1</p>
```
## [Example 563](https://spec.commonmark.org/0.29/#example-563)

This markdown:

```markdown
[foo]()

[foo]: /url1

```

Should give output:

```html
<p><a href="">foo</a></p>
```

But instead was:

```html
<p><a href="">foo</a></p><p>[foo]: /url1</p>
```
## [Example 564](https://spec.commonmark.org/0.29/#example-564)

This markdown:

```markdown
[foo](not a link)

[foo]: /url1

```

Should give output:

```html
<p><a href="/url1">foo</a>(not a link)</p>
```

But instead was:

```html
<p>[foo](not a link)</p><p>[foo]: /url1</p>
```
## [Example 565](https://spec.commonmark.org/0.29/#example-565)

This markdown:

```markdown
[foo][bar][baz]

[baz]: /url

```

Should give output:

```html
<p>[foo]<a href="/url">bar</a></p>
```

But instead was:

```html
<p>[foo][bar][baz]</p><p>[baz]: /url</p>
```
## [Example 566](https://spec.commonmark.org/0.29/#example-566)

This markdown:

```markdown
[foo][bar][baz]

[baz]: /url1
[bar]: /url2

```

Should give output:

```html
<p><a href="/url2">foo</a><a href="/url1">baz</a></p>
```

But instead was:

```html
<p>[foo][bar][baz]</p><p>[baz]: /url1 [bar]: /url2</p>
```
## [Example 567](https://spec.commonmark.org/0.29/#example-567)

This markdown:

```markdown
[foo][bar][baz]

[baz]: /url1
[foo]: /url2

```

Should give output:

```html
<p>[foo]<a href="/url1">bar</a></p>
```

But instead was:

```html
<p>[foo][bar][baz]</p><p>[baz]: /url1 [foo]: /url2</p>
```

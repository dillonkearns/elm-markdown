# GFM - Links

## [Example 481](https://spec.commonmark.org/0.29/#example-481)

This markdown:

```markdown
[link](/uri "title")

```

Should give output:

```html
<p><a href="/uri" title="title">link</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
```
## [Example 485](https://spec.commonmark.org/0.29/#example-485)

This markdown:

```markdown
[link](/my uri)

```

Should give output:

```html
<p>[link](/my uri)</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
```
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
<p><a href="/my uri">link</a></p>
```
## [Example 487](https://spec.commonmark.org/0.29/#example-487)

This markdown:

```markdown
[link](foo
bar)

```

Should give output:

```html
<p>[link](foo bar)</p>
```

But instead was:

```html
ERROR Problem at row 5 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
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
<p><a href="foo bar">link</a></p>
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
<p><a href="foo\">link</a></p>
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
ERROR Problem at row 7 Expecting Problem at row 1 Expecting symbol )
```
## [Example 492](https://spec.commonmark.org/0.29/#example-492)

This markdown:

```markdown
[link](\(foo\))

```

Should give output:

```html
<p><a href="(foo)">link</a></p>
```

But instead was:

```html
<p><a href="\(foo\">link</a>)</p>
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
<p><a href="foo(and(bar">link</a>))</p>
```
## [Example 494](https://spec.commonmark.org/0.29/#example-494)

This markdown:

```markdown
[link](foo\(and\(bar\))

```

Should give output:

```html
<p><a href="foo(and(bar)">link</a></p>
```

But instead was:

```html
<p><a href="foo\(and\(bar\">link</a>)</p>
```
## [Example 496](https://spec.commonmark.org/0.29/#example-496)

This markdown:

```markdown
[link](foo\)\:)

```

Should give output:

```html
<p><a href="foo):">link</a></p>
```

But instead was:

```html
<p><a href="foo\">link</a>\:)</p>
```
## [Example 498](https://spec.commonmark.org/0.29/#example-498)

This markdown:

```markdown
[link](foo\bar)

```

Should give output:

```html
<p><a href="foo%5Cbar">link</a></p>
```

But instead was:

```html
<p><a href="foo\bar">link</a></p>
```
## [Example 499](https://spec.commonmark.org/0.29/#example-499)

This markdown:

```markdown
[link](foo%20b&auml;)

```

Should give output:

```html
<p><a href="foo%20b%C3%A4">link</a></p>
```

But instead was:

```html
<p><a href="foo%20bä">link</a></p>
```
## [Example 500](https://spec.commonmark.org/0.29/#example-500)

This markdown:

```markdown
[link]("title")

```

Should give output:

```html
<p><a href="%22title%22">link</a></p>
```

But instead was:

```html
<p><a href="\" title\""="">link</a></p>
```
## [Example 501](https://spec.commonmark.org/0.29/#example-501)

This markdown:

```markdown
[link](/url "title")
[link](/url 'title')
[link](/url (title))

```

Should give output:

```html
<p><a href="/url" title="title">link</a><a href="/url" title="title">link</a><a href="/url" title="title">link</a></p>
```

But instead was:

```html
ERROR Problem at row 7 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
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
ERROR Problem at row 3 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
```
## [Example 503](https://spec.commonmark.org/0.29/#example-503)

This markdown:

```markdown
[link](/url "title")

```

Should give output:

```html
<p><a href="/url%C2%A0%22title%22">link</a></p>
```

But instead was:

```html
<p><a href="/url \" title\""="">link</a></p>
```
## [Example 504](https://spec.commonmark.org/0.29/#example-504)

This markdown:

```markdown
[link](/url "title "and" title")

```

Should give output:

```html
<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
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
ERROR Problem at row 3 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
```
## [Example 506](https://spec.commonmark.org/0.29/#example-506)

This markdown:

```markdown
[link](   /uri
  "title"  )

```

Should give output:

```html
<p><a href="/uri" title="title">link</a></p>
```

But instead was:

```html
ERROR Problem at row 5 Expecting Problem at row 1 Link destinations can&#39;t contain whitespace, if you would like to include them please wrap your URL with &lt; .. &gt;
```
## [Example 507](https://spec.commonmark.org/0.29/#example-507)

This markdown:

```markdown
[link] (/uri)

```

Should give output:

```html
<p>[link] (/uri)</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
## [Example 508](https://spec.commonmark.org/0.29/#example-508)

This markdown:

```markdown
[link [foo [bar]]](/uri)

```

Should give output:

```html
<p><a href="/uri">link [foo [bar]]</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
## [Example 509](https://spec.commonmark.org/0.29/#example-509)

This markdown:

```markdown
[link] bar](/uri)

```

Should give output:

```html
<p>[link] bar](/uri)</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
```
## [Example 510](https://spec.commonmark.org/0.29/#example-510)

This markdown:

```markdown
[link [bar](/uri)

```

Should give output:

```html
<p>[link<a href="/uri">bar</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
## [Example 511](https://spec.commonmark.org/0.29/#example-511)

This markdown:

```markdown
[link \[bar](/uri)

```

Should give output:

```html
<p><a href="/uri">link [bar</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
## [Example 512](https://spec.commonmark.org/0.29/#example-512)

This markdown:

```markdown
[link *foo **bar** `#`*](/uri)

```

Should give output:

```html
<p><a href="/uri">link<em>foo<strong>bar</strong><code>#</code></em></a></p>
```

But instead was:

```html
<p><a href="/uri">link<em>foo</em><strong>bar</strong><em></em><em>#</em></a></p>
```
## [Example 513](https://spec.commonmark.org/0.29/#example-513)

This markdown:

```markdown
[![moon](moon.jpg)](/uri)

```

Should give output:

```html
<p><a href="/uri"><img alt="moon" src="moon.jpg"></a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
## [Example 514](https://spec.commonmark.org/0.29/#example-514)

This markdown:

```markdown
[foo [bar](/uri)](/uri)

```

Should give output:

```html
<p>[foo<a href="/uri">bar</a>](/uri)</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
## [Example 515](https://spec.commonmark.org/0.29/#example-515)

This markdown:

```markdown
[foo *[bar [baz](/uri)](/uri)*](/uri)

```

Should give output:

```html
<p>[foo<em>[bar<a href="/uri">baz</a>](/uri)</em>](/uri)</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
## [Example 516](https://spec.commonmark.org/0.29/#example-516)

This markdown:

```markdown
![[[foo](uri1)](uri2)](uri3)

```

Should give output:

```html
<p><img alt="[foo](uri2)" src="uri3"></p>
```

But instead was:

```html
<p><img src="uri1">](uri2)](uri3)</p>
```
## [Example 517](https://spec.commonmark.org/0.29/#example-517)

This markdown:

```markdown
*[foo*](/uri)

```

Should give output:

```html
<p>*<a href="/uri">foo*</a></p>
```

But instead was:

```html
<p><a href="/uri">foo</a></p>
```
## [Example 518](https://spec.commonmark.org/0.29/#example-518)

This markdown:

```markdown
[foo *bar](baz*)

```

Should give output:

```html
<p><a href="baz*">foo *bar</a></p>
```

But instead was:

```html
<p><a href="baz*">foo<em>bar</em></a></p>
```
## [Example 519](https://spec.commonmark.org/0.29/#example-519)

This markdown:

```markdown
*foo [bar* baz]

```

Should give output:

```html
<p><em>foo [bar</em>baz]</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting symbol (
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
## [Example 521](https://spec.commonmark.org/0.29/#example-521)

This markdown:

```markdown
[foo`](/uri)`

```

Should give output:

```html
<p>[foo<code>](/uri)</code></p>
```

But instead was:

```html
<p><a href="/uri">foo</a></p>
```
## [Example 522](https://spec.commonmark.org/0.29/#example-522)

This markdown:

```markdown
[foo<http://example.com/?search=](uri)>

```

Should give output:

```html
<p>[foo<a href="http://example.com/?search=%5D(uri)">http://example.com/?search=](uri)</a></p>
```

But instead was:

```html
<p><a href="uri">foo&lt;http://example.com/?search=</a>&gt;</p>
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 542](https://spec.commonmark.org/0.29/#example-542)

This markdown:

```markdown
[foo][ref[]

[ref[]: /uri

```

Should give output:

```html
<p>[foo][ref[]</p><p>[ref[]: /uri</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 543](https://spec.commonmark.org/0.29/#example-543)

This markdown:

```markdown
[foo][ref[bar]]

[ref[bar]]: /uri

```

Should give output:

```html
<p>[foo][ref[bar]]</p><p>[ref[bar]]: /uri</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 544](https://spec.commonmark.org/0.29/#example-544)

This markdown:

```markdown
[[[foo]]]

[[[foo]]]: /url

```

Should give output:

```html
<p>[[[foo]]]</p><p>[[[foo]]]: /url</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 547](https://spec.commonmark.org/0.29/#example-547)

This markdown:

```markdown
[]

[]: /uri

```

Should give output:

```html
<p>[]</p><p>[]: /uri</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 548](https://spec.commonmark.org/0.29/#example-548)

This markdown:

```markdown
[
 ]

[
 ]: /uri

```

Should give output:

```html
<p>[ ]</p><p>[ ]: /uri</p>
```

But instead was:

```html
ERROR Problem at row 10 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
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
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```

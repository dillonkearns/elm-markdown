# GFM - Links

## [Example 488](https://spec.commonmark.org/0.29/#example-488)

This markdown:

````````````markdown
[link](<foo
bar>)

````````````

Should give output:

````````````html
<p>[link](<foo bar="">)</p>
````````````

But instead was:

````````````html
<p>[link](&lt;foo bar&gt;)</p>
````````````
## [Example 490](https://spec.commonmark.org/0.29/#example-490)

This markdown:

````````````markdown
[link](<foo\>)

````````````

Should give output:

````````````html
<p>[link](&lt;foo&gt;)</p>
````````````

But instead was:

````````````html
<p><a href="foo%5C">link</a></p>
````````````
## [Example 491](https://spec.commonmark.org/0.29/#example-491)

This markdown:

````````````markdown
[a](<b)c
[a](<b)c>
[a](<b>c)

````````````

Should give output:

````````````html
<p>[a](&lt;b)c [a](&lt;b)c&gt; [a](<b>c)</p>
````````````

But instead was:

````````````html
<p><a href="%3Cb">a</a>c<a href="%3Cb">a</a>c&gt;<a href="%3Cb%3Ec">a</a></p>
````````````
## [Example 493](https://spec.commonmark.org/0.29/#example-493)

This markdown:

````````````markdown
[link](foo(and(bar)))

````````````

Should give output:

````````````html
<p><a href="foo(and(bar))">link</a></p>
````````````

But instead was:

````````````html
<p>[link](foo(and(bar)))</p>
````````````
## [Example 502](https://spec.commonmark.org/0.29/#example-502)

This markdown:

````````````markdown
[link](/url "title \"&quot;")

````````````

Should give output:

````````````html
<p><a href="/url" title="title &quot;&quot;">link</a></p>
````````````

But instead was:

````````````html
<p><a \""="" href="/url" title="title \">link</a></p>
````````````
## [Example 505](https://spec.commonmark.org/0.29/#example-505)

This markdown:

````````````markdown
[link](/url 'title "and" title')

````````````

Should give output:

````````````html
<p><a href="/url" title="title &quot;and&quot; title">link</a></p>
````````````

But instead was:

````````````html
<p><a and\"="" href="/url" title="title \" title"="">link</a></p>
````````````
## [Example 520](https://spec.commonmark.org/0.29/#example-520)

This markdown:

````````````markdown
[foo <bar attr="](baz)">

````````````

Should give output:

````````````html
<p>[foo<bar attr="](baz)"></p>
````````````

But instead was:

````````````html
<p><a href="baz">foo &lt;bar attr=&quot;</a>&quot;&gt;</p>
````````````
## [Example 532](https://spec.commonmark.org/0.29/#example-532)

This markdown:

````````````markdown
[foo <bar attr="][ref]">

[ref]: /uri

````````````

Should give output:

````````````html
<p>[foo<bar attr="][ref]"></p>
````````````

But instead was:

````````````html
<p><a href="/uri">foo &lt;bar attr=&quot;</a>&quot;&gt;</p>
````````````
## [Example 537](https://spec.commonmark.org/0.29/#example-537)

This markdown:

````````````markdown
[Foo
  bar]: /url

[Baz][Foo bar]

````````````

Should give output:

````````````html
<p><a href="/url">Baz</a></p>
````````````

But instead was:

````````````html
<p>[Baz][Foo bar]</p>
````````````
## [Example 542](https://spec.commonmark.org/0.29/#example-542)

This markdown:

````````````markdown
[foo][ref[]

[ref[]: /uri

````````````

Should give output:

````````````html
<p>[foo][ref[]</p><p>[ref[]: /uri</p>
````````````

But instead was:

````````````html
<p>[foo][ref[]</p>
````````````
## [Example 547](https://spec.commonmark.org/0.29/#example-547)

This markdown:

````````````markdown
[]

[]: /uri

````````````

Should give output:

````````````html
<p>[]</p><p>[]: /uri</p>
````````````

But instead was:

````````````html
<p><a href="/uri"></a></p>
````````````
## [Example 548](https://spec.commonmark.org/0.29/#example-548)

This markdown:

````````````markdown
[
 ]

[
 ]: /uri

````````````

Should give output:

````````````html
<p>[ ]</p><p>[ ]: /uri</p>
````````````

But instead was:

````````````html
<p><a href="/uri"></a></p>
````````````

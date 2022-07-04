# CommonMark - Links

## [Example 490](https://spec.commonmark.org/0.30/#example-490)

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
## [Example 492](https://spec.commonmark.org/0.30/#example-492)

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
## [Example 493](https://spec.commonmark.org/0.30/#example-493)

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
## [Example 495](https://spec.commonmark.org/0.30/#example-495)

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
## [Example 505](https://spec.commonmark.org/0.30/#example-505)

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
## [Example 508](https://spec.commonmark.org/0.30/#example-508)

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
## [Example 523](https://spec.commonmark.org/0.30/#example-523)

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
## [Example 535](https://spec.commonmark.org/0.30/#example-535)

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
## [Example 539](https://spec.commonmark.org/0.30/#example-539)

This markdown:

````````````markdown
[ẞ]

[SS]: /url

````````````

Should give output:

````````````html
<p><a href="/url">ẞ</a></p>
````````````

But instead was:

````````````html
<p>[ẞ]</p>
````````````
## [Example 540](https://spec.commonmark.org/0.30/#example-540)

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
## [Example 545](https://spec.commonmark.org/0.30/#example-545)

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
## [Example 550](https://spec.commonmark.org/0.30/#example-550)

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
## [Example 551](https://spec.commonmark.org/0.30/#example-551)

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

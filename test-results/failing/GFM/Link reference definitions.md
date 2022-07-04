# GFM - Link reference definitions

## [Example 194](https://spec.commonmark.org/0.30/#example-194)

This markdown:

````````````markdown
[Foo*bar\]]:my_(url) 'title (with parens)'

[Foo*bar\]]

````````````

Should give output:

````````````html
<p><a href="my_(url)" title="title (with parens)">Foo*bar]</a></p>
````````````

But instead was:

````````````html
<p>[Foo*bar]]:my_(url) &#39;title (with parens)&#39;</p><p>[Foo*bar]]</p>
````````````
## [Example 201](https://spec.commonmark.org/0.30/#example-201)

This markdown:

````````````markdown
[foo]: <bar>(baz)

[foo]

````````````

Should give output:

````````````html
<p>[foo]:<bar>(baz)</p><p>[foo]</p>
````````````

But instead was:

````````````html
<p>[foo]: &lt;bar&gt;(baz)</p><p>[foo]</p>
````````````
## [Example 202](https://spec.commonmark.org/0.30/#example-202)

This markdown:

````````````markdown
[foo]: /url\bar\*baz "foo\"bar\baz"

[foo]

````````````

Should give output:

````````````html
<p><a href="/url%5Cbar*baz" title="foo&quot;bar\baz">foo</a></p>
````````````

But instead was:

````````````html
<p>[foo]: /url\bar*baz &quot;foo&quot;bar\baz&quot;</p><p>[foo]</p>
````````````
## [Example 213](https://spec.commonmark.org/0.30/#example-213)

This markdown:

````````````markdown
Foo
[bar]: /baz

[bar]

````````````

Should give output:

````````````html
<p>Foo [bar]: /baz</p><p>[bar]</p>
````````````

But instead was:

````````````html
<p>Foo<a href="/baz">bar</a></p>
````````````

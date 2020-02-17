# GFM - Images

## [Example 569](https://spec.commonmark.org/0.29/#example-569)

This markdown:

```markdown
![foo *bar*]

[foo *bar*]: train.jpg "train & tracks"

```

Should give output:

```html
<p><img alt="foo bar" src="train.jpg" title="train &amp; tracks"></p>
```

But instead was:

```html
<p>![foo<em>bar</em>]</p><p>[foo<em>bar</em>]: train.jpg &quot;train &amp; tracks&quot;</p>
```
## [Example 572](https://spec.commonmark.org/0.29/#example-572)

This markdown:

```markdown
![foo *bar*][]

[foo *bar*]: train.jpg "train & tracks"

```

Should give output:

```html
<p><img alt="foo bar" src="train.jpg" title="train &amp; tracks"></p>
```

But instead was:

```html
<p>![foo<em>bar</em>][]</p><p>[foo<em>bar</em>]: train.jpg &quot;train &amp; tracks&quot;</p>
```
## [Example 573](https://spec.commonmark.org/0.29/#example-573)

This markdown:

```markdown
![foo *bar*][foobar]

[FOOBAR]: train.jpg "train & tracks"

```

Should give output:

```html
<p><img alt="foo bar" src="train.jpg" title="train &amp; tracks"></p>
```

But instead was:

```html
<p>![foo<em>bar</em>][foobar]</p><p>[FOOBAR]: train.jpg &quot;train &amp; tracks&quot;</p>
```
## [Example 578](https://spec.commonmark.org/0.29/#example-578)

This markdown:

```markdown
![foo][bar]

[bar]: /url

```

Should give output:

```html
<p><img alt="foo" src="/url"></p>
```

But instead was:

```html
<p>![foo][bar]</p><p>[bar]: /url</p>
```
## [Example 579](https://spec.commonmark.org/0.29/#example-579)

This markdown:

```markdown
![foo][bar]

[BAR]: /url

```

Should give output:

```html
<p><img alt="foo" src="/url"></p>
```

But instead was:

```html
<p>![foo][bar]</p><p>[BAR]: /url</p>
```
## [Example 580](https://spec.commonmark.org/0.29/#example-580)

This markdown:

```markdown
![foo][]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="foo" src="/url" title="title"></p>
```

But instead was:

```html
<p>![foo][]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 581](https://spec.commonmark.org/0.29/#example-581)

This markdown:

```markdown
![*foo* bar][]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p><img alt="foo bar" src="/url" title="title"></p>
```

But instead was:

```html
<p>![<em>foo</em>bar][]</p><p>[<em>foo</em>bar]: /url &quot;title&quot;</p>
```
## [Example 582](https://spec.commonmark.org/0.29/#example-582)

This markdown:

```markdown
![Foo][]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="Foo" src="/url" title="title"></p>
```

But instead was:

```html
<p>![Foo][]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 583](https://spec.commonmark.org/0.29/#example-583)

This markdown:

```markdown
![foo] 
[]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="foo" src="/url" title="title">[]</p>
```

But instead was:

```html
<p>![foo] []</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 584](https://spec.commonmark.org/0.29/#example-584)

This markdown:

```markdown
![foo]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="foo" src="/url" title="title"></p>
```

But instead was:

```html
<p>![foo]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 585](https://spec.commonmark.org/0.29/#example-585)

This markdown:

```markdown
![*foo* bar]

[*foo* bar]: /url "title"

```

Should give output:

```html
<p><img alt="foo bar" src="/url" title="title"></p>
```

But instead was:

```html
<p>![<em>foo</em>bar]</p><p>[<em>foo</em>bar]: /url &quot;title&quot;</p>
```
## [Example 587](https://spec.commonmark.org/0.29/#example-587)

This markdown:

```markdown
![Foo]

[foo]: /url "title"

```

Should give output:

```html
<p><img alt="Foo" src="/url" title="title"></p>
```

But instead was:

```html
<p>![Foo]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 588](https://spec.commonmark.org/0.29/#example-588)

This markdown:

```markdown
!\[foo]

[foo]: /url "title"

```

Should give output:

```html
<p>![foo]</p>
```

But instead was:

```html
<p>![foo]</p><p>[foo]: /url &quot;title&quot;</p>
```
## [Example 589](https://spec.commonmark.org/0.29/#example-589)

This markdown:

```markdown
\![foo]

[foo]: /url "title"

```

Should give output:

```html
<p>!<a href="/url" title="title">foo</a></p>
```

But instead was:

```html
<p>![foo]</p><p>[foo]: /url &quot;title&quot;</p>
```

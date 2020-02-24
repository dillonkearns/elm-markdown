# GFM - Images

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
<p>![foo<em>bar</em>][foobar]</p>
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

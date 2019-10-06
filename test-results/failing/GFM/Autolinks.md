# GFM - Autolinks

## [Example 590](https://github.github.com/gfm/#example-590)

This markdown:

```markdown
<http://foo.bar.baz>

```

Should give output:

```html
<p><a href="http://foo.bar.baz">http://foo.bar.baz</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## [Example 591](https://github.github.com/gfm/#example-591)

This markdown:

```markdown
<http://foo.bar.baz/test?q=hello&id=22&boolean>

```

Should give output:

```html
<p><a href="http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## [Example 592](https://github.github.com/gfm/#example-592)

This markdown:

```markdown
<irc://foo.bar:2233/baz>

```

Should give output:

```html
<p><a href="irc://foo.bar:2233/baz">irc://foo.bar:2233/baz</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## [Example 593](https://github.github.com/gfm/#example-593)

This markdown:

```markdown
<MAILTO:FOO@BAR.BAZ>

```

Should give output:

```html
<p><a href="MAILTO:FOO@BAR.BAZ">MAILTO:FOO@BAR.BAZ</a></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
## [Example 594](https://github.github.com/gfm/#example-594)

This markdown:

```markdown
<a+b+c:d>

```

Should give output:

```html
<p><a href="a+b+c:d">a+b+c:d</a></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
## [Example 595](https://github.github.com/gfm/#example-595)

This markdown:

```markdown
<made-up-scheme://foo,bar>

```

Should give output:

```html
<p><a href="made-up-scheme://foo,bar">made-up-scheme://foo,bar</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## [Example 596](https://github.github.com/gfm/#example-596)

This markdown:

```markdown
<http://../>

```

Should give output:

```html
<p><a href="http://../">http://../</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## [Example 597](https://github.github.com/gfm/#example-597)

This markdown:

```markdown
<localhost:5001/foo>

```

Should give output:

```html
<p><a href="localhost:5001/foo">localhost:5001/foo</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## [Example 598](https://github.github.com/gfm/#example-598)

This markdown:

```markdown
<http://foo.bar/baz bim>

```

Should give output:

```html
<p>&lt;http://foo.bar/baz bim&gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## [Example 599](https://github.github.com/gfm/#example-599)

This markdown:

```markdown
<http://example.com/\[\>

```

Should give output:

```html
<p><a href="http://example.com/%5C%5B%5C">http://example.com/\[\</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
```
## [Example 600](https://github.github.com/gfm/#example-600)

This markdown:

```markdown
<foo@bar.example.com>

```

Should give output:

```html
<p><a href="mailto:foo@bar.example.com">foo@bar.example.com</a></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
## [Example 601](https://github.github.com/gfm/#example-601)

This markdown:

```markdown
<foo+special@Bar.baz-bar0.com>

```

Should give output:

```html
<p><a href="mailto:foo+special@Bar.baz-bar0.com">foo+special@Bar.baz-bar0.com</a></p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
## [Example 602](https://github.github.com/gfm/#example-602)

This markdown:

```markdown
<foo\+@bar.example.com>

```

Should give output:

```html
<p>&lt;foo+@bar.example.com&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
## [Example 603](https://github.github.com/gfm/#example-603)

This markdown:

```markdown
<>

```

Should give output:

```html
<p>&lt;&gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
## [Example 604](https://github.github.com/gfm/#example-604)

This markdown:

```markdown
< http://foo.bar >

```

Should give output:

```html
<p>&lt; http://foo.bar &gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
## [Example 605](https://github.github.com/gfm/#example-605)

This markdown:

```markdown
<m:abc>

```

Should give output:

```html
<p>&lt;m:abc&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```
## [Example 606](https://github.github.com/gfm/#example-606)

This markdown:

```markdown
<foo.bar.baz>

```

Should give output:

```html
<p>&lt;foo.bar.baz&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol
```

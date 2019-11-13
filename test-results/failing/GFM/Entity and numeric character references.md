# GFM - Entity and numeric character references

## [Example 311](https://github.github.com/gfm/#example-311)

This markdown:

```markdown
&nbsp; &amp; &copy; &AElig; &Dcaron;
&frac34; &HilbertSpace; &DifferentialD;
&ClockwiseContourIntegral; &ngE;

```

Should give output:

```html
<p>&amp; © Æ Ď ¾ ℋ ⅆ ∲ ≧̸</p>
```

But instead was:

```html
<p>&amp;nbsp; &amp;amp; &amp;copy; &amp;AElig; &amp;Dcaron; &amp;frac34; &amp;HilbertSpace; &amp;DifferentialD; &amp;ClockwiseContourIntegral; &amp;ngE;</p>
```
## [Example 312](https://github.github.com/gfm/#example-312)

This markdown:

```markdown
&#35; &#1234; &#992; &#0;

```

Should give output:

```html
<p># Ӓ Ϡ �</p>
```

But instead was:

```html
<p>&amp;#35; &amp;#1234; &amp;#992; &amp;#0;</p>
```
## [Example 313](https://github.github.com/gfm/#example-313)

This markdown:

```markdown
&#X22; &#XD06; &#xcab;

```

Should give output:

```html
<p>&quot; ആ ಫ</p>
```

But instead was:

```html
<p>&amp;#X22; &amp;#XD06; &amp;#xcab;</p>
```
## [Example 317](https://github.github.com/gfm/#example-317)

This markdown:

```markdown
<a href="&ouml;&ouml;.html">

```

Should give output:

```html
<a href="öö.html">
```

But instead was:

```html
ERROR Problem at row 1 No entity named &quot;ö&quot; found.
```
## [Example 318](https://github.github.com/gfm/#example-318)

This markdown:

```markdown
[foo](/f&ouml;&ouml; "f&ouml;&ouml;")

```

Should give output:

```html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>
```

But instead was:

```html
<p><a f&ouml;&ouml;\""="" href="/föö \">foo</a></p>
```
## [Example 319](https://github.github.com/gfm/#example-319)

This markdown:

```markdown
[foo]

[foo]: /f&ouml;&ouml; "f&ouml;&ouml;"

```

Should give output:

```html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## [Example 320](https://github.github.com/gfm/#example-320)

This markdown:

```markdown
``` f&ouml;&ouml;
foo
```

```

Should give output:

```html
<pre><code class="language-föö">foo</code></pre>
```

But instead was:

```html
<pre><code>foo</code></pre>
```
## [Example 323](https://github.github.com/gfm/#example-323)

This markdown:

```markdown
&#42;foo&#42;
*foo*

```

Should give output:

```html
<p>*foo*<em>foo</em></p>
```

But instead was:

```html
<p>&amp;#42;foo&amp;#42;<em>foo</em></p>
```
## [Example 324](https://github.github.com/gfm/#example-324)

This markdown:

```markdown
&#42; foo

* foo

```

Should give output:

```html
<p>* foo</p><ul><li>foo</li></ul>
```

But instead was:

```html
<p>&amp;#42; foo</p><ul><li><p>foo</p></li></ul>
```
## [Example 325](https://github.github.com/gfm/#example-325)

This markdown:

```markdown
foo&#10;&#10;bar

```

Should give output:

```html
<p>foo bar</p>
```

But instead was:

```html
<p>foo&amp;#10;&amp;#10;bar</p>
```
## [Example 326](https://github.github.com/gfm/#example-326)

This markdown:

```markdown
&#9;foo

```

Should give output:

```html
<p>foo</p>
```

But instead was:

```html
<p>&amp;#9;foo</p>
```
## [Example 327](https://github.github.com/gfm/#example-327)

This markdown:

```markdown
[a](url &quot;tit&quot;)

```

Should give output:

```html
<p>[a](url &quot;tit&quot;)</p>
```

But instead was:

```html
<p><a href="url &quot;tit&quot;">a</a></p>
```

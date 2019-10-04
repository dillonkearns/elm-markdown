# New

## links_paren

### Example undefined

This markdown:

```markdown
([one](http://example.com/1)) ([two](http://example.com/2))

([one](http://example.com/1))  ([two](http://example.com/2))

([one](http://example.com/1 "a")) ([two](http://example.com/2 "b"))

```

Should give output:

```html
<p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p><p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p><p>(<a href="http://example.com/1" title="a">one</a>) (<a href="http://example.com/2" title="b">two</a>)</p>
```

But instead was:

```html
<p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p><p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p><p>(<a a\""="" href="http://example.com/1 \">one</a>) (<a b\""="" href="http://example.com/2 \">two</a>)</p>
```
## ref_paren

### Example undefined

This markdown:

```markdown
[hi]

[hi]: /url (there)

```

Should give output:

```html
<p><a href="/url" title="there">hi</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## list_item_text

### Example undefined

This markdown:

```markdown
  * item1

    * item2

  text

```

Should give output:

```html
<ul><li><p>item1</p><ul><li>item2</li></ul><p>text</p></li></ul>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## relative_base_urls

### Example undefined

This markdown:

```markdown
# Absolutization of RFC 3986 URIs

## Absolute URI
[![section 4.3](http://example.com/logo)](http://example.com/)

## Network-path reference
[![section 4.2](//example.com/logo)](//example.com/)

## Absolute path
[![section 4.2](/path/to/img)](/path/to/content)

## Relative path
[![section 4.2](img)](content)

## Dot-relative path
[![section 3.3](./img)](./content)

[![section 3.3](../img)](../content)

## Same-document query
[![section 4.4](?type=image)](?)

## Same-document fragment
[![section 4.4](#img)](#)

## Empty
[section 4.2]()

```

Should give output:

```html
<h1 id="absolutization-of-rfc-3986-uris">Absolutization of RFC 3986 URIs</h1><h2 id="absolute-uri">Absolute URI</h2><p><a href="http://example.com/"><img alt="section 4.3" src="http://example.com/logo"></a></p><h2 id="network-path-reference">Network-path reference</h2><p><a href="//example.com/"><img alt="section 4.2" src="//example.com/logo"></a></p><h2 id="absolute-path">Absolute path</h2><p><a href="/path/to/content"><img alt="section 4.2" src="/path/to/img"></a></p><h2 id="relative-path">Relative path</h2><p><a href="/base/content"><img alt="section 4.2" src="/base/img"></a></p><h2 id="dot-relative-path">Dot-relative path</h2><p><a href="/base/./content"><img alt="section 3.3" src="/base/./img"></a></p><p><a href="/base/../content"><img alt="section 3.3" src="/base/../img"></a></p><h2 id="same-document-query">Same-document query</h2><p><a href="?"><img alt="section 4.4" src="?type=image"></a></p><h2 id="same-document-fragment">Same-document fragment</h2><p><a href="#"><img alt="section 4.4" src="#img"></a></p><h2 id="empty">Empty</h2><p><a href="">section 4.2</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
## def_blocks

### Example undefined

This markdown:

```markdown
> hello
> [1]: hello

* * *

> hello
[2]: hello


* hello
* [3]: hello


* hello
[4]: hello


> foo
> bar
[5]: foo
> bar

```

Should give output:

```html
<blockquote><p>hello [1]: hello</p></blockquote><hr><blockquote><p>hello [2]: hello</p></blockquote><ul><li>hello</li><li>[3]: hello</li></ul><ul><li>hello</li></ul><blockquote><p>foo bar [5]: foo bar</p></blockquote>
```

But instead was:

```html
ERROR Problem at row 35 Expecting Problem at row 1 Expecting symbol (
```
## html_no_new_line

### Example undefined

This markdown:

```markdown
<img src='sdfg'>
```

Should give output:

```html
<img src="sdfg">
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol
```
## code_spans

### Example undefined

This markdown:

```markdown
`someone@example.com`

``*test`*
```

Should give output:

```html
<p><code>someone@example.com</code></p><p>``<em>test`</em></p>
```

But instead was:

```html
<p><code>someone@example.com</code></p><p><em>test</em></p>
```
## tricky_list

### Example undefined

This markdown:

```markdown
**hello** _world_

* hello world

**hello** _world_

* hello world

**hello** _world_

* Hello world

**hello** _world_

* hello world

```

Should give output:

```html
<p><strong>hello</strong><em>world</em></p><ul><li>hello world</li></ul><p><strong>hello</strong><em>world</em></p><ul><li>hello world</li></ul><p><strong>hello</strong><em>world</em></p><ul><li>Hello world</li></ul><p><strong>hello</strong><em>world</em></p><ul><li>hello world</li></ul>
```

But instead was:

```html
<p><strong>hello</strong>_world_</p><p><em>hello world</em></p><p><strong>hello</strong>_world_</p><p><em>hello world</em></p><p><strong>hello</strong>_world_</p><p><em>Hello world</em></p><p><strong>hello</strong>_world_</p><p><em>hello world</em></p>
```
## autolinks

### Example undefined

This markdown:

```markdown
(See https://www.example.com/fhqwhgads.)

((http://foo.com))

((http://foo.com.))

HTTP://FOO.COM

hTtP://fOo.CoM

~~hello@email.com~~

**me@example.com**

__test@test.com__
```

Should give output:

```html
<p>(See<a href="https://www.example.com/fhqwhgads">https://www.example.com/fhqwhgads</a>.)</p><p>((<a href="http://foo.com">http://foo.com</a>))</p><p>((<a href="http://foo.com">http://foo.com</a>.))</p><p><a href="HTTP://FOO.COM">HTTP://FOO.COM</a></p><p><a href="hTtP://fOo.CoM">hTtP://fOo.CoM</a></p><p><del><a href="mailto:hello@email.com">hello@email.com</a></del></p><p><strong><a href="mailto:me@example.com">me@example.com</a></strong></p><p><strong><a href="mailto:test@test.com">test@test.com</a></strong></p>
```

But instead was:

```html
<p>(See https://www.example.com/fhqwhgads.)</p><p>((http://foo.com))</p><p>((http://foo.com.))</p><p>HTTP://FOO.COM</p><p>hTtP://fOo.CoM</p><p>~~hello@email.com~~</p><p><strong>me@example.com</strong></p><p>__test@test.com__</p>
```
## images

### Example undefined

This markdown:

```markdown
![Image](javascript:alert)

![Image](vbscript:alert)

![Image](javascript&colon;alert&#40;1&#41;)

![Image](javascript&#58document;alert&#40;1&#41;)

![Image](data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K)

```

Should give output:

```html
<p>Image</p><p>Image</p><p>Image</p><p>Image</p><p>Image</p>
```

But instead was:

```html
<p><img src="javascript:alert"></p><p><img src="vbscript:alert"></p><p><img src="javascript:alert(1)"></p><p><img src="javascript:document;alert(1)"></p><p><img src="data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K"></p>
```
## pedantic_heading_interrupts_paragraph

### Example undefined

This markdown:

```markdown
paragraph before head with hash
#how are you

paragraph before head with equals
how are you again
===========

```

Should give output:

```html
<p>paragraph before head with hash</p><h1 id="how-are-you">how are you</h1><p>paragraph before head with equals</p><h1 id="how-are-you-again">how are you again</h1>
```

But instead was:

```html
<p>paragraph before head with hash</p><h1>how are you</h1><p>paragraph before head with equals how are you again ===========</p>
```
## autolink_lines

### Example undefined

This markdown:

```markdown
hello world
<http://example.com>

```

Should give output:

```html
<p>hello world<a href="http://example.com">http://example.com</a></p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol /&gt; Problem at row 3 Expecting symbol &gt;
```
## not_a_link

### Example undefined

This markdown:

```markdown
\[test](not a link)

```

Should give output:

```html
<p>[test](not a link)</p>
```

But instead was:

```html
<p>\<a href="not a link">test</a></p>
```
## link_tick_redos

### Example undefined

This markdown:

```markdown
  dash_capstyle: ['butt' | 'round' | 'projecting']
  dash_joinstyle: ['miter' | 'round' | 'bevel']
  dashes: sequence of on/off ink in points
  drawstyle: ['default' | 'steps' | 'steps-pre' | 'steps-mid' | 'steps-post']
  figure: a `~.Figure` instance
  fillstyle: ['full' | 'left' | 'right' | 'bottom' | 'top' | 'none']
  gid: an id string
  label: object
  linestyle or ls: ['solid' | 'dashed', 'dashdot', 'dotted' | (offset, on-off-dash-seq) | ``'-'`` | ``'--'`` | ``'-.'`` | ``':'`` | ``'None'`` | ``' '`` | ``''``]
  linewidth or lw: float value in points
  marker: :mod:`A valid marker style <matplotlib.markers>`
  markeredgecolor or mec: any matplotlib color
  markeredgewidth or mew: float value in points
  markerfacecolor or mfc: any matplotlib color
  markerfacecoloralt or mfcalt: any matplotlib color
  markersize or ms: float
  markevery: [None | int | length-2 tuple of int | slice | list/array of int | float | length-2 tuple of float]
  path_effects: `~.AbstractPathEffect`
  picker: float distance in points or callable pick function ``fn(artist, event)``
  pickradius: float distance in points
  rasterized: bool or None
  sketch_params: (scale: float, length: float, randomness: float)
  snap: bool or None
  solid_capstyle: ['butt' | 'round' |  'projecting']
  solid_joinstyle: ['miter' | 'round' | 'bevel']
  transform: a :class:`matplotlib.transforms.Transform` instance
  url: a url string
  visible: bool
  xdata: 1D array
  ydata: 1D array
  zorder: float

```

Should give output:

```html
<p>dash_capstyle: [&#39;butt&#39; | &#39;round&#39; | &#39;projecting&#39;] dash_joinstyle: [&#39;miter&#39; | &#39;round&#39; | &#39;bevel&#39;] dashes: sequence of on/off ink in points drawstyle: [&#39;default&#39; | &#39;steps&#39; | &#39;steps-pre&#39; | &#39;steps-mid&#39; | &#39;steps-post&#39;] figure: a<code>~.Figure</code>instance fillstyle: [&#39;full&#39; | &#39;left&#39; | &#39;right&#39; | &#39;bottom&#39; | &#39;top&#39; | &#39;none&#39;] gid: an id string label: object linestyle or ls: [&#39;solid&#39; | &#39;dashed&#39;, &#39;dashdot&#39;, &#39;dotted&#39; | (offset, on-off-dash-seq) |<code>&#39;-&#39;</code>|<code>&#39;--&#39;</code>|<code>&#39;-.&#39;</code>|<code>&#39;:&#39;</code>|<code>&#39;None&#39;</code>|<code>&#39; &#39;</code>|<code>&#39;&#39;</code>] linewidth or lw: float value in points marker: :mod:<code>A valid marker style &lt;matplotlib.markers&gt;</code>markeredgecolor or mec: any matplotlib color markeredgewidth or mew: float value in points markerfacecolor or mfc: any matplotlib color markerfacecoloralt or mfcalt: any matplotlib color markersize or ms: float markevery: [None | int | length-2 tuple of int | slice | list/array of int | float | length-2 tuple of float] path_effects:<code>~.AbstractPathEffect</code>picker: float distance in points or callable pick function<code>fn(artist, event)</code>pickradius: float distance in points rasterized: bool or None sketch_params: (scale: float, length: float, randomness: float) snap: bool or None solid_capstyle: [&#39;butt&#39; | &#39;round&#39; | &#39;projecting&#39;] solid_joinstyle: [&#39;miter&#39; | &#39;round&#39; | &#39;bevel&#39;] transform: a :class:<code>matplotlib.transforms.Transform</code>instance url: a url string visible: bool xdata: 1D array ydata: 1D array zorder: float</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## links

### Example undefined

This markdown:

```markdown
Link: [constructor][].

[One](https://example.com/1) ([Two](https://example.com/2)) [Three](https://example.com/3)

[constructor]: https://example.org/

```

Should give output:

```html
<p>Link:<a href="https://example.org/">constructor</a>.</p><p><a href="https://example.com/1">One</a>(<a href="https://example.com/2">Two</a>)<a href="https://example.com/3">Three</a></p>
```

But instead was:

```html
ERROR Problem at row 9 Expecting Problem at row 1 Expecting symbol (
```
## blockquote_list_item

### Example undefined

This markdown:

```markdown
This fails in markdown.pl and upskirt:

* hello
  > world

```

Should give output:

```html
<p>This fails in markdown.pl and upskirt:</p><ul><li>hello<blockquote><p>world</p></blockquote></li></ul>
```

But instead was:

```html
ERROR Problem at row 6 Expecting --- Problem at row 6 Expecting *** Problem at row 6 Expecting ___
```
## table_cells

### Example undefined

This markdown:

```markdown
|1|
|-|
|1|

|1|
|-|
|\||

|1|
|-|
|1\\1|

|1|
|-|
|\\\\||

|1|
|-|
|\\\\\||

|1|2|
|-|-|
||2|

|1|2|
|-|-|
|1\|\\|2\|\\|

|1|2|
|-|-|
| |2|

1|2
-|-
1\|\\|2\|\\

1|2
-|-
 |2

1|2
-|-
1|2\|

1|2
-|-
1|2\|

|1|2|
|-|-|
|1|2\||

|1|2|
|-|-|
|1|2\||

```

Should give output:

```html
<table><thead><tr><th>1</th></tr></thead><tbody><tr><td>1</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>|</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>1\1</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>\\</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>\\|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1|\</td><td>2|\</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1|\</td><td>2|\</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table>
```

But instead was:

```html
ERROR Problem at row 68 Expecting --- Problem at row 68 Expecting *** Problem at row 68 Expecting ___
```
## adjacent_lists

### Example undefined

This markdown:

```markdown
* This should be
* An unordered list

1. This should be
2. An unordered list

```

Should give output:

```html
<ul><li>This should be</li><li>An unordered list</li></ul><ol><li>This should be</li><li>An unordered list</li></ol>
```

But instead was:

```html
<p><em>This should be</em>An unordered list</p><p>1. This should be 2. An unordered list</p>
```
## case_insensitive_refs

### Example undefined

This markdown:

```markdown
[hi]

[HI]: /url

```

Should give output:

```html
<p><a href="/url">hi</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## list_loose_tasks

### Example undefined

This markdown:

```markdown
- Tasks
- [x] Task1

- [ ] <pre>Task2</pre>

```

Should give output:

```html
<ul><li><p>Tasks</p></li><li><p><input checked="" disabled="" type="checkbox">Task1</p></li><li><p><input disabled="" type="checkbox"></p><pre>Task2</pre></li></ul>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```
## html_comments

### Example undefined

This markdown:

```markdown
### Example 1

<!-- comment -->

### Example 2

<!---->

### Example 3

<!-- -->

### Example 4

<!-- - -->

### Example 5

<!-- -- -->

### Example 6

<!-- --->

### Example 7

<!----->

### Example 8

<!------>

### Example 9

<!-- My favorite operators are > and <!-->

### Example 10

<!-- multi
line	
comment
-->

### Example 11

   <!-- indented comment -->

    <!-- too much indentation -->

### Example 12

<!--> not a comment -->

<!---> not a comment -->

<!-- <!-- not a comment? --> -->
```

Should give output:

```html
<h3 id="example-1">Example 1</h3><h3 id="example-2">Example 2</h3><h3 id="example-3">Example 3</h3><h3 id="example-4">Example 4</h3><h3 id="example-5">Example 5</h3><h3 id="example-6">Example 6</h3><h3 id="example-7">Example 7</h3><h3 id="example-8">Example 8</h3><h3 id="example-9">Example 9</h3><h3 id="example-10">Example 10</h3><h3 id="example-11">Example 11</h3><pre><code>&lt;!-- too much indentation --&gt;</code></pre><h3 id="example-12">Example 12</h3><p>&lt;!--&gt; not a comment --&gt;</p><p>&lt;!---&gt; not a comment --&gt;</p>--&gt;
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol =
```
## substitutions

### Example undefined

This markdown:

```markdown
foo␤␤bar

* a *

```

Should give output:

```html
<p>foo␤␤bar</p><p>* a *</p>
```

But instead was:

```html
<p>foo␤␤bar</p><p><em>a</em></p>
```
## smartypants

### Example undefined

This markdown:

```markdown
Hello world 'how' "are" you -- today...

"It's a more 'challenging' smartypants test..."

'And,' as a bonus --- "one
multiline" test!

```

Should give output:

```html
<p>Hello world ‘how’ “are” you – today…</p><p>“It’s a more ‘challenging’ smartypants test…”</p><p>‘And,’ as a bonus — “one multiline” test!</p>
```

But instead was:

```html
<p>Hello world &#39;how&#39; &quot;are&quot; you -- today...</p><p>&quot;It&#39;s a more &#39;challenging&#39; smartypants test...&quot;</p><p>&#39;And,&#39; as a bonus --- &quot;one multiline&quot; test!</p>
```
## mangle_xss

### Example undefined

This markdown:

```markdown
<<svg/onload="alert(1)"//@x>

<bar"onclick="alert('XSS')"@foo>

```

Should give output:

```html
<p>&lt;&lt;svg/onload=&quot;alert(1)&quot;//@x&gt;</p><p>&lt;bar&quot;onclick=&quot;alert(&#39;XSS&#39;)&quot;@foo&gt;</p>
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
## em_2char

### Example undefined

This markdown:

```markdown
_123_

*123*

_12_

*12*

_1_

*1*

__

**

_123 _

*123 *

_ 123_

_1__

*1**

```

Should give output:

```html
<p><em>123</em></p><p><em>123</em></p><p><em>12</em></p><p><em>12</em></p><p><em>1</em></p><p><em>1</em></p><p>__</p><p>**</p><p>_123 _</p><p>*123 *</p><p>_ 123_</p><p><em>1_</em></p><p><em>1*</em></p>
```

But instead was:

```html
<p>_123_</p><p><em>123</em></p><p>_12_</p><p><em>12</em></p><p>_1_</p><p><em>1</em></p><p>__</p><p>_123 _</p><p><em>123</em></p><p>_ 123_</p><p>_1__</p><p><em>1</em></p>
```
## toplevel_paragraphs

### Example undefined

This markdown:

```markdown
hello world
    text after spaces
    text after spaces

paragraph before code
```
text inside block code
```

paragraph before hr
* * *

paragraph before blockquote
> text for blockquote

paragraph before list
* text inside list

paragraph before div
<div>text inside div</div>

paragraph with span
<span>text inside span</span>

hello [world][how]

[how]: /are/you

<div>hello</div>

<span>hello</span>

```

Should give output:

```html
<p>hello world text after spaces text after spaces</p><p>paragraph before code</p><pre><code>text inside block code</code></pre><p>paragraph before hr</p><hr><p>paragraph before blockquote</p><blockquote><p>text for blockquote</p></blockquote><p>paragraph before list</p><ul><li>text inside list</li></ul><p>paragraph before div</p><div>text inside div</div><p>paragraph with span<span>text inside span</span></p><p>hello<a href="/are/you">world</a></p><div>hello</div><p><span>hello</span></p>
```

But instead was:

```html
ERROR Problem at row 48 Expecting Problem at row 1 Expecting symbol (
```
## headings_id

### Example undefined

This markdown:

```markdown
### Heading with a [link](http://github.com/)

### Heading with some _italic text_

### Or some **strong**
(which doesn't really make any difference, here)

### Or even `code`

### What about ~~strikethrough~~

## And a ref [link][destination]

[destination]: /some/url "link to nowhere"
```

Should give output:

```html
<h3 id="heading-with-a-link">Heading with a<a href="http://github.com/">link</a></h3><h3 id="heading-with-some-italic-text">Heading with some<em>italic text</em></h3><h3 id="or-some-strong">Or some<strong>strong</strong></h3><p>(which doesn&#39;t really make any difference, here)</p><h3 id="or-even-code">Or even<code>code</code></h3><h3 id="what-about-strikethrough">What about<del>strikethrough</del></h3><h2 id="and-a-ref-link">And a ref<a href="/some/url" title="link to nowhere">link</a></h2>
```

But instead was:

```html
ERROR Problem at row 21 Expecting Problem at row 1 Expecting symbol (
```
## list_table

### Example undefined

This markdown:

```markdown
* Table in list:

  | column1 | column2 |
  |---------|---------|
  | value1  | value2  |
  | value3  | value4  |

* No leading pipe table in list:

  column1 | column2
  --------|--------
  value1  | value2
  value3  | value4

```

Should give output:

```html
<ul><li><p>Table in list:</p><table><thead><tr><th>column1</th><th>column2</th></tr></thead><tbody><tr><td>value1</td><td>value2</td></tr><tr><td>value3</td><td>value4</td></tr></tbody></table></li><li><p>No leading pipe table in list:</p><table><thead><tr><th>column1</th><th>column2</th></tr></thead><tbody><tr><td>value1</td><td>value2</td></tr><tr><td>value3</td><td>value4</td></tr></tbody></table></li></ul>
```

But instead was:

```html
ERROR Problem at row 4 Expecting --- Problem at row 4 Expecting *** Problem at row 4 Expecting ___
```
## breaks

### Example undefined

This markdown:

```markdown
A
B

```

Should give output:

```html
<p>A<br>B</p>
```

But instead was:

```html
<p>A B</p>
```
## escaped_angles

### Example undefined

This markdown:

```markdown
\>

```

Should give output:

```html
<p>&gt;</p>
```

But instead was:

```html
<p>\&gt;</p>
```
## relative_urls

### Example undefined

This markdown:

```markdown
# Absolutization of RFC 3986 URIs

## Absolute URI
[![section 4.3](http://example.com/logo)](http://example.com/)

## Network-path reference
[![section 4.2](//example.com/logo)](//example.com/)

## Absolute path
[![section 4.2](/path/to/img)](/path/to/content)

## Relative path
[![section 4.2](img)](content)

## Dot-relative path
[![section 3.3](./img)](./content)

[![section 3.3](../img)](../content)

## Same-document query
[![section 4.4](?type=image)](?)

## Same-document fragment
[![section 4.4](#img)](#)

## Empty
[section 4.2]()

```

Should give output:

```html
<h1 id="absolutization-of-rfc-3986-uris">Absolutization of RFC 3986 URIs</h1><h2 id="absolute-uri">Absolute URI</h2><p><a href="http://example.com/"><img alt="section 4.3" src="http://example.com/logo"></a></p><h2 id="network-path-reference">Network-path reference</h2><p><a href="http://example.com/"><img alt="section 4.2" src="http://example.com/logo"></a></p><h2 id="absolute-path">Absolute path</h2><p><a href="http://example.com/path/to/content"><img alt="section 4.2" src="http://example.com/path/to/img"></a></p><h2 id="relative-path">Relative path</h2><p><a href="http://example.com/base/content"><img alt="section 4.2" src="http://example.com/base/img"></a></p><h2 id="dot-relative-path">Dot-relative path</h2><p><a href="http://example.com/base/./content"><img alt="section 3.3" src="http://example.com/base/./img"></a></p><p><a href="http://example.com/base/../content"><img alt="section 3.3" src="http://example.com/base/../img"></a></p><h2 id="same-document-query">Same-document query</h2><p><a href="?"><img alt="section 4.4" src="?type=image"></a></p><h2 id="same-document-fragment">Same-document fragment</h2><p><a href="#"><img alt="section 4.4" src="#img"></a></p><h2 id="empty">Empty</h2><p><a href="">section 4.2</a></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol ]
```
## nested_square_link

### Example undefined

This markdown:

```markdown
[the `]` character](/url)

[the \` character](/url)

```

Should give output:

```html
<p><a href="/url">the<code>]</code>character</a></p><p><a href="/url">the ` character</a></p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting Problem at row 1 Expecting symbol (
```
## emphasis_extra tests

### Example undefined

This markdown:

```markdown
_test_. _test_: _test_! _test_? _test_-
```

Should give output:

```html
<p><em>test</em>.<em>test</em>:<em>test</em>!<em>test</em>?<em>test</em>-</p>
```

But instead was:

```html
<p>_test_. _test_: _test_! _test_? _test_-</p>
```
## main

### Example undefined

This markdown:

```markdown
[test]: http://google.com/ "Google"

# A heading

Just a note, I've found that I can't test my markdown parser vs others.
For example, both markdown.js and showdown code blocks in lists wrong. They're
also completely [inconsistent][test] with regards to paragraphs in list items.

A link. Not anymore.

<aside>This will make me fail the test because
markdown.js doesnt acknowledge arbitrary html blocks =/</aside>

* List Item 1

* List Item 2
  * New List Item 1
    Hi, this is a list item.
  * New List Item 2
    Another item
        Code goes here.
        Lots of it...
  * New List Item 3
    The last item

* List Item 3
The final item.

* List Item 4
The real final item.

Paragraph.

> * bq Item 1
> * bq Item 2
>   * New bq Item 1
>   * New bq Item 2
>   Text here

* * *

> Another blockquote!
> I really need to get
> more creative with
> mockup text..
> markdown.js breaks here again

Another Heading
-------------

Hello *world*. Here is a [link](//hello).
And an image ![alt](src).

    Code goes here.
    Lots of it...

```

Should give output:

```html
<h1 id="a-heading">A heading</h1><p>Just a note, I&#39;ve found that I can&#39;t test my markdown parser vs others. For example, both markdown.js and showdown code blocks in lists wrong. They&#39;re also completely<a href="http://google.com/" title="Google">inconsistent</a>with regards to paragraphs in list items.</p><p>A link. Not anymore.</p><aside>This will make me fail the test because markdown.js doesnt acknowledge arbitrary html blocks =/</aside><ul><li><p>List Item 1</p></li><li><p>List Item 2</p><ul><li>New List Item 1 Hi, this is a list item.</li><li>New List Item 2 Another item<pre><code>Code goes here. Lots of it...</code></pre></li><li>New List Item 3 The last item</li></ul></li><li><p>List Item 3 The final item.</p></li><li><p>List Item 4 The real final item.</p></li></ul><p>Paragraph.</p><blockquote><ul><li>bq Item 1</li><li>bq Item 2<ul><li>New bq Item 1</li><li>New bq Item 2 Text here</li></ul></li></ul></blockquote><hr><blockquote><p>Another blockquote! I really need to get more creative with mockup text.. markdown.js breaks here again</p></blockquote><h2 id="another-heading">Another Heading</h2><p>Hello<em>world</em>. Here is a<a href="//hello">link</a>. And an image<img alt="alt" src="src">.</p><pre><code>Code goes here. Lots of it...</code></pre>
```

But instead was:

```html
ERROR Problem at row 25 Expecting --- Problem at row 25 Expecting *** Problem at row 25 Expecting ___
```
## nogfm_hashtag

### Example undefined

This markdown:

```markdown
#header

# header1

#  header2

```

Should give output:

```html
<h1 id="header">header</h1><h1 id="header1">header1</h1><h1 id="header2">header2</h1>
```

But instead was:

```html
<h1>header</h1><h1>header1</h1><h1>header2</h1>
```
## lazy_blockquotes

### Example undefined

This markdown:

```markdown
> hi there
bud

```

Should give output:

```html
<blockquote><p>hi there bud</p></blockquote>
```

But instead was:

```html
<p>&gt; hi there bud</p>
```
## em_list_links

### Example undefined

This markdown:

```markdown
- italic
  - [*named link*][some-url]
  - *[named link][some-url]*
  - [_named link_][some-url]
  - _[named link][some-url]_
- bold
  - [**named link**][some-url]
  - **[named link][some-url]**
  - [__named link__][some-url]
  - __[named link][some-url]__
- bold italic
  - [***named link***][some-url]
  - ***[named link][some-url]***
  - [___named link___][some-url]
  - ___[named link][some-url]___
  - [*__named link__*][some-url]
  - [__*named link*__][some-url]
  - __*[named link][some-url]*__
- code
  - [`named link`][some-url]
- code italic
  - *[`named link`][some-url]*
  - [*`named link`*][some-url]
  - _[`named link`][some-url]_
  - [_`named link`_][some-url]
- code bold
  - **[`named link`][some-url]**
  - [**`named link`**][some-url]
  - __[`named link`][some-url]__
  - [__`named link`__][some-url]
- code bold italic
  - [***`named link`***][some-url]
  - ***[`named link`][some-url]***
  - [___`named link`___][some-url]
  - ___[`named link`][some-url]___
  - [*__`named link`__*][some-url]
  - [__*`named link`*__][some-url]
  - __*[`named link`][some-url]*__

[some-url]: https://www.google.com
```

Should give output:

```html
<ul><li>italic<ul><li><a href="https://www.google.com"><em>named link</em></a></li><li><em><a href="https://www.google.com">named link</a></em></li><li><a href="https://www.google.com"><em>named link</em></a></li><li><em><a href="https://www.google.com">named link</a></em></li></ul></li><li>bold<ul><li><a href="https://www.google.com"><strong>named link</strong></a></li><li><strong><a href="https://www.google.com">named link</a></strong></li><li><a href="https://www.google.com"><strong>named link</strong></a></li><li><strong><a href="https://www.google.com">named link</a></strong></li></ul></li><li>bold italic<ul><li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li><li><strong><em><a href="https://www.google.com">named link</a></em></strong></li><li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li><li><strong><em><a href="https://www.google.com">named link</a></em></strong></li><li><a href="https://www.google.com"><em><strong>named link</strong></em></a></li><li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li><li><strong><em><a href="https://www.google.com">named link</a></em></strong></li></ul></li><li>code<ul><li><a href="https://www.google.com"><code>named link</code></a></li></ul></li><li>code italic<ul><li><em><a href="https://www.google.com"><code>named link</code></a></em></li><li><a href="https://www.google.com"><em><code>named link</code></em></a></li><li><em><a href="https://www.google.com"><code>named link</code></a></em></li><li><a href="https://www.google.com"><em><code>named link</code></em></a></li></ul></li><li>code bold<ul><li><strong><a href="https://www.google.com"><code>named link</code></a></strong></li><li><a href="https://www.google.com"><strong><code>named link</code></strong></a></li><li><strong><a href="https://www.google.com"><code>named link</code></a></strong></li><li><a href="https://www.google.com"><strong><code>named link</code></strong></a></li></ul></li><li>code bold italic<ul><li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li><li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li><li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li><li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li><li><a href="https://www.google.com"><em><strong><code>named link</code></strong></em></a></li><li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li><li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li></ul></li></ul>
```

But instead was:

```html
ERROR Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```
## uppercase_hex

### Example undefined

This markdown:

```markdown
lower[click me](javascript&#x3a;...)lower
upper[click me](javascript&#X3a;...)upper

```

Should give output:

```html
<p>lowerclick melower upperclick meupper</p>
```

But instead was:

```html
<p>lower<a href="javascript:...">click me</a>lower upper<a href="javascript:...">click me</a>upper</p>
```
## nested_code

### Example undefined

This markdown:

```markdown
````` hi ther `` ok ``` `````

`` ` ``

``There is a literal backtick (`) here.``

A backtick-delimited string in a code span: `` `foo` ``

Please don't use any `<blink>` tags.
```

Should give output:

```html
<p><code>hi ther `` ok ```</code></p><p><code>`</code></p><p><code>There is a literal backtick (`) here.</code></p><p>A backtick-delimited string in a code span:<code>`foo`</code></p><p>Please don&#39;t use any<code>&lt;blink&gt;</code>tags.</p>
```

But instead was:

```html
ERROR Problem at row 10 Expecting symbol ```
```
## smartypants_code

### Example undefined

This markdown:

```markdown
<pre>&amp;</pre>
<code>--foo</code>
<kbd>---foo</kbd>
<script>--foo</script>

Ensure that text such as custom tags that happen to
begin with the same letters as the above tags don't
match and thus benefit from Smartypants-ing.
<script-custom>--foo</script-custom>
`--foo` <codebar --foo codebar>

```

Should give output:

```html
<pre>&amp;</pre><p><code>--foo</code><kbd>---foo</kbd></p><script>--foo</script><p>Ensure that text such as custom tags that happen to begin with the same letters as the above tags don’t match and thus benefit from Smartypants-ing.</p><p><script-custom>–foo</script-custom><code>--foo</code>&lt;codebar –foo codebar&gt;</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting Problem at row 1 Expecting symbol
```
## sanitize_links

### Example undefined

This markdown:

```markdown
[URL](javascript:alert)

[URL](vbscript:alert)

[URL](javascript&colon;alert&#40;1&#41;)

[URL](javascript&#58document;alert&#40;1&#41;)

[URL](data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K)

```

Should give output:

```html
<p>URL</p><p>URL</p><p>URL</p><p>URL</p><p>URL</p>
```

But instead was:

```html
<p><a href="javascript:alert">URL</a></p><p><a href="vbscript:alert">URL</a></p><p><a href="javascript:alert(1)">URL</a></p><p><a href="javascript:document;alert(1)">URL</a></p><p><a href="data:text/html;base64,PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4K">URL</a></p>
```
## link_lt

### Example undefined

This markdown:

```markdown
[URL](<test)

```

Should give output:

```html
<p><a href="%3Ctest">URL</a></p>
```

But instead was:

```html
<p><a href="&lt;test">URL</a></p>
```
## hr_list_break

### Example undefined

This markdown:

```markdown
* hello
world
* how
are
* * *
you today?

```

Should give output:

```html
<ul><li>hello world</li><li>how are</li></ul><hr><p>you today?</p>
```

But instead was:

```html
<p><em>hello world</em>how are<em></em><em>you today?</em></p>
```
## same_bullet

### Example undefined

This markdown:

```markdown
* test
+ test
- test

```

Should give output:

```html
<ul><li>test</li><li>test</li><li>test</li></ul>
```

But instead was:

```html
<p><em>test + test</em></p><ul><li><p>test</p></li></ul>
```
## nested_em

### Example undefined

This markdown:

```markdown
*test **test** test*

_test __test__ test_

```

Should give output:

```html
<p><em>test<strong>test</strong>test</em></p><p><em>test<strong>test</strong>test</em></p>
```

But instead was:

```html
<p><em>test</em><strong>test</strong><em>test</em></p><p>_test __test__ test_</p>
```
## double_link

### Example undefined

This markdown:

```markdown
<p>Already linked: <a href="http://example.com/">http://example.com/</a>.</p>

Already linked: [http://example.com/](http://example.com/).

Already linked: <a href="http://example.com/">**http://example.com/**</a>.

```

Should give output:

```html
<p>Already linked:<a href="http://example.com/">http://example.com/</a>.</p><p>Already linked:<a href="http://example.com/">http://example.com/</a>.</p><p>Already linked:<a href="http://example.com/"><strong>http://example.com/</strong></a>.</p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```

## New

### uppercase_hex

Example undefined

```html
<p>lowerclick melower
upperclick meupper</p>

```

### smartypants_code

Example undefined

```html
<pre>&amp;</pre>
<p><code>--foo</code>
<kbd>---foo</kbd></p>
<script>--foo</script>

<p>Ensure that text such as custom tags that happen to
begin with the same letters as the above tags don’t
match and thus benefit from Smartypants-ing.</p>

<p><script-custom>–foo</script-custom>
<code>--foo</code> &lt;codebar –foo codebar&gt;</p>

```

### em_2char

Example undefined

```html
<p><em>123</em></p>

<p><em>123</em></p>

<p><em>12</em></p>

<p><em>12</em></p>

<p><em>1</em></p>

<p><em>1</em></p>

<p>__</p>

<p>**</p>

<p>_123 _</p>

<p>*123 *</p>

<p>_ 123_</p>

<p><em>1_</em></p>

<p><em>1*</em></p>

```

### html_no_new_line

Example undefined

```html
<img src='sdfg'>
```

### relative_base_urls

Example undefined

```html
<h1 id="absolutization-of-rfc-3986-uris">Absolutization of RFC 3986 URIs</h1>

<h2 id="absolute-uri">Absolute URI</h2>

<p><a href="http://example.com/"><img src="http://example.com/logo" alt="section 4.3"></a></p>

<h2 id="network-path-reference">Network-path reference</h2>

<p><a href="//example.com/"><img src="//example.com/logo" alt="section 4.2"></a></p>

<h2 id="absolute-path">Absolute path</h2>

<p><a href="/path/to/content"><img src="/path/to/img" alt="section 4.2"></a></p>

<h2 id="relative-path">Relative path</h2>

<p><a href="/base/content"><img src="/base/img" alt="section 4.2"></a></p>

<h2 id="dot-relative-path">Dot-relative path</h2>

<p><a href="/base/./content"><img src="/base/./img" alt="section 3.3"></a></p>

<p><a href="/base/../content"><img src="/base/../img" alt="section 3.3"></a></p>

<h2 id="same-document-query">Same-document query</h2>

<p><a href="?"><img src="?type=image" alt="section 4.4"></a></p>

<h2 id="same-document-fragment">Same-document fragment</h2>

<p><a href="#"><img src="#img" alt="section 4.4"></a></p>

<h2 id="empty">Empty</h2>

<p><a href="">section 4.2</a></p>

```

### headings_id

Example undefined

```html
<h3 id="heading-with-a-link">Heading with a <a href="http://github.com/">link</a></h3>

<h3 id="heading-with-some-italic-text">Heading with some <em>italic text</em></h3>

<h3 id="or-some-strong">Or some <strong>strong</strong></h3>

<p>(which doesn&#39;t really make any difference, here)</p>

<h3 id="or-even-code">Or even <code>code</code></h3>

<h3 id="what-about-strikethrough">What about <del>strikethrough</del></h3>

<h2 id="and-a-ref-link">And a ref <a href="/some/url" title="link to nowhere">link</a></h2>
```

### smartypants

Example undefined

```html
<p>Hello world ‘how’ “are” you – today…</p>

<p>“It’s a more ‘challenging’ smartypants test…”</p>

<p>‘And,’ as a bonus — “one
multiline” test!</p>

```

### escaped_angles

Example undefined

```html
<p>&gt;</p>

```

### ref_paren

Example undefined

```html
<p><a href="/url" title="there">hi</a></p>

```

### pedantic_heading_interrupts_paragraph

Example undefined

```html
<p>paragraph before head with hash</p>
<h1 id="how-are-you">how are you</h1>

<p>paragraph before head with equals</p>
<h1 id="how-are-you-again">how are you again</h1>

```

### double_link

Example undefined

```html
<p>Already linked: <a href="http://example.com/">http://example.com/</a>.</p>

<p>Already linked: <a href="http://example.com/">http://example.com/</a>.</p>

<p>Already linked: <a href="http://example.com/"><strong>http://example.com/</strong></a>.</p>

```

### nogfm_hashtag

Example undefined

```html
<h1 id="header">header</h1>

<h1 id="header1">header1</h1>

<h1 id="header2">header2</h1>

```

### def_blocks

Example undefined

```html
<blockquote>
  <p>hello
[1]: hello</p>
</blockquote>

<hr>

<blockquote>
  <p>hello
[2]: hello</p>
</blockquote>


<ul>
<li>hello</li>
<li>[3]: hello</li>
</ul>


<ul>
<li>hello</li>
</ul>


<blockquote>
  <p>foo
bar
[5]: foo
bar</p>
</blockquote>

```

### tricky_list

Example undefined

```html
<p><strong>hello</strong> <em>world</em></p>

<ul>
<li>hello world</li>
</ul>

<p><strong>hello</strong> <em>world</em></p>

<ul>
<li>hello world</li>
</ul>

<p><strong>hello</strong> <em>world</em></p>

<ul>
<li>Hello world</li>
</ul>

<p><strong>hello</strong> <em>world</em></p>

<ul>
<li>hello world</li>
</ul>

```

### sanitize_links

Example undefined

```html
<p>URL</p>
<p>URL</p>
<p>URL</p>
<p>URL</p>
<p>URL</p>

```

### html_comments

Example undefined

```html
<h3 id="example-1">Example 1</h3>

<!-- comment -->

<h3 id="example-2">Example 2</h3>

<!---->

<h3 id="example-3">Example 3</h3>

<!-- -->

<h3 id="example-4">Example 4</h3>

<!-- - -->

<h3 id="example-5">Example 5</h3>

<!-- -- -->

<h3 id="example-6">Example 6</h3>

<!-- --->

<h3 id="example-7">Example 7</h3>

<!----->

<h3 id="example-8">Example 8</h3>

<!------>

<h3 id="example-9">Example 9</h3>

<!-- My favorite operators are > and <!-->

<h3 id="example-10">Example 10</h3>

<!-- multi
line    
comment
-->

<h3 id="example-11">Example 11</h3>

   <!-- indented comment -->

<pre><code>&lt;!-- too much indentation --&gt;
</code></pre>

<h3 id="example-12">Example 12</h3>

<p>&lt;!--&gt; not a comment --&gt;</p>

<p>&lt;!---&gt; not a comment --&gt;</p>

<!-- <!-- not a comment? --> -->

```

### case_insensitive_refs

Example undefined

```html
<p><a href="/url">hi</a></p>

```

### list_loose_tasks

Example undefined

```html
<ul>
<li>
<p>Tasks</p>
</li>
<li>
<p><input type="checkbox" checked="" disabled=""> Task1</p>
</li>
<li>
<p><input type="checkbox" disabled=""></p>
<pre>Task2</pre>
</li>
</ul>

```

### toplevel_paragraphs

Example undefined

```html
<p>hello world
    text after spaces
    text after spaces</p>

<p>paragraph before code</p>
<pre><code>text inside block code</code></pre>

<p>paragraph before hr</p>
<hr>

<p>paragraph before blockquote</p>
<blockquote><p>text for blockquote</p></blockquote>

<p>paragraph before list</p>
<ul><li>text inside list</li></ul>

<p>paragraph before div</p>
<div>text inside div</div>

<p>paragraph with span
<span>text inside span</span></p>

<p>hello <a href="/are/you">world</a>
</p>

<div>hello</div>

<p><span>hello</span></p>

```

### nested_em

Example undefined

```html
<p><em>test <strong>test</strong> test</em></p>

<p><em>test <strong>test</strong> test</em></p>

```

### same_bullet

Example undefined

```html
<ul>
<li>test</li>
<li>test</li>
<li>test</li>
</ul>

```

### breaks

Example undefined

```html
<p>A<br>B</p>

```

### emphasis_extra tests

Example undefined

```html
<p><em>test</em>. <em>test</em>: <em>test</em>! <em>test</em>? <em>test</em>-</p>
```

### not_a_link

Example undefined

```html
<p>[test](not a link)</p>

```

### blockquote_list_item

Example undefined

```html
<p>This fails in markdown.pl and upskirt:</p>

<ul><li>hello<blockquote><p>world</p></blockquote></li></ul>

```

### list_table

Example undefined

```html
<ul>
	<li>
		<p>Table in list:</p>
		<table>
			<thead>
				<tr>
					<th>column1</th>
					<th>column2</th>
				</tr>
			</thead>
			<tbody>
				<tr>
					<td>value1</td>
					<td>value2</td>
				</tr>
				<tr>
					<td>value3</td>
					<td>value4</td>
				</tr>
			</tbody>
		</table>
	</li>
	<li>
		<p>No leading pipe table in list:</p>
		<table>
			<thead>
				<tr>
					<th>column1</th>
					<th>column2</th>
				</tr>
			</thead>
			<tbody>
				<tr>
					<td>value1</td>
					<td>value2</td>
				</tr>
				<tr>
					<td>value3</td>
					<td>value4</td>
				</tr>
			</tbody>
		</table>
	</li>
</ul>

```

### em_list_links

Example undefined

```html
<ul>
<li>italic<ul>
<li><a href="https://www.google.com"><em>named link</em></a></li>
<li><em><a href="https://www.google.com">named link</a></em></li>
<li><a href="https://www.google.com"><em>named link</em></a></li>
<li><em><a href="https://www.google.com">named link</a></em></li>
</ul>
</li>
<li>bold<ul>
<li><a href="https://www.google.com"><strong>named link</strong></a></li>
<li><strong><a href="https://www.google.com">named link</a></strong></li>
<li><a href="https://www.google.com"><strong>named link</strong></a></li>
<li><strong><a href="https://www.google.com">named link</a></strong></li>
</ul>
</li>
<li>bold italic<ul>
<li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li>
<li><strong><em><a href="https://www.google.com">named link</a></em></strong></li>
<li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li>
<li><strong><em><a href="https://www.google.com">named link</a></em></strong></li>
<li><a href="https://www.google.com"><em><strong>named link</strong></em></a></li>
<li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li>
<li><strong><em><a href="https://www.google.com">named link</a></em></strong></li>
</ul>
</li>
<li>code<ul>
<li><a href="https://www.google.com"><code>named link</code></a></li>
</ul>
</li>
<li>code italic<ul>
<li><em><a href="https://www.google.com"><code>named link</code></a></em></li>
<li><a href="https://www.google.com"><em><code>named link</code></em></a></li>
<li><em><a href="https://www.google.com"><code>named link</code></a></em></li>
<li><a href="https://www.google.com"><em><code>named link</code></em></a></li>
</ul>
</li>
<li>code bold<ul>
<li><strong><a href="https://www.google.com"><code>named link</code></a></strong></li>
<li><a href="https://www.google.com"><strong><code>named link</code></strong></a></li>
<li><strong><a href="https://www.google.com"><code>named link</code></a></strong></li>
<li><a href="https://www.google.com"><strong><code>named link</code></strong></a></li>
</ul>
</li>
<li>code bold italic<ul>
<li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li>
<li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li>
<li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li>
<li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li>
<li><a href="https://www.google.com"><em><strong><code>named link</code></strong></em></a></li>
<li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li>
<li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li>
</ul>
</li>
</ul>


```

### autolinks

Example undefined

```html
<p>(See <a href="https://www.example.com/fhqwhgads">https://www.example.com/fhqwhgads</a>.)</p>

<p>((<a href="http://foo.com">http://foo.com</a>))</p>

<p>((<a href="http://foo.com">http://foo.com</a>.))</p>

<p><a href="HTTP://FOO.COM">HTTP://FOO.COM</a></p>

<p><a href="hTtP://fOo.CoM">hTtP://fOo.CoM</a></p>

<p><del><a href="mailto:hello@email.com">hello@email.com</a></del></p>

<p><strong><a href="mailto:me@example.com">me@example.com</a></strong></p>

<p><strong><a href="mailto:test@test.com">test@test.com</a></strong></p>
```

### mangle_xss

Example undefined

```html
<p>&lt;&lt;svg/onload=&quot;alert(1)&quot;//@x&gt;</p>

<p>&lt;bar&quot;onclick=&quot;alert(&#39;XSS&#39;)&quot;@foo&gt;</p>

```

### hr_list_break

Example undefined

```html
<ul>
<li>hello
world</li>
<li>how
are</li>
</ul>

<hr>

<p>you today?</p>

```

### list_item_text

Example undefined

```html
<ul><li><p>item1</p>  <ul><li>item2 </li></ul> <p>text</p> </li></ul>

```

### nested_square_link

Example undefined

```html
<p><a href="/url">the <code>]</code> character</a></p>

<p><a href="/url">the ` character</a></p>

```

### relative_urls

Example undefined

```html
<h1 id="absolutization-of-rfc-3986-uris">Absolutization of RFC 3986 URIs</h1>

<h2 id="absolute-uri">Absolute URI</h2>

<p><a href="http://example.com/"><img src="http://example.com/logo" alt="section 4.3"></a></p>

<h2 id="network-path-reference">Network-path reference</h2>

<p><a href="http://example.com/"><img src="http://example.com/logo" alt="section 4.2"></a></p>

<h2 id="absolute-path">Absolute path</h2>

<p><a href="http://example.com/path/to/content"><img src="http://example.com/path/to/img" alt="section 4.2"></a></p>

<h2 id="relative-path">Relative path</h2>

<p><a href="http://example.com/base/content"><img src="http://example.com/base/img" alt="section 4.2"></a></p>

<h2 id="dot-relative-path">Dot-relative path</h2>

<p><a href="http://example.com/base/./content"><img src="http://example.com/base/./img" alt="section 3.3"></a></p>

<p><a href="http://example.com/base/../content"><img src="http://example.com/base/../img" alt="section 3.3"></a></p>

<h2 id="same-document-query">Same-document query</h2>

<p><a href="?"><img src="?type=image" alt="section 4.4"></a></p>

<h2 id="same-document-fragment">Same-document fragment</h2>

<p><a href="#"><img src="#img" alt="section 4.4"></a></p>

<h2 id="empty">Empty</h2>

<p><a href="">section 4.2</a></p>

```

### links

Example undefined

```html
<p>Link: <a href="https://example.org/">constructor</a>.</p>

<p><a href="https://example.com/1">One</a> (<a href="https://example.com/2">Two</a>) <a href="https://example.com/3">Three</a></p>

```

### adjacent_lists

Example undefined

```html
<ul>
<li>This should be</li>
<li>An unordered list</li>
</ul>

<ol>
<li>This should be</li>
<li>An unordered list</li>
</ol>

```

### links_paren

Example undefined

```html
<p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p>

<p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p>

<p>(<a href="http://example.com/1" title="a">one</a>) (<a href="http://example.com/2" title="b">two</a>)</p>

```

### nested_code

Example undefined

```html
<p><code>hi ther `` ok ```</code></p>

<p><code>`</code></p>

<p><code>There is a literal backtick (`) here.</code></p>

<p>A backtick-delimited string in a code span: <code>`foo`</code></p>

<p>Please don&#39;t use any <code>&lt;blink&gt;</code> tags.</p>
```

### autolink_lines

Example undefined

```html
<p>hello world
<a href="http://example.com">http://example.com</a>
</p>

```

### main

Example undefined

```html
<h1 id="a-heading">A heading</h1> <p>Just a note, I&#39;ve found that I can&#39;t test my markdown parser vs others. For example, both markdown.js and showdown code blocks in lists wrong. They&#39;re  also completely <a href="http://google.com/" title="Google">inconsistent</a> with regards to paragraphs in list items.</p> <p>A link. Not anymore.</p> <aside>This will make me fail the test because
markdown.js doesnt acknowledge arbitrary html blocks =/</aside> <ul><li><p>List Item 1</p></li><li><p>List Item 2 </p><ul><li>New List Item 1 Hi, this is a list item.</li><li>New List Item 2 Another item <pre><code>Code goes here.
Lots of it...</code></pre></li><li>New List Item 3 The last item</li></ul></li><li><p>List Item 3 The final item.</p></li><li><p>List Item 4 The real final item.</p></li></ul> <p>Paragraph.</p> <blockquote><ul><li>bq Item 1</li><li>bq Item 2 <ul><li>New bq Item 1</li><li>New bq Item 2 Text here</li></ul></li></ul></blockquote> <hr> <blockquote><p> Another blockquote!  I really need to get  more creative with  mockup text..  markdown.js breaks here again</p></blockquote> <h2 id="another-heading">Another Heading</h2> <p>Hello <em>world</em>. Here is a <a href="//hello">link</a>. And an image <img src="src" alt="alt">.</p> <pre><code>Code goes here.
Lots of it...</code></pre>

```

### table_cells

Example undefined

```html
<table><thead><tr><th>1</th></tr></thead><tbody><tr><td>1</td></tr></tbody></table>

<table><thead><tr><th>1</th></tr></thead><tbody><tr><td>|</td></tr></tbody></table>

<table><thead><tr><th>1</th></tr></thead><tbody><tr><td>1\1</td></tr></tbody></table>

<table><thead><tr><th>1</th></tr></thead><tbody><tr><td>\\</td></tr></tbody></table>

<table><thead><tr><th>1</th></tr></thead><tbody><tr><td>\\|</td></tr></tbody></table>

<table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table>

<table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1|\</td><td>2|\</td></tr></tbody></table>

<table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table>

<table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1|\</td><td>2|\</td></tr></tbody></table>

<table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table>

<table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table>

<table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table>

<table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table>

<table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table>

```

### link_lt

Example undefined

```html
<p><a href="%3Ctest">URL</a></p>

```

### substitutions

Example undefined

```html
<p>foo␤␤bar</p>

<p>* a *</p>

```

### code_spans

Example undefined

```html
<p><code>someone@example.com</code></p>

<p>``<em>test`</em></p>
```

### link_tick_redos

Example undefined

```html
<p>dash_capstyle: [&#39;butt&#39; | &#39;round&#39; | &#39;projecting&#39;]
dash_joinstyle: [&#39;miter&#39; | &#39;round&#39; | &#39;bevel&#39;]
dashes: sequence of on/off ink in points
drawstyle: [&#39;default&#39; | &#39;steps&#39; | &#39;steps-pre&#39; | &#39;steps-mid&#39; | &#39;steps-post&#39;]
figure: a <code>~.Figure</code> instance
fillstyle: [&#39;full&#39; | &#39;left&#39; | &#39;right&#39; | &#39;bottom&#39; | &#39;top&#39; | &#39;none&#39;]
gid: an id string
label: object
linestyle or ls: [&#39;solid&#39; | &#39;dashed&#39;, &#39;dashdot&#39;, &#39;dotted&#39; | (offset, on-off-dash-seq) | <code>&#39;-&#39;</code> | <code>&#39;--&#39;</code> | <code>&#39;-.&#39;</code> | <code>&#39;:&#39;</code> | <code>&#39;None&#39;</code> | <code>&#39; &#39;</code> | <code>&#39;&#39;</code>]
linewidth or lw: float value in points
marker: :mod:<code>A valid marker style &lt;matplotlib.markers&gt;</code>
markeredgecolor or mec: any matplotlib color
markeredgewidth or mew: float value in points
markerfacecolor or mfc: any matplotlib color
markerfacecoloralt or mfcalt: any matplotlib color
markersize or ms: float
markevery: [None | int | length-2 tuple of int | slice | list/array of int | float | length-2 tuple of float]
path_effects: <code>~.AbstractPathEffect</code>
picker: float distance in points or callable pick function <code>fn(artist, event)</code>
pickradius: float distance in points
rasterized: bool or None
sketch_params: (scale: float, length: float, randomness: float)
snap: bool or None
solid_capstyle: [&#39;butt&#39; | &#39;round&#39; |  &#39;projecting&#39;]
solid_joinstyle: [&#39;miter&#39; | &#39;round&#39; | &#39;bevel&#39;]
transform: a :class:<code>matplotlib.transforms.Transform</code> instance
url: a url string
visible: bool
xdata: 1D array
ydata: 1D array
zorder: float</p>

```

### images

Example undefined

```html
<p>Image</p>
<p>Image</p>
<p>Image</p>
<p>Image</p>
<p>Image</p>

```

### lazy_blockquotes

Example undefined

```html
<blockquote>
  <p>hi there
bud</p>
</blockquote>

```


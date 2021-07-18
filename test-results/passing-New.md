# New

## adjacent_lists

### Example undefined

This markdown:


````````````markdown
* This should be
* An unordered list

1. This should be
2. An unordered list

````````````

Gives this correct output:


````````````html
<ul>
<li>This should be</li>
<li>An unordered list</li>
</ul>

<ol>
<li>This should be</li>
<li>An unordered list</li>
</ol>

````````````

## autolink_lines

### Example undefined

This markdown:


````````````markdown
hello world
<http://example.com>

````````````

Gives this correct output:


````````````html
<p>hello world
<a href="http://example.com">http://example.com</a>
</p>

````````````

## blockquote_list_item

### Example undefined

This markdown:


````````````markdown
This fails in markdown.pl and upskirt:

* hello
  > world

````````````

Gives this correct output:


````````````html
<p>This fails in markdown.pl and upskirt:</p>

<ul><li>hello<blockquote><p>world</p></blockquote></li></ul>

````````````

## case_insensitive_refs

### Example undefined

This markdown:


````````````markdown
[hi]

[HI]: /url

````````````

Gives this correct output:


````````````html
<p><a href="/url">hi</a></p>

````````````

## code_spans

### Example undefined

This markdown:


````````````markdown
`someone@example.com`

``*test`*
````````````

Gives this correct output:


````````````html
<p><code>someone@example.com</code></p>

<p>``<em>test`</em></p>
````````````

## emphasis_extra tests

### Example undefined

This markdown:


````````````markdown
_test_. _test_: _test_! _test_? _test_-
````````````

Gives this correct output:


````````````html
<p><em>test</em>. <em>test</em>: <em>test</em>! <em>test</em>? <em>test</em>-</p>
````````````

## escaped_angles

### Example undefined

This markdown:


````````````markdown
\>

````````````

Gives this correct output:


````````````html
<p>&gt;</p>

````````````

## hr_list_break

### Example undefined

This markdown:


````````````markdown
* hello
world
* how
are
* * *
you today?

````````````

Gives this correct output:


````````````html
<ul>
<li>hello
world</li>
<li>how
are</li>
</ul>

<hr>

<p>you today?</p>

````````````

## lazy_blockquotes

### Example undefined

This markdown:


````````````markdown
> hi there
bud

````````````

Gives this correct output:


````````````html
<blockquote>
  <p>hi there
bud</p>
</blockquote>

````````````

## link_lt

### Example undefined

This markdown:


````````````markdown
[URL](<test)

````````````

Gives this correct output:


````````````html
<p><a href="%3Ctest">URL</a></p>

````````````

## link_tick_redos

### Example undefined

This markdown:


````````````markdown
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

````````````

Gives this correct output:


````````````html
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

````````````

## links

### Example undefined

This markdown:


````````````markdown
Link: [constructor][].

[One](https://example.com/1) ([Two](https://example.com/2)) [Three](https://example.com/3)

[constructor]: https://example.org/

````````````

Gives this correct output:


````````````html
<p>Link: <a href="https://example.org/">constructor</a>.</p>

<p><a href="https://example.com/1">One</a> (<a href="https://example.com/2">Two</a>) <a href="https://example.com/3">Three</a></p>

````````````

## links_paren

### Example undefined

This markdown:


````````````markdown
([one](http://example.com/1)) ([two](http://example.com/2))

([one](http://example.com/1))  ([two](http://example.com/2))

([one](http://example.com/1 "a")) ([two](http://example.com/2 "b"))

````````````

Gives this correct output:


````````````html
<p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p>

<p>(<a href="http://example.com/1">one</a>) (<a href="http://example.com/2">two</a>)</p>

<p>(<a href="http://example.com/1" title="a">one</a>) (<a href="http://example.com/2" title="b">two</a>)</p>

````````````

## list_table

### Example undefined

This markdown:


````````````markdown
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

````````````

Gives this correct output:


````````````html
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

````````````

## nested_code

### Example undefined

This markdown:


````````````markdown
````` hi ther `` ok ``` `````

`` ` ``

``There is a literal backtick (`) here.``

A backtick-delimited string in a code span: `` `foo` ``

Please don't use any `<blink>` tags.
````````````

Gives this correct output:


````````````html
<p><code>hi ther `` ok ```</code></p>

<p><code>`</code></p>

<p><code>There is a literal backtick (`) here.</code></p>

<p>A backtick-delimited string in a code span: <code>`foo`</code></p>

<p>Please don&#39;t use any <code>&lt;blink&gt;</code> tags.</p>
````````````

## nested_em

### Example undefined

This markdown:


````````````markdown
*test **test** test*

_test __test__ test_

````````````

Gives this correct output:


````````````html
<p><em>test <strong>test</strong> test</em></p>

<p><em>test <strong>test</strong> test</em></p>

````````````

## nested_square_link

### Example undefined

This markdown:


````````````markdown
[the `]` character](/url)

[the \` character](/url)

````````````

Gives this correct output:


````````````html
<p><a href="/url">the <code>]</code> character</a></p>

<p><a href="/url">the ` character</a></p>

````````````

## not_a_link

### Example undefined

This markdown:


````````````markdown
\[test](not a link)

````````````

Gives this correct output:


````````````html
<p>[test](not a link)</p>

````````````

## substitutions

### Example undefined

This markdown:


````````````markdown
foo␤␤bar

* a *

````````````

Gives this correct output:


````````````html
<p>foo␤␤bar</p>

<p>* a *</p>

````````````

## tricky_list

### Example undefined

This markdown:


````````````markdown
**hello** _world_

* hello world

**hello** _world_

* hello world

**hello** _world_

* Hello world

**hello** _world_

* hello world

````````````

Gives this correct output:


````````````html
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

````````````


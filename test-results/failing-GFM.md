## GFM

### Blank lines

Example 197

```html
<p>aaa</p>
<h1>aaa</h1>

```

### Autolinks

Example 590

```html
<p><a href="http://foo.bar.baz">http://foo.bar.baz</a></p>

```

Example 591

```html
<p><a href="http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>

```

Example 592

```html
<p><a href="irc://foo.bar:2233/baz">irc://foo.bar:2233/baz</a></p>

```

Example 593

```html
<p><a href="MAILTO:FOO@BAR.BAZ">MAILTO:FOO@BAR.BAZ</a></p>

```

Example 594

```html
<p><a href="a+b+c:d">a+b+c:d</a></p>

```

Example 595

```html
<p><a href="made-up-scheme://foo,bar">made-up-scheme://foo,bar</a></p>

```

Example 596

```html
<p><a href="http://../">http://../</a></p>

```

Example 597

```html
<p><a href="localhost:5001/foo">localhost:5001/foo</a></p>

```

Example 598

```html
<p>&lt;http://foo.bar/baz bim&gt;</p>

```

Example 599

```html
<p><a href="http://example.com/%5C%5B%5C">http://example.com/\[\</a></p>

```

Example 600

```html
<p><a href="mailto:foo@bar.example.com">foo@bar.example.com</a></p>

```

Example 601

```html
<p><a href="mailto:foo+special@Bar.baz-bar0.com">foo+special@Bar.baz-bar0.com</a></p>

```

Example 602

```html
<p>&lt;foo+@bar.example.com&gt;</p>

```

Example 603

```html
<p>&lt;&gt;</p>

```

Example 604

```html
<p>&lt; http://foo.bar &gt;</p>

```

Example 605

```html
<p>&lt;m:abc&gt;</p>

```

Example 606

```html
<p>&lt;foo.bar.baz&gt;</p>

```

### Block quotes

Example 198

```html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

```

Example 199

```html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

```

Example 200

```html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

```

Example 201

```html
<pre><code>&gt; # Foo
&gt; bar
&gt; baz
</code></pre>

```

Example 202

```html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

```

Example 203

```html
<blockquote>
<p>bar
baz
foo</p>
</blockquote>

```

Example 204

```html
<blockquote>
<p>foo</p>
</blockquote>
<hr />

```

Example 205

```html
<blockquote>
<ul>
<li>foo</li>
</ul>
</blockquote>
<ul>
<li>bar</li>
</ul>

```

Example 206

```html
<blockquote>
<pre><code>foo
</code></pre>
</blockquote>
<pre><code>bar
</code></pre>

```

Example 207

```html
<blockquote>
<pre><code></code></pre>
</blockquote>
<p>foo</p>
<pre><code></code></pre>

```

Example 208

```html
<blockquote>
<p>foo
- bar</p>
</blockquote>

```

Example 209

```html
<blockquote>
</blockquote>

```

Example 210

```html
<blockquote>
</blockquote>

```

Example 211

```html
<blockquote>
<p>foo</p>
</blockquote>

```

Example 212

```html
<blockquote>
<p>foo</p>
</blockquote>
<blockquote>
<p>bar</p>
</blockquote>

```

Example 213

```html
<blockquote>
<p>foo
bar</p>
</blockquote>

```

Example 214

```html
<blockquote>
<p>foo</p>
<p>bar</p>
</blockquote>

```

Example 215

```html
<p>foo</p>
<blockquote>
<p>bar</p>
</blockquote>

```

Example 216

```html
<blockquote>
<p>aaa</p>
</blockquote>
<hr />
<blockquote>
<p>bbb</p>
</blockquote>

```

Example 217

```html
<blockquote>
<p>bar
baz</p>
</blockquote>

```

Example 218

```html
<blockquote>
<p>bar</p>
</blockquote>
<p>baz</p>

```

Example 219

```html
<blockquote>
<p>bar</p>
</blockquote>
<p>baz</p>

```

Example 220

```html
<blockquote>
<blockquote>
<blockquote>
<p>foo
bar</p>
</blockquote>
</blockquote>
</blockquote>

```

Example 221

```html
<blockquote>
<blockquote>
<blockquote>
<p>foo
bar
baz</p>
</blockquote>
</blockquote>
</blockquote>

```

Example 222

```html
<blockquote>
<pre><code>code
</code></pre>
</blockquote>
<blockquote>
<p>not code</p>
</blockquote>

```

### Lists

Example 271

```html
<ul>
<li>foo</li>
<li>bar</li>
</ul>
<ul>
<li>baz</li>
</ul>

```

Example 272

```html
<ol>
<li>foo</li>
<li>bar</li>
</ol>
<ol start="3">
<li>baz</li>
</ol>

```

Example 273

```html
<p>Foo</p>
<ul>
<li>bar</li>
<li>baz</li>
</ul>

```

Example 274

```html
<p>The number of windows in my house is
14.  The number of doors is 6.</p>

```

Example 275

```html
<p>The number of windows in my house is</p>
<ol>
<li>The number of doors is 6.</li>
</ol>

```

Example 276

```html
<ul>
<li>
<p>foo</p>
</li>
<li>
<p>bar</p>
</li>
<li>
<p>baz</p>
</li>
</ul>

```

Example 277

```html
<ul>
<li>foo
<ul>
<li>bar
<ul>
<li>
<p>baz</p>
<p>bim</p>
</li>
</ul>
</li>
</ul>
</li>
</ul>

```

Example 278

```html
<ul>
<li>foo</li>
<li>bar</li>
</ul>
<!-- -->
<ul>
<li>baz</li>
<li>bim</li>
</ul>

```

Example 279

```html
<ul>
<li>
<p>foo</p>
<p>notcode</p>
</li>
<li>
<p>foo</p>
</li>
</ul>
<!-- -->
<pre><code>code
</code></pre>

```

Example 280

```html
<ul>
<li>a</li>
<li>b</li>
<li>c</li>
<li>d</li>
<li>e</li>
<li>f</li>
<li>g</li>
</ul>

```

Example 281

```html
<ol>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>c</p>
</li>
</ol>

```

Example 282

```html
<ul>
<li>a</li>
<li>b</li>
<li>c</li>
<li>d
- e</li>
</ul>

```

Example 283

```html
<ol>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
</ol>
<pre><code>3. c
</code></pre>

```

Example 284

```html
<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>c</p>
</li>
</ul>

```

Example 285

```html
<ul>
<li>
<p>a</p>
</li>
<li></li>
<li>
<p>c</p>
</li>
</ul>

```

Example 286

```html
<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
<p>c</p>
</li>
<li>
<p>d</p>
</li>
</ul>

```

Example 287

```html
<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>d</p>
</li>
</ul>

```

Example 288

```html
<ul>
<li>a</li>
<li>
<pre><code>b


</code></pre>
</li>
<li>c</li>
</ul>

```

Example 289

```html
<ul>
<li>a
<ul>
<li>
<p>b</p>
<p>c</p>
</li>
</ul>
</li>
<li>d</li>
</ul>

```

Example 290

```html
<ul>
<li>a
<blockquote>
<p>b</p>
</blockquote>
</li>
<li>c</li>
</ul>

```

Example 291

```html
<ul>
<li>a
<blockquote>
<p>b</p>
</blockquote>
<pre><code>c
</code></pre>
</li>
<li>d</li>
</ul>

```

Example 292

```html
<ul>
<li>a</li>
</ul>

```

Example 293

```html
<ul>
<li>a
<ul>
<li>b</li>
</ul>
</li>
</ul>

```

Example 294

```html
<ol>
<li>
<pre><code>foo
</code></pre>
<p>bar</p>
</li>
</ol>

```

Example 295

```html
<ul>
<li>
<p>foo</p>
<ul>
<li>bar</li>
</ul>
<p>baz</p>
</li>
</ul>

```

Example 296

```html
<ul>
<li>
<p>a</p>
<ul>
<li>b</li>
<li>c</li>
</ul>
</li>
<li>
<p>d</p>
<ul>
<li>e</li>
<li>f</li>
</ul>
</li>
</ul>

```

### Thematic breaks

Example 16

```html
<p>--
**
__</p>

```

Example 19

```html
<p>Foo
***</p>

```

Example 21

```html
<hr />

```

Example 22

```html
<hr />

```

Example 23

```html
<hr />

```

Example 24

```html
<hr />

```

Example 25

```html
<p>_ _ _ _ a</p>
<p>a------</p>
<p>---a---</p>

```

Example 26

```html
<p><em>-</em></p>

```

Example 27

```html
<ul>
<li>foo</li>
</ul>
<hr />
<ul>
<li>bar</li>
</ul>

```

Example 29

```html
<h2>Foo</h2>
<p>bar</p>

```

Example 30

```html
<ul>
<li>Foo</li>
</ul>
<hr />
<ul>
<li>Bar</li>
</ul>

```

Example 31

```html
<ul>
<li>Foo</li>
<li>
<hr />
</li>
</ul>

```

### Fenced code blocks

Example 91

```html
<p><code>foo</code></p>

```

Example 94

```html
<pre><code>aaa
```
</code></pre>

```

Example 95

```html
<pre><code>aaa
~~~
</code></pre>

```

Example 96

```html
<pre><code></code></pre>

```

Example 97

```html
<pre><code>
```
aaa
</code></pre>

```

Example 98

```html
<blockquote>
<pre><code>aaa
</code></pre>
</blockquote>
<p>bbb</p>

```

Example 100

```html
<pre><code></code></pre>

```

Example 101

```html
<pre><code>aaa
aaa
</code></pre>

```

Example 102

```html
<pre><code>aaa
aaa
aaa
</code></pre>

```

Example 103

```html
<pre><code>aaa
 aaa
aaa
</code></pre>

```

Example 104

```html
<pre><code>```
aaa
```
</code></pre>

```

Example 105

```html
<pre><code>aaa
</code></pre>

```

Example 106

```html
<pre><code>aaa
</code></pre>

```

Example 107

```html
<pre><code>aaa
    ```
</code></pre>

```

Example 108

```html
<p><code> </code>
aaa</p>

```

Example 109

```html
<pre><code>aaa
~~~ ~~
</code></pre>

```

Example 111

```html
<h2>foo</h2>
<pre><code>bar
</code></pre>
<h1>baz</h1>

```

Example 112

```html
<pre><code class="language-ruby">def foo(x)
  return 3
end
</code></pre>

```

Example 113

```html
<pre><code class="language-ruby">def foo(x)
  return 3
end
</code></pre>

```

Example 114

```html
<pre><code class="language-;"></code></pre>

```

Example 115

```html
<p><code>aa</code>
foo</p>

```

Example 116

```html
<pre><code class="language-aa">foo
</code></pre>

```

### ATX headings

Example 33

```html
<p>####### foo</p>

```

Example 34

```html
<p>#5 bolt</p>
<p>#hashtag</p>

```

Example 35

```html
<p>## foo</p>

```

Example 36

```html
<h1>foo <em>bar</em> *baz*</h1>

```

Example 38

```html
<h3>foo</h3>
<h2>foo</h2>
<h1>foo</h1>

```

Example 40

```html
<p>foo
# bar</p>

```

Example 41

```html
<h2>foo</h2>
<h3>bar</h3>

```

Example 43

```html
<h3>foo</h3>

```

Example 45

```html
<h1>foo#</h1>

```

Example 46

```html
<h3>foo ###</h3>
<h2>foo ###</h2>
<h1>foo #</h1>

```

### HTML blocks

Example 118

```html
<table><tr><td>
<pre>
**Hello**,
<p><em>world</em>.
</pre></p>
</td></tr></table>

```

Example 119

```html
<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>
<p>okay.</p>

```

Example 120

```html
 <div>
  *hello*
         <foo><a>

```

Example 121

```html
</div>
*foo*

```

Example 122

```html
<DIV CLASS="foo">
<p><em>Markdown</em></p>
</DIV>

```

Example 123

```html
<div id="foo"
  class="bar">
</div>

```

Example 124

```html
<div id="foo" class="bar
  baz">
</div>

```

Example 125

```html
<div>
*foo*
<p><em>bar</em></p>

```

Example 126

```html
<div id="foo"
*hi*

```

Example 127

```html
<div class
foo

```

Example 128

```html
<div *???-&&&-<---
*foo*

```

Example 129

```html
<div><a href="bar">*foo*</a></div>

```

Example 130

```html
<table><tr><td>
foo
</td></tr></table>

```

Example 131

```html
<div></div>
``` c
int x = 33;
```

```

Example 132

```html
<a href="foo">
*bar*
</a>

```

Example 133

```html
<Warning>
*bar*
</Warning>

```

Example 134

```html
<i class="foo">
*bar*
</i>

```

Example 135

```html
</ins>
*bar*

```

Example 136

```html
<del>
*foo*
</del>

```

Example 137

```html
<del>
<p><em>foo</em></p>
</del>

```

Example 138

```html
<p><del><em>foo</em></del></p>

```

Example 139

```html
<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
<p>okay</p>

```

Example 140

```html
<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
</script>
<p>okay</p>

```

Example 141

```html
<style
  type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
<p>okay</p>

```

Example 142

```html
<style
  type="text/css">

foo

```

Example 143

```html
<blockquote>
<div>
foo
</blockquote>
<p>bar</p>

```

Example 144

```html
<ul>
<li>
<div>
</li>
<li>foo</li>
</ul>

```

Example 145

```html
<style>p{color:red;}</style>
<p><em>foo</em></p>

```

Example 146

```html
<!-- foo -->*bar*
<p><em>baz</em></p>

```

Example 147

```html
<script>
foo
</script>1. *bar*

```

Example 148

```html
<!-- Foo

bar
   baz -->
<p>okay</p>

```

Example 149

```html
<?php

  echo '>';

?>
<p>okay</p>

```

Example 150

```html
<!DOCTYPE html>

```

Example 151

```html
<![CDATA[
function matchwo(a,b)
{
  if (a < b && a < 0) then {
    return 1;

  } else {

    return 0;
  }
}
]]>
<p>okay</p>

```

Example 152

```html
  <!-- foo -->
<pre><code>&lt;!-- foo --&gt;
</code></pre>

```

Example 153

```html
  <div>
<pre><code>&lt;div&gt;
</code></pre>

```

Example 154

```html
<p>Foo</p>
<div>
bar
</div>

```

Example 155

```html
<div>
bar
</div>
*foo*

```

Example 156

```html
<p>Foo
<a href="bar">
baz</p>

```

Example 157

```html
<div>
<p><em>Emphasized</em> text.</p>
</div>

```

Example 158

```html
<div>
*Emphasized* text.
</div>

```

Example 159

```html
<table>
<tr>
<td>
Hi
</td>
</tr>
</table>

```

Example 160

```html
<table>
  <tr>
<pre><code>&lt;td&gt;
  Hi
&lt;/td&gt;
</code></pre>
  </tr>
</table>

```

### List items

Example 223

```html
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>

```

Example 224

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 225

```html
<ul>
<li>one</li>
</ul>
<p>two</p>

```

Example 226

```html
<ul>
<li>
<p>one</p>
<p>two</p>
</li>
</ul>

```

Example 227

```html
<ul>
<li>one</li>
</ul>
<pre><code> two
</code></pre>

```

Example 228

```html
<ul>
<li>
<p>one</p>
<p>two</p>
</li>
</ul>

```

Example 229

```html
<blockquote>
<blockquote>
<ol>
<li>
<p>one</p>
<p>two</p>
</li>
</ol>
</blockquote>
</blockquote>

```

Example 230

```html
<blockquote>
<blockquote>
<ul>
<li>one</li>
</ul>
<p>two</p>
</blockquote>
</blockquote>

```

Example 231

```html
<p>-one</p>
<p>2.two</p>

```

Example 232

```html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

```

Example 233

```html
<ol>
<li>
<p>foo</p>
<pre><code>bar
</code></pre>
<p>baz</p>
<blockquote>
<p>bam</p>
</blockquote>
</li>
</ol>

```

Example 234

```html
<ul>
<li>
<p>Foo</p>
<pre><code>bar


baz
</code></pre>
</li>
</ul>

```

Example 235

```html
<ol start="123456789">
<li>ok</li>
</ol>

```

Example 237

```html
<ol start="0">
<li>ok</li>
</ol>

```

Example 238

```html
<ol start="3">
<li>ok</li>
</ol>

```

Example 239

```html
<p>-1. not ok</p>

```

Example 240

```html
<ul>
<li>
<p>foo</p>
<pre><code>bar
</code></pre>
</li>
</ul>

```

Example 241

```html
<ol start="10">
<li>
<p>foo</p>
<pre><code>bar
</code></pre>
</li>
</ol>

```

Example 243

```html
<ol>
<li>
<pre><code>indented code
</code></pre>
<p>paragraph</p>
<pre><code>more code
</code></pre>
</li>
</ol>

```

Example 244

```html
<ol>
<li>
<pre><code> indented code
</code></pre>
<p>paragraph</p>
<pre><code>more code
</code></pre>
</li>
</ol>

```

Example 245

```html
<p>foo</p>
<p>bar</p>

```

Example 246

```html
<ul>
<li>foo</li>
</ul>
<p>bar</p>

```

Example 247

```html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

```

Example 248

```html
<ul>
<li>foo</li>
<li>
<pre><code>bar
</code></pre>
</li>
<li>
<pre><code>baz
</code></pre>
</li>
</ul>

```

Example 249

```html
<ul>
<li>foo</li>
</ul>

```

Example 250

```html
<ul>
<li></li>
</ul>
<p>foo</p>

```

Example 251

```html
<ul>
<li>foo</li>
<li></li>
<li>bar</li>
</ul>

```

Example 252

```html
<ul>
<li>foo</li>
<li></li>
<li>bar</li>
</ul>

```

Example 253

```html
<ol>
<li>foo</li>
<li></li>
<li>bar</li>
</ol>

```

Example 254

```html
<ul>
<li></li>
</ul>

```

Example 255

```html
<p>foo
*</p>
<p>foo
1.</p>

```

Example 256

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 257

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 258

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 259

```html
<pre><code>1.  A paragraph
    with two lines.

        indented code

    &gt; A block quote.
</code></pre>

```

Example 260

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 261

```html
<ol>
<li>A paragraph
with two lines.</li>
</ol>

```

Example 262

```html
<blockquote>
<ol>
<li>
<blockquote>
<p>Blockquote
continued here.</p>
</blockquote>
</li>
</ol>
</blockquote>

```

Example 263

```html
<blockquote>
<ol>
<li>
<blockquote>
<p>Blockquote
continued here.</p>
</blockquote>
</li>
</ol>
</blockquote>

```

Example 264

```html
<ul>
<li>foo
<ul>
<li>bar
<ul>
<li>baz
<ul>
<li>boo</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>

```

Example 265

```html
<ul>
<li>foo</li>
<li>bar</li>
<li>baz</li>
<li>boo</li>
</ul>

```

Example 266

```html
<ol start="10">
<li>foo
<ul>
<li>bar</li>
</ul>
</li>
</ol>

```

Example 267

```html
<ol start="10">
<li>foo</li>
</ol>
<ul>
<li>bar</li>
</ul>

```

Example 268

```html
<ul>
<li>
<ul>
<li>foo</li>
</ul>
</li>
</ul>

```

Example 269

```html
<ol>
<li>
<ul>
<li>
<ol start="2">
<li>foo</li>
</ol>
</li>
</ul>
</li>
</ol>

```

Example 270

```html
<ul>
<li>
<h1>Foo</h1>
</li>
<li>
<h2>Bar</h2>
baz</li>
</ul>

```

### [extension] Disallowed Raw HTML

Example 653

```html
<p><strong> &lt;title> &lt;style> <em></p>
<blockquote>
  &lt;xmp> is disallowed.  &lt;XMP> is also disallowed.
</blockquote>
```

### Setext headings

Example 50

```html
<h1>Foo <em>bar</em></h1>
<h2>Foo <em>bar</em></h2>

```

Example 51

```html
<h1>Foo <em>bar
baz</em></h1>

```

Example 52

```html
<h1>Foo <em>bar
baz</em></h1>

```

Example 53

```html
<h2>Foo</h2>
<h1>Foo</h1>

```

Example 54

```html
<h2>Foo</h2>
<h2>Foo</h2>
<h1>Foo</h1>

```

Example 55

```html
<pre><code>Foo
---

Foo
</code></pre>
<hr />

```

Example 56

```html
<h2>Foo</h2>

```

Example 57

```html
<p>Foo
---</p>

```

Example 58

```html
<p>Foo
= =</p>
<p>Foo</p>
<hr />

```

Example 59

```html
<h2>Foo</h2>

```

Example 60

```html
<h2>Foo\</h2>

```

Example 61

```html
<h2>`Foo</h2>
<p>`</p>
<h2>&lt;a title=&quot;a lot</h2>
<p>of dashes&quot;/&gt;</p>

```

Example 62

```html
<blockquote>
<p>Foo</p>
</blockquote>
<hr />

```

Example 63

```html
<blockquote>
<p>foo
bar
===</p>
</blockquote>

```

Example 64

```html
<ul>
<li>Foo</li>
</ul>
<hr />

```

Example 65

```html
<h2>Foo
Bar</h2>

```

Example 66

```html
<hr />
<h2>Foo</h2>
<h2>Bar</h2>
<p>Baz</p>

```

Example 69

```html
<ul>
<li>foo</li>
</ul>
<hr />

```

Example 71

```html
<blockquote>
<p>foo</p>
</blockquote>
<hr />

```

Example 72

```html
<h2>&gt; foo</h2>

```

Example 73

```html
<p>Foo</p>
<h2>bar</h2>
<p>baz</p>

```

Example 74

```html
<p>Foo
bar</p>
<hr />
<p>baz</p>

```

Example 75

```html
<p>Foo
bar</p>
<hr />
<p>baz</p>

```

Example 76

```html
<p>Foo
bar
---
baz</p>

```

### Backslash escapes

Example 298

```html
<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>

```

Example 300

```html
<p>*not emphasized*
&lt;br/&gt; not a tag
[not a link](/foo)
`not code`
1. not a list
* not a list
# not a heading
[foo]: /url &quot;not a reference&quot;
&amp;ouml; not a character entity</p>

```

Example 301

```html
<p>\<em>emphasis</em></p>

```

Example 302

```html
<p>foo<br />
bar</p>

```

Example 303

```html
<p><code>\[\`</code></p>

```

Example 306

```html
<p><a href="http://example.com?find=%5C*">http://example.com?find=\*</a></p>

```

Example 307

```html
<a href="/bar\/)">

```

Example 308

```html
<p><a href="/bar*" title="ti*tle">foo</a></p>

```

Example 309

```html
<p><a href="/bar*" title="ti*tle">foo</a></p>

```

Example 310

```html
<pre><code class="language-foo+bar">foo
</code></pre>

```

### Emphasis and strong emphasis

Example 351

```html
<p>a * foo bar*</p>

```

Example 352

```html
<p>a*&quot;foo&quot;*</p>

```

Example 353

```html
<p>* a *</p>

```

Example 356

```html
<p><em>foo bar</em></p>

```

Example 363

```html
<p>foo-<em>(bar)</em></p>

```

Example 364

```html
<p>_foo*</p>

```

Example 365

```html
<p>*foo bar *</p>

```

Example 366

```html
<p>*foo bar
*</p>

```

Example 367

```html
<p>*(*foo)</p>

```

Example 368

```html
<p><em>(<em>foo</em>)</em></p>

```

Example 372

```html
<p><em>(<em>foo</em>)</em></p>

```

Example 375

```html
<p><em>foo_bar_baz</em></p>

```

Example 376

```html
<p><em>(bar)</em>.</p>

```

Example 378

```html
<p>** foo bar**</p>

```

Example 379

```html
<p>a**&quot;foo&quot;**</p>

```

Example 381

```html
<p><strong>foo bar</strong></p>

```

Example 383

```html
<p>__
foo bar__</p>

```

Example 388

```html
<p><strong>foo, <strong>bar</strong>, baz</strong></p>

```

Example 389

```html
<p>foo-<strong>(bar)</strong></p>

```

Example 390

```html
<p>**foo bar **</p>

```

Example 391

```html
<p>**(**foo)</p>

```

Example 392

```html
<p><em>(<strong>foo</strong>)</em></p>

```

Example 393

```html
<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.
<em>Asclepias physocarpa</em>)</strong></p>

```

Example 394

```html
<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>

```

Example 398

```html
<p><em>(<strong>foo</strong>)</em></p>

```

Example 401

```html
<p><strong>foo__bar__baz</strong></p>

```

Example 402

```html
<p><strong>(bar)</strong>.</p>

```

Example 403

```html
<p><em>foo <a href="/url">bar</a></em></p>

```

Example 404

```html
<p><em>foo
bar</em></p>

```

Example 405

```html
<p><em>foo <strong>bar</strong> baz</em></p>

```

Example 406

```html
<p><em>foo <em>bar</em> baz</em></p>

```

Example 407

```html
<p><em><em>foo</em> bar</em></p>

```

Example 408

```html
<p><em>foo <em>bar</em></em></p>

```

Example 409

```html
<p><em>foo <strong>bar</strong> baz</em></p>

```

Example 410

```html
<p><em>foo<strong>bar</strong>baz</em></p>

```

Example 411

```html
<p><em>foo**bar</em></p>

```

Example 412

```html
<p><em><strong>foo</strong> bar</em></p>

```

Example 413

```html
<p><em>foo <strong>bar</strong></em></p>

```

Example 414

```html
<p><em>foo<strong>bar</strong></em></p>

```

Example 415

```html
<p>foo<em><strong>bar</strong></em>baz</p>

```

Example 416

```html
<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>

```

Example 417

```html
<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>

```

Example 418

```html
<p><em>foo <a href="/url"><em>bar</em></a></em></p>

```

Example 419

```html
<p>** is not an empty emphasis</p>

```

Example 420

```html
<p>**** is not an empty strong emphasis</p>

```

Example 421

```html
<p><strong>foo <a href="/url">bar</a></strong></p>

```

Example 422

```html
<p><strong>foo
bar</strong></p>

```

Example 423

```html
<p><strong>foo <em>bar</em> baz</strong></p>

```

Example 424

```html
<p><strong>foo <strong>bar</strong> baz</strong></p>

```

Example 425

```html
<p><strong><strong>foo</strong> bar</strong></p>

```

Example 426

```html
<p><strong>foo <strong>bar</strong></strong></p>

```

Example 427

```html
<p><strong>foo <em>bar</em> baz</strong></p>

```

Example 428

```html
<p><strong>foo<em>bar</em>baz</strong></p>

```

Example 429

```html
<p><strong><em>foo</em> bar</strong></p>

```

Example 430

```html
<p><strong>foo <em>bar</em></strong></p>

```

Example 431

```html
<p><strong>foo <em>bar <strong>baz</strong>
bim</em> bop</strong></p>

```

Example 432

```html
<p><strong>foo <a href="/url"><em>bar</em></a></strong></p>

```

Example 434

```html
<p>____ is not an empty strong emphasis</p>

```

Example 435

```html
<p>foo ***</p>

```

Example 436

```html
<p>foo <em>*</em></p>

```

Example 438

```html
<p>foo *****</p>

```

Example 439

```html
<p>foo <strong>*</strong></p>

```

Example 441

```html
<p>*<em>foo</em></p>

```

Example 442

```html
<p><em>foo</em>*</p>

```

Example 443

```html
<p>*<strong>foo</strong></p>

```

Example 444

```html
<p>***<em>foo</em></p>

```

Example 445

```html
<p><strong>foo</strong>*</p>

```

Example 446

```html
<p><em>foo</em>***</p>

```

Example 448

```html
<p>foo <em>_</em></p>

```

Example 449

```html
<p>foo <em>*</em></p>

```

Example 451

```html
<p>foo <strong>_</strong></p>

```

Example 452

```html
<p>foo <strong>*</strong></p>

```

Example 453

```html
<p>_<em>foo</em></p>

```

Example 454

```html
<p><em>foo</em>_</p>

```

Example 455

```html
<p>_<strong>foo</strong></p>

```

Example 456

```html
<p>___<em>foo</em></p>

```

Example 457

```html
<p><strong>foo</strong>_</p>

```

Example 458

```html
<p><em>foo</em>___</p>

```

Example 460

```html
<p><em><em>foo</em></em></p>

```

Example 461

```html
<p><strong>foo</strong></p>

```

Example 462

```html
<p><em><em>foo</em></em></p>

```

Example 463

```html
<p><strong><strong>foo</strong></strong></p>

```

Example 464

```html
<p><strong><strong>foo</strong></strong></p>

```

Example 465

```html
<p><strong><strong><strong>foo</strong></strong></strong></p>

```

Example 466

```html
<p><em><strong>foo</strong></em></p>

```

Example 467

```html
<p><em><strong><strong>foo</strong></strong></em></p>

```

Example 469

```html
<p><em>foo <strong>bar *baz bim</strong> bam</em></p>

```

Example 470

```html
<p>**foo <strong>bar baz</strong></p>

```

Example 471

```html
<p>*foo <em>bar baz</em></p>

```

Example 472

```html
<p>*<a href="/url">bar*</a></p>

```

Example 474

```html
<p>*<img src="foo" title="*"/></p>

```

Example 475

```html
<p>**<a href="**"></p>

```

Example 476

```html
<p>__<a href="__"></p>

```

Example 477

```html
<p><em>a <code>*</code></em></p>

```

Example 478

```html
<p><em>a <code>_</code></em></p>

```

Example 479

```html
<p>**a<a href="http://foo.bar/?q=**">http://foo.bar/?q=**</a></p>

```

Example 480

```html
<p>__a<a href="http://foo.bar/?q=__">http://foo.bar/?q=__</a></p>

```

### Indented code blocks

Example 77

```html
<pre><code>a simple
  indented code block
</code></pre>

```

Example 78

```html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

```

Example 79

```html
<ol>
<li>
<p>foo</p>
<ul>
<li>bar</li>
</ul>
</li>
</ol>

```

Example 80

```html
<pre><code>&lt;a/&gt;
*hi*

- one
</code></pre>

```

Example 81

```html
<pre><code>chunk1

chunk2



chunk3
</code></pre>

```

Example 82

```html
<pre><code>chunk1
  
  chunk2
</code></pre>

```

Example 83

```html
<p>Foo
bar</p>

```

Example 85

```html
<h1>Heading</h1>
<pre><code>foo
</code></pre>
<h2>Heading</h2>
<pre><code>foo
</code></pre>
<hr />

```

Example 86

```html
<pre><code>    foo
bar
</code></pre>

```

Example 87

```html
<pre><code>foo
</code></pre>

```

### [extension] Tables

Example 198

```html
<table>
<thead>
<tr>
<th>foo</th>
<th>bar</th>
</tr>
</thead>
<tbody>
<tr>
<td>baz</td>
<td>bim</td>
</tr>
</tbody>
</table>
```

Example 199

```html
<table>
<thead>
<tr>
<th align="center">abc</th>
<th align="right">defghi</th>
</tr>
</thead>
<tbody>
<tr>
<td align="center">bar</td>
<td align="right">baz</td>
</tr>
</tbody>
</table>
```

Example 200

```html
<table>
<thead>
<tr>
<th>f|oo</th>
</tr>
</thead>
<tbody>
<tr>
<td>b <code>|</code> az</td>
</tr>
<tr>
<td>b <strong>|</strong> im</td>
</tr>
</tbody>
</table>
```

Example 201

```html
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
</tbody>
</table>
<blockquote>
<p>bar</p>
</blockquote>
```

Example 202

```html
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
<tr>
<td>bar</td>
<td></td>
</tr>
</tbody>
</table>
<p>bar</p>
```

Example 203

```html
<p>| abc | def |
| --- |
| bar |</p>
```

Example 204

```html
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td></td>
</tr>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
</tbody>
</table>
```

Example 205

```html
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
</table>
```

### [extension] Autolinks

Example 621

```html
<p><a href="http://www.commonmark.org">www.commonmark.org</a></p>
```

Example 622

```html
<p>Visit <a href="http://www.commonmark.org/help">www.commonmark.org/help</a> for more information.</p>
```

Example 623

```html
<p>Visit <a href="http://www.commonmark.org">www.commonmark.org</a>.</p>
<p>Visit <a href="http://www.commonmark.org/a.b">www.commonmark.org/a.b</a>.</p>
```

Example 624

```html
<p><a href="http://www.google.com/search?q=Markup+(business)">www.google.com/search?q=Markup+(business)</a></p>
<p><a href="http://www.google.com/search?q=Markup+(business)">www.google.com/search?q=Markup+(business)</a>))</p>
<p>(<a href="http://www.google.com/search?q=Markup+(business)">www.google.com/search?q=Markup+(business)</a>)</p>
<p>(<a href="http://www.google.com/search?q=Markup+(business)">www.google.com/search?q=Markup+(business)</a></p>
```

Example 625

```html
<p><a href="http://www.google.com/search?q=(business))+ok">www.google.com/search?q=(business))+ok</a></p>
```

Example 626

```html
<p><a href="http://www.google.com/search?q=commonmark&amp;hl=en">www.google.com/search?q=commonmark&amp;hl=en</a></p>
<p><a href="http://www.google.com/search?q=commonmark">www.google.com/search?q=commonmark</a>&amp;hl;</p>
```

Example 627

```html
<p><a href="http://www.commonmark.org/he">www.commonmark.org/he</a>&lt;lp</p>
```

Example 628

```html
<p><a href="http://commonmark.org">http://commonmark.org</a></p>
<p>(Visit <a href="https://encrypted.google.com/search?q=Markup+(business)">https://encrypted.google.com/search?q=Markup+(business)</a>)</p>
<p>Anonymous FTP is available at <a href="ftp://foo.bar.baz">ftp://foo.bar.baz</a>.</p>
```

Example 629

```html
<p><a href="mailto:foo@bar.baz">foo@bar.baz</a></p>
```

Example 630

```html
<p>hello@mail+xyz.example isn't valid, but <a href="mailto:hello+xyz@mail.example">hello+xyz@mail.example</a> is.</p>
```

Example 631

```html
<p><a href="mailto:a.b-c_d@a.b">a.b-c_d@a.b</a></p>
<p><a href="mailto:a.b-c_d@a.b">a.b-c_d@a.b</a>.</p>
<p>a.b-c_d@a.b-</p>
<p>a.b-c_d@a.b_</p>
```

### Code spans

Example 329

```html
<p><code>foo ` bar</code></p>

```

Example 330

```html
<p><code>``</code></p>

```

Example 331

```html
<p><code> `` </code></p>

```

Example 334

```html
<p><code> </code>
<code>  </code></p>

```

Example 335

```html
<p><code>foo bar   baz</code></p>

```

Example 336

```html
<p><code>foo </code></p>

```

Example 337

```html
<p><code>foo   bar  baz</code></p>

```

Example 338

```html
<p><code>foo\</code>bar`</p>

```

Example 339

```html
<p><code>foo`bar</code></p>

```

Example 340

```html
<p><code>foo `` bar</code></p>

```

Example 341

```html
<p>*foo<code>*</code></p>

```

Example 342

```html
<p>[not a <code>link](/foo</code>)</p>

```

Example 343

```html
<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>

```

Example 344

```html
<p><a href="`">`</p>

```

Example 345

```html
<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>

```

Example 346

```html
<p><a href="http://foo.bar.%60baz">http://foo.bar.`baz</a>`</p>

```

Example 347

```html
<p>```foo``</p>

```

Example 348

```html
<p>`foo</p>

```

Example 349

```html
<p>`foo<code>bar</code></p>

```

### Inlines

Example 297

```html
<p><code>hi</code>lo`</p>

```

### Links

Example 481

```html
<p><a href="/uri" title="title">link</a></p>

```

Example 484

```html
<p><a href="">link</a></p>

```

Example 485

```html
<p>[link](/my uri)</p>

```

Example 486

```html
<p><a href="/my%20uri">link</a></p>

```

Example 487

```html
<p>[link](foo
bar)</p>

```

Example 488

```html
<p>[link](<foo
bar>)</p>

```

Example 489

```html
<p><a href="b)c">a</a></p>

```

Example 490

```html
<p>[link](&lt;foo&gt;)</p>

```

Example 491

```html
<p>[a](&lt;b)c
[a](&lt;b)c&gt;
[a](<b>c)</p>

```

Example 492

```html
<p><a href="(foo)">link</a></p>

```

Example 493

```html
<p><a href="foo(and(bar))">link</a></p>

```

Example 494

```html
<p><a href="foo(and(bar)">link</a></p>

```

Example 495

```html
<p><a href="foo(and(bar)">link</a></p>

```

Example 496

```html
<p><a href="foo):">link</a></p>

```

Example 498

```html
<p><a href="foo%5Cbar">link</a></p>

```

Example 499

```html
<p><a href="foo%20b%C3%A4">link</a></p>

```

Example 500

```html
<p><a href="%22title%22">link</a></p>

```

Example 501

```html
<p><a href="/url" title="title">link</a>
<a href="/url" title="title">link</a>
<a href="/url" title="title">link</a></p>

```

Example 502

```html
<p><a href="/url" title="title &quot;&quot;">link</a></p>

```

Example 503

```html
<p><a href="/url%C2%A0%22title%22">link</a></p>

```

Example 504

```html
<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>

```

Example 505

```html
<p><a href="/url" title="title &quot;and&quot; title">link</a></p>

```

Example 506

```html
<p><a href="/uri" title="title">link</a></p>

```

Example 507

```html
<p>[link] (/uri)</p>

```

Example 508

```html
<p><a href="/uri">link [foo [bar]]</a></p>

```

Example 509

```html
<p>[link] bar](/uri)</p>

```

Example 510

```html
<p>[link <a href="/uri">bar</a></p>

```

Example 511

```html
<p><a href="/uri">link [bar</a></p>

```

Example 512

```html
<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>

```

Example 513

```html
<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>

```

Example 514

```html
<p>[foo <a href="/uri">bar</a>](/uri)</p>

```

Example 515

```html
<p>[foo <em>[bar <a href="/uri">baz</a>](/uri)</em>](/uri)</p>

```

Example 516

```html
<p><img src="uri3" alt="[foo](uri2)" /></p>

```

Example 517

```html
<p>*<a href="/uri">foo*</a></p>

```

Example 518

```html
<p><a href="baz*">foo *bar</a></p>

```

Example 519

```html
<p><em>foo [bar</em> baz]</p>

```

Example 520

```html
<p>[foo <bar attr="](baz)"></p>

```

Example 521

```html
<p>[foo<code>](/uri)</code></p>

```

Example 522

```html
<p>[foo<a href="http://example.com/?search=%5D(uri)">http://example.com/?search=](uri)</a></p>

```

Example 523

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 524

```html
<p><a href="/uri">link [foo [bar]]</a></p>

```

Example 525

```html
<p><a href="/uri">link [bar</a></p>

```

Example 526

```html
<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>

```

Example 527

```html
<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>

```

Example 528

```html
<p>[foo <a href="/uri">bar</a>]<a href="/uri">ref</a></p>

```

Example 529

```html
<p>[foo <em>bar <a href="/uri">baz</a></em>]<a href="/uri">ref</a></p>

```

Example 530

```html
<p>*<a href="/uri">foo*</a></p>

```

Example 531

```html
<p><a href="/uri">foo *bar</a></p>

```

Example 532

```html
<p>[foo <bar attr="][ref]"></p>

```

Example 533

```html
<p>[foo<code>][ref]</code></p>

```

Example 534

```html
<p>[foo<a href="http://example.com/?search=%5D%5Bref%5D">http://example.com/?search=][ref]</a></p>

```

Example 535

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 536

```html
<p><a href="/url">Толпой</a> is a Russian word.</p>

```

Example 537

```html
<p><a href="/url">Baz</a></p>

```

Example 538

```html
<p>[foo] <a href="/url" title="title">bar</a></p>

```

Example 539

```html
<p>[foo]
<a href="/url" title="title">bar</a></p>

```

Example 540

```html
<p><a href="/url1">bar</a></p>

```

Example 541

```html
<p>[bar][foo!]</p>

```

Example 542

```html
<p>[foo][ref[]</p>
<p>[ref[]: /uri</p>

```

Example 543

```html
<p>[foo][ref[bar]]</p>
<p>[ref[bar]]: /uri</p>

```

Example 544

```html
<p>[[[foo]]]</p>
<p>[[[foo]]]: /url</p>

```

Example 545

```html
<p><a href="/uri">foo</a></p>

```

Example 546

```html
<p><a href="/uri">bar\</a></p>

```

Example 547

```html
<p>[]</p>
<p>[]: /uri</p>

```

Example 548

```html
<p>[
]</p>
<p>[
]: /uri</p>

```

Example 549

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 550

```html
<p><a href="/url" title="title"><em>foo</em> bar</a></p>

```

Example 551

```html
<p><a href="/url" title="title">Foo</a></p>

```

Example 552

```html
<p><a href="/url" title="title">foo</a>
[]</p>

```

Example 553

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 554

```html
<p><a href="/url" title="title"><em>foo</em> bar</a></p>

```

Example 555

```html
<p>[<a href="/url" title="title"><em>foo</em> bar</a>]</p>

```

Example 556

```html
<p>[[bar <a href="/url">foo</a></p>

```

Example 557

```html
<p><a href="/url" title="title">Foo</a></p>

```

Example 558

```html
<p><a href="/url">foo</a> bar</p>

```

Example 559

```html
<p>[foo]</p>

```

Example 560

```html
<p>*<a href="/url">foo*</a></p>

```

Example 561

```html
<p><a href="/url2">foo</a></p>

```

Example 562

```html
<p><a href="/url1">foo</a></p>

```

Example 563

```html
<p><a href="">foo</a></p>

```

Example 564

```html
<p><a href="/url1">foo</a>(not a link)</p>

```

Example 565

```html
<p>[foo]<a href="/url">bar</a></p>

```

Example 566

```html
<p><a href="/url2">foo</a><a href="/url1">baz</a></p>

```

Example 567

```html
<p>[foo]<a href="/url1">bar</a></p>

```

### [extension] Strikethrough

Example 491

```html
<p><del>Hi</del> Hello, world!</p>
```

### Soft line breaks

Example 645

```html
<p>foo
baz</p>

```

Example 646

```html
<p>foo
baz</p>

```

### [extension] Task list items

Example 279

```html
<ul>
<li><input disabled="" type="checkbox"> foo</li>
<li><input checked="" disabled="" type="checkbox"> bar</li>
</ul>
```

Example 280

```html
<ul>
<li><input checked="" disabled="" type="checkbox"> foo
<ul>
<li><input disabled="" type="checkbox"> bar</li>
<li><input checked="" disabled="" type="checkbox"> baz</li>
</ul>
</li>
<li><input disabled="" type="checkbox"> bim</li>
</ul>
```

### Entity and numeric character references

Example 311

```html
<p>  &amp; © Æ Ď
¾ ℋ ⅆ
∲ ≧̸</p>

```

Example 312

```html
<p># Ӓ Ϡ �</p>

```

Example 313

```html
<p>&quot; ആ ಫ</p>

```

Example 314

```html
<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;
&amp;#987654321;
&amp;#abcdef0;
&amp;ThisIsNotDefined; &amp;hi?;</p>

```

Example 317

```html
<a href="&ouml;&ouml;.html">

```

Example 318

```html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>

```

Example 319

```html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>

```

Example 320

```html
<pre><code class="language-föö">foo
</code></pre>

```

Example 323

```html
<p>*foo*
<em>foo</em></p>

```

Example 324

```html
<p>* foo</p>
<ul>
<li>foo</li>
</ul>

```

Example 325

```html
<p>foo

bar</p>

```

Example 326

```html
<p>	foo</p>

```

Example 327

```html
<p>[a](url &quot;tit&quot;)</p>

```

### Raw HTML

Example 609

```html
<p><a><bab><c2c></p>

```

Example 610

```html
<p><a/><b2/></p>

```

Example 611

```html
<p><a  /><b2
data="foo" ></p>

```

Example 612

```html
<p><a foo="bar" bam = 'baz <em>"</em>'
_boolean zoop:33=zoop:33 /></p>

```

Example 613

```html
<p>Foo <responsive-image src="foo.jpg" /></p>

```

Example 614

```html
<p>&lt;33&gt; &lt;__&gt;</p>

```

Example 615

```html
<p>&lt;a h*#ref=&quot;hi&quot;&gt;</p>

```

Example 616

```html
<p>&lt;a href=&quot;hi'&gt; &lt;a href=hi'&gt;</p>

```

Example 617

```html
<p>&lt; a&gt;&lt;
foo&gt;&lt;bar/ &gt;
&lt;foo bar=baz
bim!bop /&gt;</p>

```

Example 618

```html
<p>&lt;a href='bar'title=title&gt;</p>

```

Example 619

```html
<p></a></foo ></p>

```

Example 620

```html
<p>&lt;/a href=&quot;foo&quot;&gt;</p>

```

Example 621

```html
<p>foo <!-- this is a
comment - with hyphen --></p>

```

Example 624

```html
<p>foo <?php echo $a; ?></p>

```

Example 625

```html
<p>foo <!ELEMENT br EMPTY></p>

```

Example 626

```html
<p>foo <![CDATA[>&<]]></p>

```

Example 627

```html
<p>foo <a href="&ouml;"></p>

```

Example 628

```html
<p>foo <a href="\*"></p>

```

Example 629

```html
<p>&lt;a href=&quot;&quot;&quot;&gt;</p>

```

### Images

Example 568

```html
<p><img src="/url" alt="foo" title="title" /></p>

```

Example 569

```html
<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>

```

Example 570

```html
<p><img src="/url2" alt="foo bar" /></p>

```

Example 571

```html
<p><img src="/url2" alt="foo bar" /></p>

```

Example 572

```html
<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>

```

Example 573

```html
<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>

```

Example 574

```html
<p><img src="train.jpg" alt="foo" /></p>

```

Example 575

```html
<p>My <img src="/path/to/train.jpg" alt="foo bar" title="title" /></p>

```

Example 576

```html
<p><img src="url" alt="foo" /></p>

```

Example 577

```html
<p><img src="/url" alt="" /></p>

```

Example 578

```html
<p><img src="/url" alt="foo" /></p>

```

Example 579

```html
<p><img src="/url" alt="foo" /></p>

```

Example 580

```html
<p><img src="/url" alt="foo" title="title" /></p>

```

Example 581

```html
<p><img src="/url" alt="foo bar" title="title" /></p>

```

Example 582

```html
<p><img src="/url" alt="Foo" title="title" /></p>

```

Example 583

```html
<p><img src="/url" alt="foo" title="title" />
[]</p>

```

Example 584

```html
<p><img src="/url" alt="foo" title="title" /></p>

```

Example 585

```html
<p><img src="/url" alt="foo bar" title="title" /></p>

```

Example 586

```html
<p>![[foo]]</p>
<p>[[foo]]: /url &quot;title&quot;</p>

```

Example 587

```html
<p><img src="/url" alt="Foo" title="title" /></p>

```

Example 588

```html
<p>![foo]</p>

```

Example 589

```html
<p>!<a href="/url" title="title">foo</a></p>

```

### Link reference definitions

Example 161

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 162

```html
<p><a href="/url" title="the title">foo</a></p>

```

Example 163

```html
<p><a href="my_(url)" title="title (with parens)">Foo*bar]</a></p>

```

Example 164

```html
<p><a href="my%20url" title="title">Foo bar</a></p>

```

Example 165

```html
<p><a href="/url" title="
title
line1
line2
">foo</a></p>

```

Example 166

```html
<p>[foo]: /url 'title</p>
<p>with blank line'</p>
<p>[foo]</p>

```

Example 167

```html
<p><a href="/url">foo</a></p>

```

Example 168

```html
<p>[foo]:</p>
<p>[foo]</p>

```

Example 169

```html
<p><a href="">foo</a></p>

```

Example 170

```html
<p>[foo]: <bar>(baz)</p>
<p>[foo]</p>

```

Example 171

```html
<p><a href="/url%5Cbar*baz" title="foo&quot;bar\baz">foo</a></p>

```

Example 172

```html
<p><a href="url">foo</a></p>

```

Example 173

```html
<p><a href="first">foo</a></p>

```

Example 174

```html
<p><a href="/url">Foo</a></p>

```

Example 175

```html
<p><a href="/%CF%86%CE%BF%CF%85">αγω</a></p>

```

Example 176

```html

```

Example 177

```html
<p>bar</p>

```

Example 178

```html
<p>[foo]: /url &quot;title&quot; ok</p>

```

Example 179

```html
<p>&quot;title&quot; ok</p>

```

Example 180

```html
<pre><code>[foo]: /url &quot;title&quot;
</code></pre>
<p>[foo]</p>

```

Example 181

```html
<pre><code>[foo]: /url
</code></pre>
<p>[foo]</p>

```

Example 182

```html
<p>Foo
[bar]: /baz</p>
<p>[bar]</p>

```

Example 183

```html
<h1><a href="/url">Foo</a></h1>
<blockquote>
<p>bar</p>
</blockquote>

```

Example 184

```html
<h1>bar</h1>
<p><a href="/url">foo</a></p>

```

Example 185

```html
<p>===
<a href="/url">foo</a></p>

```

Example 186

```html
<p><a href="/foo-url" title="foo">foo</a>,
<a href="/bar-url" title="bar">bar</a>,
<a href="/baz-url">baz</a></p>

```

Example 187

```html
<p><a href="/url">foo</a></p>
<blockquote>
</blockquote>

```

Example 188

```html

```

### Paragraphs

Example 190

```html
<p>aaa
bbb</p>
<p>ccc
ddd</p>

```

Example 192

```html
<p>aaa
bbb</p>

```

Example 193

```html
<p>aaa
bbb
ccc</p>

```

Example 194

```html
<p>aaa
bbb</p>

```

Example 196

```html
<p>aaa<br />
bbb</p>

```

### Precedence

Example 12

```html
<ul>
<li>`one</li>
<li>two`</li>
</ul>

```

### Hard line breaks

Example 630

```html
<p>foo<br />
baz</p>

```

Example 631

```html
<p>foo<br />
baz</p>

```

Example 632

```html
<p>foo<br />
baz</p>

```

Example 633

```html
<p>foo<br />
bar</p>

```

Example 634

```html
<p>foo<br />
bar</p>

```

Example 635

```html
<p><em>foo<br />
bar</em></p>

```

Example 636

```html
<p><em>foo<br />
bar</em></p>

```

Example 637

```html
<p><code>code  span</code></p>

```

Example 638

```html
<p><code>code\ span</code></p>

```

Example 639

```html
<p><a href="foo  
bar"></p>

```

Example 640

```html
<p><a href="foo\
bar"></p>

```

### Tabs

Example 2

```html
<pre><code>foo	baz		bim
</code></pre>

```

Example 3

```html
<pre><code>a	a
ὐ	a
</code></pre>

```

Example 4

```html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

```

Example 5

```html
<ul>
<li>
<p>foo</p>
<pre><code>  bar
</code></pre>
</li>
</ul>

```

Example 6

```html
<blockquote>
<pre><code>  foo
</code></pre>
</blockquote>

```

Example 7

```html
<ul>
<li>
<pre><code>  foo
</code></pre>
</li>
</ul>

```

Example 8

```html
<pre><code>foo
bar
</code></pre>

```

Example 9

```html
<ul>
<li>foo
<ul>
<li>bar
<ul>
<li>baz</li>
</ul>
</li>
</ul>
</li>
</ul>

```

Example 11

```html
<hr />

```


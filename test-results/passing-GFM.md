## GFM

### Thematic breaks

Example 13

This markdown:


```markdown
***
---
___

```

Gives this correct output:


```html
<hr />
<hr />
<hr />

```

Example 14

This markdown:


```markdown
+++

```

Gives this correct output:


```html
<p>+++</p>

```

Example 15

This markdown:


```markdown
===

```

Gives this correct output:


```html
<p>===</p>

```

Example 17

This markdown:


```markdown
 ***
  ***
   ***

```

Gives this correct output:


```html
<hr />
<hr />
<hr />

```

Example 18

This markdown:


```markdown
    ***

```

Gives this correct output:


```html
<pre><code>***
</code></pre>

```

Example 20

This markdown:


```markdown
_____________________________________

```

Gives this correct output:


```html
<hr />

```

Example 28

This markdown:


```markdown
Foo
***
bar

```

Gives this correct output:


```html
<p>Foo</p>
<hr />
<p>bar</p>

```

### ATX headings

Example 32

This markdown:


```markdown
# foo
## foo
### foo
#### foo
##### foo
###### foo

```

Gives this correct output:


```html
<h1>foo</h1>
<h2>foo</h2>
<h3>foo</h3>
<h4>foo</h4>
<h5>foo</h5>
<h6>foo</h6>

```

Example 37

This markdown:


```markdown
#                  foo                     

```

Gives this correct output:


```html
<h1>foo</h1>

```

Example 39

This markdown:


```markdown
    # foo

```

Gives this correct output:


```html
<pre><code># foo
</code></pre>

```

Example 42

This markdown:


```markdown
# foo ##################################
##### foo ##

```

Gives this correct output:


```html
<h1>foo</h1>
<h5>foo</h5>

```

Example 44

This markdown:


```markdown
### foo ### b

```

Gives this correct output:


```html
<h3>foo ### b</h3>

```

Example 47

This markdown:


```markdown
****
## foo
****

```

Gives this correct output:


```html
<hr />
<h2>foo</h2>
<hr />

```

Example 48

This markdown:


```markdown
Foo bar
# baz
Bar foo

```

Gives this correct output:


```html
<p>Foo bar</p>
<h1>baz</h1>
<p>Bar foo</p>

```

Example 49

This markdown:


```markdown
## 
#
### ###

```

Gives this correct output:


```html
<h2></h2>
<h1></h1>
<h3></h3>

```

### Emphasis and strong emphasis

Example 350

This markdown:


```markdown
*foo bar*

```

Gives this correct output:


```html
<p><em>foo bar</em></p>

```

Example 354

This markdown:


```markdown
foo*bar*

```

Gives this correct output:


```html
<p>foo<em>bar</em></p>

```

Example 355

This markdown:


```markdown
5*6*78

```

Gives this correct output:


```html
<p>5<em>6</em>78</p>

```

Example 357

This markdown:


```markdown
_ foo bar_

```

Gives this correct output:


```html
<p>_ foo bar_</p>

```

Example 358

This markdown:


```markdown
a_"foo"_

```

Gives this correct output:


```html
<p>a_&quot;foo&quot;_</p>

```

Example 359

This markdown:


```markdown
foo_bar_

```

Gives this correct output:


```html
<p>foo_bar_</p>

```

Example 360

This markdown:


```markdown
5_6_78

```

Gives this correct output:


```html
<p>5_6_78</p>

```

Example 361

This markdown:


```markdown
пристаням_стремятся_

```

Gives this correct output:


```html
<p>пристаням_стремятся_</p>

```

Example 362

This markdown:


```markdown
aa_"bb"_cc

```

Gives this correct output:


```html
<p>aa_&quot;bb&quot;_cc</p>

```

Example 369

This markdown:


```markdown
*foo*bar

```

Gives this correct output:


```html
<p><em>foo</em>bar</p>

```

Example 370

This markdown:


```markdown
_foo bar _

```

Gives this correct output:


```html
<p>_foo bar _</p>

```

Example 371

This markdown:


```markdown
_(_foo)

```

Gives this correct output:


```html
<p>_(_foo)</p>

```

Example 373

This markdown:


```markdown
_foo_bar

```

Gives this correct output:


```html
<p>_foo_bar</p>

```

Example 374

This markdown:


```markdown
_пристаням_стремятся

```

Gives this correct output:


```html
<p>_пристаням_стремятся</p>

```

Example 377

This markdown:


```markdown
**foo bar**

```

Gives this correct output:


```html
<p><strong>foo bar</strong></p>

```

Example 380

This markdown:


```markdown
foo**bar**

```

Gives this correct output:


```html
<p>foo<strong>bar</strong></p>

```

Example 382

This markdown:


```markdown
__ foo bar__

```

Gives this correct output:


```html
<p>__ foo bar__</p>

```

Example 384

This markdown:


```markdown
a__"foo"__

```

Gives this correct output:


```html
<p>a__&quot;foo&quot;__</p>

```

Example 385

This markdown:


```markdown
foo__bar__

```

Gives this correct output:


```html
<p>foo__bar__</p>

```

Example 386

This markdown:


```markdown
5__6__78

```

Gives this correct output:


```html
<p>5__6__78</p>

```

Example 387

This markdown:


```markdown
пристаням__стремятся__

```

Gives this correct output:


```html
<p>пристаням__стремятся__</p>

```

Example 395

This markdown:


```markdown
**foo**bar

```

Gives this correct output:


```html
<p><strong>foo</strong>bar</p>

```

Example 396

This markdown:


```markdown
__foo bar __

```

Gives this correct output:


```html
<p>__foo bar __</p>

```

Example 397

This markdown:


```markdown
__(__foo)

```

Gives this correct output:


```html
<p>__(__foo)</p>

```

Example 399

This markdown:


```markdown
__foo__bar

```

Gives this correct output:


```html
<p>__foo__bar</p>

```

Example 400

This markdown:


```markdown
__пристаням__стремятся

```

Gives this correct output:


```html
<p>__пристаням__стремятся</p>

```

Example 433

This markdown:


```markdown
__ is not an empty emphasis

```

Gives this correct output:


```html
<p>__ is not an empty emphasis</p>

```

Example 437

This markdown:


```markdown
foo *_*

```

Gives this correct output:


```html
<p>foo <em>_</em></p>

```

Example 440

This markdown:


```markdown
foo **_**

```

Gives this correct output:


```html
<p>foo <strong>_</strong></p>

```

Example 447

This markdown:


```markdown
foo ___

```

Gives this correct output:


```html
<p>foo ___</p>

```

Example 450

This markdown:


```markdown
foo _____

```

Gives this correct output:


```html
<p>foo _____</p>

```

Example 459

This markdown:


```markdown
**foo**

```

Gives this correct output:


```html
<p><strong>foo</strong></p>

```

Example 468

This markdown:


```markdown
*foo _bar* baz_

```

Gives this correct output:


```html
<p><em>foo _bar</em> baz_</p>

```

Example 473

This markdown:


```markdown
_foo [bar_](/url)

```

Gives this correct output:


```html
<p>_foo <a href="/url">bar_</a></p>

```

### Backslash escapes

Example 299

This markdown:


```markdown
\	\A\a\ \3\φ\«

```

Gives this correct output:


```html
<p>\	\A\a\ \3\φ\«</p>

```

Example 304

This markdown:


```markdown
    \[\]

```

Gives this correct output:


```html
<pre><code>\[\]
</code></pre>

```

Example 305

This markdown:


```markdown
~~~
\[\]
~~~

```

Gives this correct output:


```html
<pre><code>\[\]
</code></pre>

```

### List items

Example 236

This markdown:


```markdown
1234567890. not ok

```

Gives this correct output:


```html
<p>1234567890. not ok</p>

```

Example 242

This markdown:


```markdown
    indented code

paragraph

    more code

```

Gives this correct output:


```html
<pre><code>indented code
</code></pre>
<p>paragraph</p>
<pre><code>more code
</code></pre>

```

### [extension] Strikethrough

Example 492

This markdown:


```markdown
This ~~has a

new paragraph~~.
```

Gives this correct output:


```html
<p>This ~~has a</p>
<p>new paragraph~~.</p>
```

### Links

Example 482

This markdown:


```markdown
[link](/uri)

```

Gives this correct output:


```html
<p><a href="/uri">link</a></p>

```

Example 483

This markdown:


```markdown
[link]()

```

Gives this correct output:


```html
<p><a href="">link</a></p>

```

Example 497

This markdown:


```markdown
[link](#fragment)

[link](http://example.com#fragment)

[link](http://example.com?foo=3#frag)

```

Gives this correct output:


```html
<p><a href="#fragment">link</a></p>
<p><a href="http://example.com#fragment">link</a></p>
<p><a href="http://example.com?foo=3#frag">link</a></p>

```

### Raw HTML

Example 622

This markdown:


```markdown
foo <!-- not a comment -- two hyphens -->

```

Gives this correct output:


```html
<p>foo &lt;!-- not a comment -- two hyphens --&gt;</p>

```

Example 623

This markdown:


```markdown
foo <!--> foo -->

foo <!-- foo--->

```

Gives this correct output:


```html
<p>foo &lt;!--&gt; foo --&gt;</p>
<p>foo &lt;!-- foo---&gt;</p>

```

### Autolinks

Example 607

This markdown:


```markdown
http://example.com

```

Gives this correct output:


```html
<p>http://example.com</p>

```

Example 608

This markdown:


```markdown
foo@bar.example.com

```

Gives this correct output:


```html
<p>foo@bar.example.com</p>

```

### Setext headings

Example 67

This markdown:


```markdown

====

```

Gives this correct output:


```html
<p>====</p>

```

Example 68

This markdown:


```markdown
---
---

```

Gives this correct output:


```html
<hr />
<hr />

```

Example 70

This markdown:


```markdown
    foo
---

```

Gives this correct output:


```html
<pre><code>foo
</code></pre>
<hr />

```

### Paragraphs

Example 189

This markdown:


```markdown
aaa

bbb

```

Gives this correct output:


```html
<p>aaa</p>
<p>bbb</p>

```

Example 191

This markdown:


```markdown
aaa


bbb

```

Gives this correct output:


```html
<p>aaa</p>
<p>bbb</p>

```

Example 195

This markdown:


```markdown
    aaa
bbb

```

Gives this correct output:


```html
<pre><code>aaa
</code></pre>
<p>bbb</p>

```

### Indented code blocks

Example 84

This markdown:


```markdown
    foo
bar

```

Gives this correct output:


```html
<pre><code>foo
</code></pre>
<p>bar</p>

```

Example 88

This markdown:


```markdown
    foo  

```

Gives this correct output:


```html
<pre><code>foo  
</code></pre>

```

### Hard line breaks

Example 641

This markdown:


```markdown
foo\

```

Gives this correct output:


```html
<p>foo\</p>

```

Example 642

This markdown:


```markdown
foo  

```

Gives this correct output:


```html
<p>foo</p>

```

Example 643

This markdown:


```markdown
### foo\

```

Gives this correct output:


```html
<h3>foo\</h3>

```

Example 644

This markdown:


```markdown
### foo  

```

Gives this correct output:


```html
<h3>foo</h3>

```

### Fenced code blocks

Example 89

This markdown:


```markdown
```
<
 >
```

```

Gives this correct output:


```html
<pre><code>&lt;
 &gt;
</code></pre>

```

Example 90

This markdown:


```markdown
~~~
<
 >
~~~

```

Gives this correct output:


```html
<pre><code>&lt;
 &gt;
</code></pre>

```

Example 92

This markdown:


```markdown
```
aaa
~~~
```

```

Gives this correct output:


```html
<pre><code>aaa
~~~
</code></pre>

```

Example 93

This markdown:


```markdown
~~~
aaa
```
~~~

```

Gives this correct output:


```html
<pre><code>aaa
```
</code></pre>

```

Example 99

This markdown:


```markdown
```

  
```

```

Gives this correct output:


```html
<pre><code>
  
</code></pre>

```

Example 110

This markdown:


```markdown
foo
```
bar
```
baz

```

Gives this correct output:


```html
<p>foo</p>
<pre><code>bar
</code></pre>
<p>baz</p>

```

Example 117

This markdown:


```markdown
```
``` aaa
```

```

Gives this correct output:


```html
<pre><code>``` aaa
</code></pre>

```

### Textual content

Example 647

This markdown:


```markdown
hello $.;'there

```

Gives this correct output:


```html
<p>hello $.;'there</p>

```

Example 648

This markdown:


```markdown
Foo χρῆν

```

Gives this correct output:


```html
<p>Foo χρῆν</p>

```

Example 649

This markdown:


```markdown
Multiple     spaces

```

Gives this correct output:


```html
<p>Multiple     spaces</p>

```

### Tabs

Example 1

This markdown:


```markdown
	foo	baz		bim

```

Gives this correct output:


```html
<pre><code>foo	baz		bim
</code></pre>

```

Example 10

This markdown:


```markdown
#	Foo

```

Gives this correct output:


```html
<h1>Foo</h1>

```

### Entity and numeric character references

Example 315

This markdown:


```markdown
&copy

```

Gives this correct output:


```html
<p>&amp;copy</p>

```

Example 316

This markdown:


```markdown
&MadeUpEntity;

```

Gives this correct output:


```html
<p>&amp;MadeUpEntity;</p>

```

Example 321

This markdown:


```markdown
`f&ouml;&ouml;`

```

Gives this correct output:


```html
<p><code>f&amp;ouml;&amp;ouml;</code></p>

```

Example 322

This markdown:


```markdown
    f&ouml;f&ouml;

```

Gives this correct output:


```html
<pre><code>f&amp;ouml;f&amp;ouml;
</code></pre>

```

### Code spans

Example 328

This markdown:


```markdown
`foo`

```

Gives this correct output:


```html
<p><code>foo</code></p>

```

Example 332

This markdown:


```markdown
` a`

```

Gives this correct output:


```html
<p><code> a</code></p>

```

Example 333

This markdown:


```markdown
` b `

```

Gives this correct output:


```html
<p><code> b </code></p>

```


# GFM

## Paragraphs

### [Example 189](https://github.github.com/gfm/#example-189)

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

### [Example 190](https://github.github.com/gfm/#example-190)

This markdown:


```markdown
aaa
bbb

ccc
ddd

```

Gives this correct output:


```html
<p>aaa
bbb</p>
<p>ccc
ddd</p>

```

### [Example 191](https://github.github.com/gfm/#example-191)

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

### [Example 195](https://github.github.com/gfm/#example-195)

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

## [extension] Tables

### [Example 203](https://github.github.com/gfm/#example-203)

This markdown:


```markdown
| abc | def |
| --- |
| bar |
```

Gives this correct output:


```html
<p>| abc | def |
| --- |
| bar |</p>
```

## Fenced code blocks

### [Example 89](https://github.github.com/gfm/#example-89)

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

### [Example 90](https://github.github.com/gfm/#example-90)

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

### [Example 92](https://github.github.com/gfm/#example-92)

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

### [Example 93](https://github.github.com/gfm/#example-93)

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

### [Example 99](https://github.github.com/gfm/#example-99)

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

### [Example 110](https://github.github.com/gfm/#example-110)

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

### [Example 117](https://github.github.com/gfm/#example-117)

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

## Indented code blocks

### [Example 84](https://github.github.com/gfm/#example-84)

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

### [Example 88](https://github.github.com/gfm/#example-88)

This markdown:


```markdown
    foo  

```

Gives this correct output:


```html
<pre><code>foo  
</code></pre>

```

## Setext headings

### [Example 67](https://github.github.com/gfm/#example-67)

This markdown:


```markdown

====

```

Gives this correct output:


```html
<p>====</p>

```

### [Example 68](https://github.github.com/gfm/#example-68)

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

### [Example 70](https://github.github.com/gfm/#example-70)

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

### [Example 74](https://github.github.com/gfm/#example-74)

This markdown:


```markdown
Foo
bar

---

baz

```

Gives this correct output:


```html
<p>Foo
bar</p>
<hr />
<p>baz</p>

```

## Entity and numeric character references

### [Example 314](https://github.github.com/gfm/#example-314)

This markdown:


```markdown
&nbsp &x; &#; &#x;
&#987654321;
&#abcdef0;
&ThisIsNotDefined; &hi?;

```

Gives this correct output:


```html
<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;
&amp;#987654321;
&amp;#abcdef0;
&amp;ThisIsNotDefined; &amp;hi?;</p>

```

### [Example 315](https://github.github.com/gfm/#example-315)

This markdown:


```markdown
&copy

```

Gives this correct output:


```html
<p>&amp;copy</p>

```

### [Example 316](https://github.github.com/gfm/#example-316)

This markdown:


```markdown
&MadeUpEntity;

```

Gives this correct output:


```html
<p>&amp;MadeUpEntity;</p>

```

### [Example 321](https://github.github.com/gfm/#example-321)

This markdown:


```markdown
`f&ouml;&ouml;`

```

Gives this correct output:


```html
<p><code>f&amp;ouml;&amp;ouml;</code></p>

```

### [Example 322](https://github.github.com/gfm/#example-322)

This markdown:


```markdown
    f&ouml;f&ouml;

```

Gives this correct output:


```html
<pre><code>f&amp;ouml;f&amp;ouml;
</code></pre>

```

## Hard line breaks

### [Example 637](https://github.github.com/gfm/#example-637)

This markdown:


```markdown
`code 
span`

```

Gives this correct output:


```html
<p><code>code  span</code></p>

```

### [Example 638](https://github.github.com/gfm/#example-638)

This markdown:


```markdown
`code\
span`

```

Gives this correct output:


```html
<p><code>code\ span</code></p>

```

### [Example 641](https://github.github.com/gfm/#example-641)

This markdown:


```markdown
foo\

```

Gives this correct output:


```html
<p>foo\</p>

```

### [Example 642](https://github.github.com/gfm/#example-642)

This markdown:


```markdown
foo  

```

Gives this correct output:


```html
<p>foo</p>

```

### [Example 643](https://github.github.com/gfm/#example-643)

This markdown:


```markdown
### foo\

```

Gives this correct output:


```html
<h3>foo\</h3>

```

### [Example 644](https://github.github.com/gfm/#example-644)

This markdown:


```markdown
### foo  

```

Gives this correct output:


```html
<h3>foo</h3>

```

## Thematic breaks

### [Example 13](https://github.github.com/gfm/#example-13)

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

### [Example 15](https://github.github.com/gfm/#example-15)

This markdown:


```markdown
===

```

Gives this correct output:


```html
<p>===</p>

```

### [Example 17](https://github.github.com/gfm/#example-17)

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

### [Example 18](https://github.github.com/gfm/#example-18)

This markdown:


```markdown
    ***

```

Gives this correct output:


```html
<pre><code>***
</code></pre>

```

### [Example 20](https://github.github.com/gfm/#example-20)

This markdown:


```markdown
_____________________________________

```

Gives this correct output:


```html
<hr />

```

### [Example 28](https://github.github.com/gfm/#example-28)

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

## ATX headings

### [Example 32](https://github.github.com/gfm/#example-32)

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

### [Example 37](https://github.github.com/gfm/#example-37)

This markdown:


```markdown
#                  foo                     

```

Gives this correct output:


```html
<h1>foo</h1>

```

### [Example 39](https://github.github.com/gfm/#example-39)

This markdown:


```markdown
    # foo

```

Gives this correct output:


```html
<pre><code># foo
</code></pre>

```

### [Example 42](https://github.github.com/gfm/#example-42)

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

### [Example 44](https://github.github.com/gfm/#example-44)

This markdown:


```markdown
### foo ### b

```

Gives this correct output:


```html
<h3>foo ### b</h3>

```

### [Example 47](https://github.github.com/gfm/#example-47)

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

### [Example 48](https://github.github.com/gfm/#example-48)

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

### [Example 49](https://github.github.com/gfm/#example-49)

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

## Textual content

### [Example 647](https://github.github.com/gfm/#example-647)

This markdown:


```markdown
hello $.;'there

```

Gives this correct output:


```html
<p>hello $.;'there</p>

```

### [Example 648](https://github.github.com/gfm/#example-648)

This markdown:


```markdown
Foo χρῆν

```

Gives this correct output:


```html
<p>Foo χρῆν</p>

```

### [Example 649](https://github.github.com/gfm/#example-649)

This markdown:


```markdown
Multiple     spaces

```

Gives this correct output:


```html
<p>Multiple     spaces</p>

```

## Tabs

### [Example 1](https://github.github.com/gfm/#example-1)

This markdown:


```markdown
	foo	baz		bim

```

Gives this correct output:


```html
<pre><code>foo	baz		bim
</code></pre>

```

### [Example 10](https://github.github.com/gfm/#example-10)

This markdown:


```markdown
#	Foo

```

Gives this correct output:


```html
<h1>Foo</h1>

```

## Backslash escapes

### [Example 299](https://github.github.com/gfm/#example-299)

This markdown:


```markdown
\	\A\a\ \3\φ\«

```

Gives this correct output:


```html
<p>\	\A\a\ \3\φ\«</p>

```

### [Example 304](https://github.github.com/gfm/#example-304)

This markdown:


```markdown
    \[\]

```

Gives this correct output:


```html
<pre><code>\[\]
</code></pre>

```

### [Example 305](https://github.github.com/gfm/#example-305)

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

## Code spans

### [Example 328](https://github.github.com/gfm/#example-328)

This markdown:


```markdown
`foo`

```

Gives this correct output:


```html
<p><code>foo</code></p>

```

### [Example 332](https://github.github.com/gfm/#example-332)

This markdown:


```markdown
` a`

```

Gives this correct output:


```html
<p><code> a</code></p>

```

### [Example 333](https://github.github.com/gfm/#example-333)

This markdown:


```markdown
` b `

```

Gives this correct output:


```html
<p><code> b </code></p>

```

### [Example 334](https://github.github.com/gfm/#example-334)

This markdown:


```markdown
` `
`  `

```

Gives this correct output:


```html
<p><code> </code>
<code>  </code></p>

```

### [Example 337](https://github.github.com/gfm/#example-337)

This markdown:


```markdown
`foo   bar 
baz`

```

Gives this correct output:


```html
<p><code>foo   bar  baz</code></p>

```

## Soft line breaks

### [Example 645](https://github.github.com/gfm/#example-645)

This markdown:


```markdown
foo
baz

```

Gives this correct output:


```html
<p>foo
baz</p>

```

## Emphasis and strong emphasis

### [Example 354](https://github.github.com/gfm/#example-354)

This markdown:


```markdown
foo*bar*

```

Gives this correct output:


```html
<p>foo<em>bar</em></p>

```

### [Example 355](https://github.github.com/gfm/#example-355)

This markdown:


```markdown
5*6*78

```

Gives this correct output:


```html
<p>5<em>6</em>78</p>

```

### [Example 357](https://github.github.com/gfm/#example-357)

This markdown:


```markdown
_ foo bar_

```

Gives this correct output:


```html
<p>_ foo bar_</p>

```

### [Example 358](https://github.github.com/gfm/#example-358)

This markdown:


```markdown
a_"foo"_

```

Gives this correct output:


```html
<p>a_&quot;foo&quot;_</p>

```

### [Example 359](https://github.github.com/gfm/#example-359)

This markdown:


```markdown
foo_bar_

```

Gives this correct output:


```html
<p>foo_bar_</p>

```

### [Example 360](https://github.github.com/gfm/#example-360)

This markdown:


```markdown
5_6_78

```

Gives this correct output:


```html
<p>5_6_78</p>

```

### [Example 361](https://github.github.com/gfm/#example-361)

This markdown:


```markdown
пристаням_стремятся_

```

Gives this correct output:


```html
<p>пристаням_стремятся_</p>

```

### [Example 362](https://github.github.com/gfm/#example-362)

This markdown:


```markdown
aa_"bb"_cc

```

Gives this correct output:


```html
<p>aa_&quot;bb&quot;_cc</p>

```

### [Example 370](https://github.github.com/gfm/#example-370)

This markdown:


```markdown
_foo bar _

```

Gives this correct output:


```html
<p>_foo bar _</p>

```

### [Example 371](https://github.github.com/gfm/#example-371)

This markdown:


```markdown
_(_foo)

```

Gives this correct output:


```html
<p>_(_foo)</p>

```

### [Example 373](https://github.github.com/gfm/#example-373)

This markdown:


```markdown
_foo_bar

```

Gives this correct output:


```html
<p>_foo_bar</p>

```

### [Example 374](https://github.github.com/gfm/#example-374)

This markdown:


```markdown
_пристаням_стремятся

```

Gives this correct output:


```html
<p>_пристаням_стремятся</p>

```

### [Example 380](https://github.github.com/gfm/#example-380)

This markdown:


```markdown
foo**bar**

```

Gives this correct output:


```html
<p>foo<strong>bar</strong></p>

```

### [Example 382](https://github.github.com/gfm/#example-382)

This markdown:


```markdown
__ foo bar__

```

Gives this correct output:


```html
<p>__ foo bar__</p>

```

### [Example 383](https://github.github.com/gfm/#example-383)

This markdown:


```markdown
__
foo bar__

```

Gives this correct output:


```html
<p>__
foo bar__</p>

```

### [Example 384](https://github.github.com/gfm/#example-384)

This markdown:


```markdown
a__"foo"__

```

Gives this correct output:


```html
<p>a__&quot;foo&quot;__</p>

```

### [Example 385](https://github.github.com/gfm/#example-385)

This markdown:


```markdown
foo__bar__

```

Gives this correct output:


```html
<p>foo__bar__</p>

```

### [Example 386](https://github.github.com/gfm/#example-386)

This markdown:


```markdown
5__6__78

```

Gives this correct output:


```html
<p>5__6__78</p>

```

### [Example 387](https://github.github.com/gfm/#example-387)

This markdown:


```markdown
пристаням__стремятся__

```

Gives this correct output:


```html
<p>пристаням__стремятся__</p>

```

### [Example 396](https://github.github.com/gfm/#example-396)

This markdown:


```markdown
__foo bar __

```

Gives this correct output:


```html
<p>__foo bar __</p>

```

### [Example 397](https://github.github.com/gfm/#example-397)

This markdown:


```markdown
__(__foo)

```

Gives this correct output:


```html
<p>__(__foo)</p>

```

### [Example 399](https://github.github.com/gfm/#example-399)

This markdown:


```markdown
__foo__bar

```

Gives this correct output:


```html
<p>__foo__bar</p>

```

### [Example 400](https://github.github.com/gfm/#example-400)

This markdown:


```markdown
__пристаням__стремятся

```

Gives this correct output:


```html
<p>__пристаням__стремятся</p>

```

### [Example 433](https://github.github.com/gfm/#example-433)

This markdown:


```markdown
__ is not an empty emphasis

```

Gives this correct output:


```html
<p>__ is not an empty emphasis</p>

```

### [Example 437](https://github.github.com/gfm/#example-437)

This markdown:


```markdown
foo *_*

```

Gives this correct output:


```html
<p>foo <em>_</em></p>

```

### [Example 440](https://github.github.com/gfm/#example-440)

This markdown:


```markdown
foo **_**

```

Gives this correct output:


```html
<p>foo <strong>_</strong></p>

```

### [Example 447](https://github.github.com/gfm/#example-447)

This markdown:


```markdown
foo ___

```

Gives this correct output:


```html
<p>foo ___</p>

```

### [Example 450](https://github.github.com/gfm/#example-450)

This markdown:


```markdown
foo _____

```

Gives this correct output:


```html
<p>foo _____</p>

```

### [Example 473](https://github.github.com/gfm/#example-473)

This markdown:


```markdown
_foo [bar_](/url)

```

Gives this correct output:


```html
<p>_foo <a href="/url">bar_</a></p>

```

## Lists

### [Example 274](https://github.github.com/gfm/#example-274)

This markdown:


```markdown
The number of windows in my house is
14.  The number of doors is 6.

```

Gives this correct output:


```html
<p>The number of windows in my house is
14.  The number of doors is 6.</p>

```

## [extension] Strikethrough

### [Example 492](https://github.github.com/gfm/#example-492)

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

## Links

### [Example 482](https://github.github.com/gfm/#example-482)

This markdown:


```markdown
[link](/uri)

```

Gives this correct output:


```html
<p><a href="/uri">link</a></p>

```

### [Example 483](https://github.github.com/gfm/#example-483)

This markdown:


```markdown
[link]()

```

Gives this correct output:


```html
<p><a href="">link</a></p>

```

### [Example 484](https://github.github.com/gfm/#example-484)

This markdown:


```markdown
[link](<>)

```

Gives this correct output:


```html
<p><a href="">link</a></p>

```

### [Example 489](https://github.github.com/gfm/#example-489)

This markdown:


```markdown
[a](<b)c>)

```

Gives this correct output:


```html
<p><a href="b)c">a</a></p>

```

### [Example 495](https://github.github.com/gfm/#example-495)

This markdown:


```markdown
[link](<foo(and(bar)>)

```

Gives this correct output:


```html
<p><a href="foo(and(bar)">link</a></p>

```

### [Example 497](https://github.github.com/gfm/#example-497)

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

## Raw HTML

### [Example 622](https://github.github.com/gfm/#example-622)

This markdown:


```markdown
foo <!-- not a comment -- two hyphens -->

```

Gives this correct output:


```html
<p>foo &lt;!-- not a comment -- two hyphens --&gt;</p>

```

### [Example 623](https://github.github.com/gfm/#example-623)

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

## Autolinks

### [Example 607](https://github.github.com/gfm/#example-607)

This markdown:


```markdown
http://example.com

```

Gives this correct output:


```html
<p>http://example.com</p>

```

### [Example 608](https://github.github.com/gfm/#example-608)

This markdown:


```markdown
foo@bar.example.com

```

Gives this correct output:


```html
<p>foo@bar.example.com</p>

```

## List items

### [Example 236](https://github.github.com/gfm/#example-236)

This markdown:


```markdown
1234567890. not ok

```

Gives this correct output:


```html
<p>1234567890. not ok</p>

```

### [Example 242](https://github.github.com/gfm/#example-242)

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


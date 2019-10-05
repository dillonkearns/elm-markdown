# CommonMark

## Paragraphs

### [Example 189](https://spec.commonmark.org/0.29/#example-189)

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

### [Example 190](https://spec.commonmark.org/0.29/#example-190)

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

### [Example 191](https://spec.commonmark.org/0.29/#example-191)

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

### [Example 195](https://spec.commonmark.org/0.29/#example-195)

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

## List items

### [Example 236](https://spec.commonmark.org/0.29/#example-236)

This markdown:


```markdown
1234567890. not ok

```

Gives this correct output:


```html
<p>1234567890. not ok</p>

```

### [Example 242](https://spec.commonmark.org/0.29/#example-242)

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

## Hard line breaks

### [Example 637](https://spec.commonmark.org/0.29/#example-637)

This markdown:


```markdown
`code 
span`

```

Gives this correct output:


```html
<p><code>code  span</code></p>

```

### [Example 638](https://spec.commonmark.org/0.29/#example-638)

This markdown:


```markdown
`code\
span`

```

Gives this correct output:


```html
<p><code>code\ span</code></p>

```

### [Example 641](https://spec.commonmark.org/0.29/#example-641)

This markdown:


```markdown
foo\

```

Gives this correct output:


```html
<p>foo\</p>

```

### [Example 642](https://spec.commonmark.org/0.29/#example-642)

This markdown:


```markdown
foo  

```

Gives this correct output:


```html
<p>foo</p>

```

### [Example 643](https://spec.commonmark.org/0.29/#example-643)

This markdown:


```markdown
### foo\

```

Gives this correct output:


```html
<h3>foo\</h3>

```

### [Example 644](https://spec.commonmark.org/0.29/#example-644)

This markdown:


```markdown
### foo  

```

Gives this correct output:


```html
<h3>foo</h3>

```

## Links

### [Example 482](https://spec.commonmark.org/0.29/#example-482)

This markdown:


```markdown
[link](/uri)

```

Gives this correct output:


```html
<p><a href="/uri">link</a></p>

```

### [Example 483](https://spec.commonmark.org/0.29/#example-483)

This markdown:


```markdown
[link]()

```

Gives this correct output:


```html
<p><a href="">link</a></p>

```

### [Example 497](https://spec.commonmark.org/0.29/#example-497)

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

## ATX headings

### [Example 32](https://spec.commonmark.org/0.29/#example-32)

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

### [Example 37](https://spec.commonmark.org/0.29/#example-37)

This markdown:


```markdown
#                  foo                     

```

Gives this correct output:


```html
<h1>foo</h1>

```

### [Example 39](https://spec.commonmark.org/0.29/#example-39)

This markdown:


```markdown
    # foo

```

Gives this correct output:


```html
<pre><code># foo
</code></pre>

```

### [Example 42](https://spec.commonmark.org/0.29/#example-42)

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

### [Example 44](https://spec.commonmark.org/0.29/#example-44)

This markdown:


```markdown
### foo ### b

```

Gives this correct output:


```html
<h3>foo ### b</h3>

```

### [Example 47](https://spec.commonmark.org/0.29/#example-47)

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

### [Example 48](https://spec.commonmark.org/0.29/#example-48)

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

### [Example 49](https://spec.commonmark.org/0.29/#example-49)

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

## Soft line breaks

### [Example 645](https://spec.commonmark.org/0.29/#example-645)

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

## Lists

### [Example 274](https://spec.commonmark.org/0.29/#example-274)

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

## Fenced code blocks

### [Example 89](https://spec.commonmark.org/0.29/#example-89)

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

### [Example 90](https://spec.commonmark.org/0.29/#example-90)

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

### [Example 92](https://spec.commonmark.org/0.29/#example-92)

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

### [Example 93](https://spec.commonmark.org/0.29/#example-93)

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

### [Example 99](https://spec.commonmark.org/0.29/#example-99)

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

### [Example 110](https://spec.commonmark.org/0.29/#example-110)

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

### [Example 117](https://spec.commonmark.org/0.29/#example-117)

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

## Textual content

### [Example 647](https://spec.commonmark.org/0.29/#example-647)

This markdown:


```markdown
hello $.;'there

```

Gives this correct output:


```html
<p>hello $.;'there</p>

```

### [Example 648](https://spec.commonmark.org/0.29/#example-648)

This markdown:


```markdown
Foo χρῆν

```

Gives this correct output:


```html
<p>Foo χρῆν</p>

```

### [Example 649](https://spec.commonmark.org/0.29/#example-649)

This markdown:


```markdown
Multiple     spaces

```

Gives this correct output:


```html
<p>Multiple     spaces</p>

```

## Backslash escapes

### [Example 299](https://spec.commonmark.org/0.29/#example-299)

This markdown:


```markdown
\	\A\a\ \3\φ\«

```

Gives this correct output:


```html
<p>\	\A\a\ \3\φ\«</p>

```

### [Example 304](https://spec.commonmark.org/0.29/#example-304)

This markdown:


```markdown
    \[\]

```

Gives this correct output:


```html
<pre><code>\[\]
</code></pre>

```

### [Example 305](https://spec.commonmark.org/0.29/#example-305)

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

## Autolinks

### [Example 607](https://spec.commonmark.org/0.29/#example-607)

This markdown:


```markdown
http://example.com

```

Gives this correct output:


```html
<p>http://example.com</p>

```

### [Example 608](https://spec.commonmark.org/0.29/#example-608)

This markdown:


```markdown
foo@bar.example.com

```

Gives this correct output:


```html
<p>foo@bar.example.com</p>

```

## Indented code blocks

### [Example 84](https://spec.commonmark.org/0.29/#example-84)

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

### [Example 88](https://spec.commonmark.org/0.29/#example-88)

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

### [Example 67](https://spec.commonmark.org/0.29/#example-67)

This markdown:


```markdown

====

```

Gives this correct output:


```html
<p>====</p>

```

### [Example 68](https://spec.commonmark.org/0.29/#example-68)

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

### [Example 70](https://spec.commonmark.org/0.29/#example-70)

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

### [Example 74](https://spec.commonmark.org/0.29/#example-74)

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

## Raw HTML

### [Example 622](https://spec.commonmark.org/0.29/#example-622)

This markdown:


```markdown
foo <!-- not a comment -- two hyphens -->

```

Gives this correct output:


```html
<p>foo &lt;!-- not a comment -- two hyphens --&gt;</p>

```

### [Example 623](https://spec.commonmark.org/0.29/#example-623)

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

## Entity and numeric character references

### [Example 314](https://spec.commonmark.org/0.29/#example-314)

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

### [Example 315](https://spec.commonmark.org/0.29/#example-315)

This markdown:


```markdown
&copy

```

Gives this correct output:


```html
<p>&amp;copy</p>

```

### [Example 316](https://spec.commonmark.org/0.29/#example-316)

This markdown:


```markdown
&MadeUpEntity;

```

Gives this correct output:


```html
<p>&amp;MadeUpEntity;</p>

```

### [Example 321](https://spec.commonmark.org/0.29/#example-321)

This markdown:


```markdown
`f&ouml;&ouml;`

```

Gives this correct output:


```html
<p><code>f&amp;ouml;&amp;ouml;</code></p>

```

### [Example 322](https://spec.commonmark.org/0.29/#example-322)

This markdown:


```markdown
    f&ouml;f&ouml;

```

Gives this correct output:


```html
<pre><code>f&amp;ouml;f&amp;ouml;
</code></pre>

```

## Code spans

### [Example 328](https://spec.commonmark.org/0.29/#example-328)

This markdown:


```markdown
`foo`

```

Gives this correct output:


```html
<p><code>foo</code></p>

```

### [Example 332](https://spec.commonmark.org/0.29/#example-332)

This markdown:


```markdown
` a`

```

Gives this correct output:


```html
<p><code> a</code></p>

```

### [Example 333](https://spec.commonmark.org/0.29/#example-333)

This markdown:


```markdown
` b `

```

Gives this correct output:


```html
<p><code> b </code></p>

```

### [Example 334](https://spec.commonmark.org/0.29/#example-334)

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

### [Example 337](https://spec.commonmark.org/0.29/#example-337)

This markdown:


```markdown
`foo   bar 
baz`

```

Gives this correct output:


```html
<p><code>foo   bar  baz</code></p>

```

## Tabs

### [Example 1](https://spec.commonmark.org/0.29/#example-1)

This markdown:


```markdown
	foo	baz		bim

```

Gives this correct output:


```html
<pre><code>foo	baz		bim
</code></pre>

```

### [Example 10](https://spec.commonmark.org/0.29/#example-10)

This markdown:


```markdown
#	Foo

```

Gives this correct output:


```html
<h1>Foo</h1>

```

## Emphasis and strong emphasis

### [Example 350](https://spec.commonmark.org/0.29/#example-350)

This markdown:


```markdown
*foo bar*

```

Gives this correct output:


```html
<p><em>foo bar</em></p>

```

### [Example 354](https://spec.commonmark.org/0.29/#example-354)

This markdown:


```markdown
foo*bar*

```

Gives this correct output:


```html
<p>foo<em>bar</em></p>

```

### [Example 355](https://spec.commonmark.org/0.29/#example-355)

This markdown:


```markdown
5*6*78

```

Gives this correct output:


```html
<p>5<em>6</em>78</p>

```

### [Example 357](https://spec.commonmark.org/0.29/#example-357)

This markdown:


```markdown
_ foo bar_

```

Gives this correct output:


```html
<p>_ foo bar_</p>

```

### [Example 358](https://spec.commonmark.org/0.29/#example-358)

This markdown:


```markdown
a_"foo"_

```

Gives this correct output:


```html
<p>a_&quot;foo&quot;_</p>

```

### [Example 359](https://spec.commonmark.org/0.29/#example-359)

This markdown:


```markdown
foo_bar_

```

Gives this correct output:


```html
<p>foo_bar_</p>

```

### [Example 360](https://spec.commonmark.org/0.29/#example-360)

This markdown:


```markdown
5_6_78

```

Gives this correct output:


```html
<p>5_6_78</p>

```

### [Example 361](https://spec.commonmark.org/0.29/#example-361)

This markdown:


```markdown
пристаням_стремятся_

```

Gives this correct output:


```html
<p>пристаням_стремятся_</p>

```

### [Example 362](https://spec.commonmark.org/0.29/#example-362)

This markdown:


```markdown
aa_"bb"_cc

```

Gives this correct output:


```html
<p>aa_&quot;bb&quot;_cc</p>

```

### [Example 369](https://spec.commonmark.org/0.29/#example-369)

This markdown:


```markdown
*foo*bar

```

Gives this correct output:


```html
<p><em>foo</em>bar</p>

```

### [Example 370](https://spec.commonmark.org/0.29/#example-370)

This markdown:


```markdown
_foo bar _

```

Gives this correct output:


```html
<p>_foo bar _</p>

```

### [Example 371](https://spec.commonmark.org/0.29/#example-371)

This markdown:


```markdown
_(_foo)

```

Gives this correct output:


```html
<p>_(_foo)</p>

```

### [Example 373](https://spec.commonmark.org/0.29/#example-373)

This markdown:


```markdown
_foo_bar

```

Gives this correct output:


```html
<p>_foo_bar</p>

```

### [Example 374](https://spec.commonmark.org/0.29/#example-374)

This markdown:


```markdown
_пристаням_стремятся

```

Gives this correct output:


```html
<p>_пристаням_стремятся</p>

```

### [Example 377](https://spec.commonmark.org/0.29/#example-377)

This markdown:


```markdown
**foo bar**

```

Gives this correct output:


```html
<p><strong>foo bar</strong></p>

```

### [Example 380](https://spec.commonmark.org/0.29/#example-380)

This markdown:


```markdown
foo**bar**

```

Gives this correct output:


```html
<p>foo<strong>bar</strong></p>

```

### [Example 382](https://spec.commonmark.org/0.29/#example-382)

This markdown:


```markdown
__ foo bar__

```

Gives this correct output:


```html
<p>__ foo bar__</p>

```

### [Example 383](https://spec.commonmark.org/0.29/#example-383)

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

### [Example 384](https://spec.commonmark.org/0.29/#example-384)

This markdown:


```markdown
a__"foo"__

```

Gives this correct output:


```html
<p>a__&quot;foo&quot;__</p>

```

### [Example 385](https://spec.commonmark.org/0.29/#example-385)

This markdown:


```markdown
foo__bar__

```

Gives this correct output:


```html
<p>foo__bar__</p>

```

### [Example 386](https://spec.commonmark.org/0.29/#example-386)

This markdown:


```markdown
5__6__78

```

Gives this correct output:


```html
<p>5__6__78</p>

```

### [Example 387](https://spec.commonmark.org/0.29/#example-387)

This markdown:


```markdown
пристаням__стремятся__

```

Gives this correct output:


```html
<p>пристаням__стремятся__</p>

```

### [Example 395](https://spec.commonmark.org/0.29/#example-395)

This markdown:


```markdown
**foo**bar

```

Gives this correct output:


```html
<p><strong>foo</strong>bar</p>

```

### [Example 396](https://spec.commonmark.org/0.29/#example-396)

This markdown:


```markdown
__foo bar __

```

Gives this correct output:


```html
<p>__foo bar __</p>

```

### [Example 397](https://spec.commonmark.org/0.29/#example-397)

This markdown:


```markdown
__(__foo)

```

Gives this correct output:


```html
<p>__(__foo)</p>

```

### [Example 399](https://spec.commonmark.org/0.29/#example-399)

This markdown:


```markdown
__foo__bar

```

Gives this correct output:


```html
<p>__foo__bar</p>

```

### [Example 400](https://spec.commonmark.org/0.29/#example-400)

This markdown:


```markdown
__пристаням__стремятся

```

Gives this correct output:


```html
<p>__пристаням__стремятся</p>

```

### [Example 404](https://spec.commonmark.org/0.29/#example-404)

This markdown:


```markdown
*foo
bar*

```

Gives this correct output:


```html
<p><em>foo
bar</em></p>

```

### [Example 422](https://spec.commonmark.org/0.29/#example-422)

This markdown:


```markdown
**foo
bar**

```

Gives this correct output:


```html
<p><strong>foo
bar</strong></p>

```

### [Example 433](https://spec.commonmark.org/0.29/#example-433)

This markdown:


```markdown
__ is not an empty emphasis

```

Gives this correct output:


```html
<p>__ is not an empty emphasis</p>

```

### [Example 437](https://spec.commonmark.org/0.29/#example-437)

This markdown:


```markdown
foo *_*

```

Gives this correct output:


```html
<p>foo <em>_</em></p>

```

### [Example 440](https://spec.commonmark.org/0.29/#example-440)

This markdown:


```markdown
foo **_**

```

Gives this correct output:


```html
<p>foo <strong>_</strong></p>

```

### [Example 447](https://spec.commonmark.org/0.29/#example-447)

This markdown:


```markdown
foo ___

```

Gives this correct output:


```html
<p>foo ___</p>

```

### [Example 450](https://spec.commonmark.org/0.29/#example-450)

This markdown:


```markdown
foo _____

```

Gives this correct output:


```html
<p>foo _____</p>

```

### [Example 459](https://spec.commonmark.org/0.29/#example-459)

This markdown:


```markdown
**foo**

```

Gives this correct output:


```html
<p><strong>foo</strong></p>

```

### [Example 468](https://spec.commonmark.org/0.29/#example-468)

This markdown:


```markdown
*foo _bar* baz_

```

Gives this correct output:


```html
<p><em>foo _bar</em> baz_</p>

```

### [Example 473](https://spec.commonmark.org/0.29/#example-473)

This markdown:


```markdown
_foo [bar_](/url)

```

Gives this correct output:


```html
<p>_foo <a href="/url">bar_</a></p>

```

## Thematic breaks

### [Example 13](https://spec.commonmark.org/0.29/#example-13)

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

### [Example 14](https://spec.commonmark.org/0.29/#example-14)

This markdown:


```markdown
+++

```

Gives this correct output:


```html
<p>+++</p>

```

### [Example 15](https://spec.commonmark.org/0.29/#example-15)

This markdown:


```markdown
===

```

Gives this correct output:


```html
<p>===</p>

```

### [Example 17](https://spec.commonmark.org/0.29/#example-17)

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

### [Example 18](https://spec.commonmark.org/0.29/#example-18)

This markdown:


```markdown
    ***

```

Gives this correct output:


```html
<pre><code>***
</code></pre>

```

### [Example 20](https://spec.commonmark.org/0.29/#example-20)

This markdown:


```markdown
_____________________________________

```

Gives this correct output:


```html
<hr />

```

### [Example 28](https://spec.commonmark.org/0.29/#example-28)

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


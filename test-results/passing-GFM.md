# GFM

## ATX headings

### [Example 62](https://spec.commonmark.org/0.30/#example-62)

This markdown:


````````````markdown
# foo
## foo
### foo
#### foo
##### foo
###### foo

````````````

Gives this correct output:


````````````html
<h1>foo</h1>
<h2>foo</h2>
<h3>foo</h3>
<h4>foo</h4>
<h5>foo</h5>
<h6>foo</h6>

````````````

### [Example 63](https://spec.commonmark.org/0.30/#example-63)

This markdown:


````````````markdown
####### foo

````````````

Gives this correct output:


````````````html
<p>####### foo</p>

````````````

### [Example 64](https://spec.commonmark.org/0.30/#example-64)

This markdown:


````````````markdown
#5 bolt

#hashtag

````````````

Gives this correct output:


````````````html
<p>#5 bolt</p>
<p>#hashtag</p>

````````````

### [Example 65](https://spec.commonmark.org/0.30/#example-65)

This markdown:


````````````markdown
\## foo

````````````

Gives this correct output:


````````````html
<p>## foo</p>

````````````

### [Example 66](https://spec.commonmark.org/0.30/#example-66)

This markdown:


````````````markdown
# foo *bar* \*baz\*

````````````

Gives this correct output:


````````````html
<h1>foo <em>bar</em> *baz*</h1>

````````````

### [Example 67](https://spec.commonmark.org/0.30/#example-67)

This markdown:


````````````markdown
#                  foo                     

````````````

Gives this correct output:


````````````html
<h1>foo</h1>

````````````

### [Example 68](https://spec.commonmark.org/0.30/#example-68)

This markdown:


````````````markdown
 ### foo
  ## foo
   # foo

````````````

Gives this correct output:


````````````html
<h3>foo</h3>
<h2>foo</h2>
<h1>foo</h1>

````````````

### [Example 69](https://spec.commonmark.org/0.30/#example-69)

This markdown:


````````````markdown
    # foo

````````````

Gives this correct output:


````````````html
<pre><code># foo
</code></pre>

````````````

### [Example 70](https://spec.commonmark.org/0.30/#example-70)

This markdown:


````````````markdown
foo
    # bar

````````````

Gives this correct output:


````````````html
<p>foo
# bar</p>

````````````

### [Example 71](https://spec.commonmark.org/0.30/#example-71)

This markdown:


````````````markdown
## foo ##
  ###   bar    ###

````````````

Gives this correct output:


````````````html
<h2>foo</h2>
<h3>bar</h3>

````````````

### [Example 72](https://spec.commonmark.org/0.30/#example-72)

This markdown:


````````````markdown
# foo ##################################
##### foo ##

````````````

Gives this correct output:


````````````html
<h1>foo</h1>
<h5>foo</h5>

````````````

### [Example 73](https://spec.commonmark.org/0.30/#example-73)

This markdown:


````````````markdown
### foo ###     

````````````

Gives this correct output:


````````````html
<h3>foo</h3>

````````````

### [Example 74](https://spec.commonmark.org/0.30/#example-74)

This markdown:


````````````markdown
### foo ### b

````````````

Gives this correct output:


````````````html
<h3>foo ### b</h3>

````````````

### [Example 75](https://spec.commonmark.org/0.30/#example-75)

This markdown:


````````````markdown
# foo#

````````````

Gives this correct output:


````````````html
<h1>foo#</h1>

````````````

### [Example 76](https://spec.commonmark.org/0.30/#example-76)

This markdown:


````````````markdown
### foo \###
## foo #\##
# foo \#

````````````

Gives this correct output:


````````````html
<h3>foo ###</h3>
<h2>foo ###</h2>
<h1>foo #</h1>

````````````

### [Example 77](https://spec.commonmark.org/0.30/#example-77)

This markdown:


````````````markdown
****
## foo
****

````````````

Gives this correct output:


````````````html
<hr />
<h2>foo</h2>
<hr />

````````````

### [Example 78](https://spec.commonmark.org/0.30/#example-78)

This markdown:


````````````markdown
Foo bar
# baz
Bar foo

````````````

Gives this correct output:


````````````html
<p>Foo bar</p>
<h1>baz</h1>
<p>Bar foo</p>

````````````

### [Example 79](https://spec.commonmark.org/0.30/#example-79)

This markdown:


````````````markdown
## 
#
### ###

````````````

Gives this correct output:


````````````html
<h2></h2>
<h1></h1>
<h3></h3>

````````````

## Autolinks

### [Example 593](https://spec.commonmark.org/0.30/#example-593)

This markdown:


````````````markdown
<http://foo.bar.baz>

````````````

Gives this correct output:


````````````html
<p><a href="http://foo.bar.baz">http://foo.bar.baz</a></p>

````````````

### [Example 594](https://spec.commonmark.org/0.30/#example-594)

This markdown:


````````````markdown
<http://foo.bar.baz/test?q=hello&id=22&boolean>

````````````

Gives this correct output:


````````````html
<p><a href="http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>

````````````

### [Example 595](https://spec.commonmark.org/0.30/#example-595)

This markdown:


````````````markdown
<irc://foo.bar:2233/baz>

````````````

Gives this correct output:


````````````html
<p><a href="irc://foo.bar:2233/baz">irc://foo.bar:2233/baz</a></p>

````````````

### [Example 596](https://spec.commonmark.org/0.30/#example-596)

This markdown:


````````````markdown
<MAILTO:FOO@BAR.BAZ>

````````````

Gives this correct output:


````````````html
<p><a href="MAILTO:FOO@BAR.BAZ">MAILTO:FOO@BAR.BAZ</a></p>

````````````

### [Example 597](https://spec.commonmark.org/0.30/#example-597)

This markdown:


````````````markdown
<a+b+c:d>

````````````

Gives this correct output:


````````````html
<p><a href="a+b+c:d">a+b+c:d</a></p>

````````````

### [Example 598](https://spec.commonmark.org/0.30/#example-598)

This markdown:


````````````markdown
<made-up-scheme://foo,bar>

````````````

Gives this correct output:


````````````html
<p><a href="made-up-scheme://foo,bar">made-up-scheme://foo,bar</a></p>

````````````

### [Example 599](https://spec.commonmark.org/0.30/#example-599)

This markdown:


````````````markdown
<http://../>

````````````

Gives this correct output:


````````````html
<p><a href="http://../">http://../</a></p>

````````````

### [Example 600](https://spec.commonmark.org/0.30/#example-600)

This markdown:


````````````markdown
<localhost:5001/foo>

````````````

Gives this correct output:


````````````html
<p><a href="localhost:5001/foo">localhost:5001/foo</a></p>

````````````

### [Example 601](https://spec.commonmark.org/0.30/#example-601)

This markdown:


````````````markdown
<http://foo.bar/baz bim>

````````````

Gives this correct output:


````````````html
<p>&lt;http://foo.bar/baz bim&gt;</p>

````````````

### [Example 602](https://spec.commonmark.org/0.30/#example-602)

This markdown:


````````````markdown
<http://example.com/\[\>

````````````

Gives this correct output:


````````````html
<p><a href="http://example.com/%5C%5B%5C">http://example.com/\[\</a></p>

````````````

### [Example 603](https://spec.commonmark.org/0.30/#example-603)

This markdown:


````````````markdown
<foo@bar.example.com>

````````````

Gives this correct output:


````````````html
<p><a href="mailto:foo@bar.example.com">foo@bar.example.com</a></p>

````````````

### [Example 604](https://spec.commonmark.org/0.30/#example-604)

This markdown:


````````````markdown
<foo+special@Bar.baz-bar0.com>

````````````

Gives this correct output:


````````````html
<p><a href="mailto:foo+special@Bar.baz-bar0.com">foo+special@Bar.baz-bar0.com</a></p>

````````````

### [Example 605](https://spec.commonmark.org/0.30/#example-605)

This markdown:


````````````markdown
<foo\+@bar.example.com>

````````````

Gives this correct output:


````````````html
<p>&lt;foo+@bar.example.com&gt;</p>

````````````

### [Example 606](https://spec.commonmark.org/0.30/#example-606)

This markdown:


````````````markdown
<>

````````````

Gives this correct output:


````````````html
<p>&lt;&gt;</p>

````````````

### [Example 607](https://spec.commonmark.org/0.30/#example-607)

This markdown:


````````````markdown
< http://foo.bar >

````````````

Gives this correct output:


````````````html
<p>&lt; http://foo.bar &gt;</p>

````````````

### [Example 608](https://spec.commonmark.org/0.30/#example-608)

This markdown:


````````````markdown
<m:abc>

````````````

Gives this correct output:


````````````html
<p>&lt;m:abc&gt;</p>

````````````

### [Example 609](https://spec.commonmark.org/0.30/#example-609)

This markdown:


````````````markdown
<foo.bar.baz>

````````````

Gives this correct output:


````````````html
<p>&lt;foo.bar.baz&gt;</p>

````````````

### [Example 610](https://spec.commonmark.org/0.30/#example-610)

This markdown:


````````````markdown
http://example.com

````````````

Gives this correct output:


````````````html
<p>http://example.com</p>

````````````

### [Example 611](https://spec.commonmark.org/0.30/#example-611)

This markdown:


````````````markdown
foo@bar.example.com

````````````

Gives this correct output:


````````````html
<p>foo@bar.example.com</p>

````````````

## Backslash escapes

### [Example 12](https://spec.commonmark.org/0.30/#example-12)

This markdown:


````````````markdown
\!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~

````````````

Gives this correct output:


````````````html
<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>

````````````

### [Example 13](https://spec.commonmark.org/0.30/#example-13)

This markdown:


````````````markdown
\	\A\a\ \3\φ\«

````````````

Gives this correct output:


````````````html
<p>\	\A\a\ \3\φ\«</p>

````````````

### [Example 15](https://spec.commonmark.org/0.30/#example-15)

This markdown:


````````````markdown
\\*emphasis*

````````````

Gives this correct output:


````````````html
<p>\<em>emphasis</em></p>

````````````

### [Example 16](https://spec.commonmark.org/0.30/#example-16)

This markdown:


````````````markdown
foo\
bar

````````````

Gives this correct output:


````````````html
<p>foo<br />
bar</p>

````````````

### [Example 17](https://spec.commonmark.org/0.30/#example-17)

This markdown:


````````````markdown
`` \[\` ``

````````````

Gives this correct output:


````````````html
<p><code>\[\`</code></p>

````````````

### [Example 18](https://spec.commonmark.org/0.30/#example-18)

This markdown:


````````````markdown
    \[\]

````````````

Gives this correct output:


````````````html
<pre><code>\[\]
</code></pre>

````````````

### [Example 19](https://spec.commonmark.org/0.30/#example-19)

This markdown:


````````````markdown
~~~
\[\]
~~~

````````````

Gives this correct output:


````````````html
<pre><code>\[\]
</code></pre>

````````````

### [Example 20](https://spec.commonmark.org/0.30/#example-20)

This markdown:


````````````markdown
<http://example.com?find=\*>

````````````

Gives this correct output:


````````````html
<p><a href="http://example.com?find=%5C*">http://example.com?find=\*</a></p>

````````````

### [Example 22](https://spec.commonmark.org/0.30/#example-22)

This markdown:


````````````markdown
[foo](/bar\* "ti\*tle")

````````````

Gives this correct output:


````````````html
<p><a href="/bar*" title="ti*tle">foo</a></p>

````````````

### [Example 23](https://spec.commonmark.org/0.30/#example-23)

This markdown:


````````````markdown
[foo]

[foo]: /bar\* "ti\*tle"

````````````

Gives this correct output:


````````````html
<p><a href="/bar*" title="ti*tle">foo</a></p>

````````````

## Blank lines

### [Example 227](https://spec.commonmark.org/0.30/#example-227)

This markdown:


````````````markdown
  

aaa
  

# aaa

  

````````````

Gives this correct output:


````````````html
<p>aaa</p>
<h1>aaa</h1>

````````````

## Block quotes

### [Example 228](https://spec.commonmark.org/0.30/#example-228)

This markdown:


````````````markdown
> # Foo
> bar
> baz

````````````

Gives this correct output:


````````````html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

````````````

### [Example 229](https://spec.commonmark.org/0.30/#example-229)

This markdown:


````````````markdown
># Foo
>bar
> baz

````````````

Gives this correct output:


````````````html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

````````````

### [Example 230](https://spec.commonmark.org/0.30/#example-230)

This markdown:


````````````markdown
   > # Foo
   > bar
 > baz

````````````

Gives this correct output:


````````````html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

````````````

### [Example 231](https://spec.commonmark.org/0.30/#example-231)

This markdown:


````````````markdown
    > # Foo
    > bar
    > baz

````````````

Gives this correct output:


````````````html
<pre><code>&gt; # Foo
&gt; bar
&gt; baz
</code></pre>

````````````

### [Example 232](https://spec.commonmark.org/0.30/#example-232)

This markdown:


````````````markdown
> # Foo
> bar
baz

````````````

Gives this correct output:


````````````html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

````````````

### [Example 233](https://spec.commonmark.org/0.30/#example-233)

This markdown:


````````````markdown
> bar
baz
> foo

````````````

Gives this correct output:


````````````html
<blockquote>
<p>bar
baz
foo</p>
</blockquote>

````````````

### [Example 234](https://spec.commonmark.org/0.30/#example-234)

This markdown:


````````````markdown
> foo
---

````````````

Gives this correct output:


````````````html
<blockquote>
<p>foo</p>
</blockquote>
<hr />

````````````

### [Example 235](https://spec.commonmark.org/0.30/#example-235)

This markdown:


````````````markdown
> - foo
- bar

````````````

Gives this correct output:


````````````html
<blockquote>
<ul>
<li>foo</li>
</ul>
</blockquote>
<ul>
<li>bar</li>
</ul>

````````````

### [Example 236](https://spec.commonmark.org/0.30/#example-236)

This markdown:


````````````markdown
>     foo
    bar

````````````

Gives this correct output:


````````````html
<blockquote>
<pre><code>foo
</code></pre>
</blockquote>
<pre><code>bar
</code></pre>

````````````

### [Example 237](https://spec.commonmark.org/0.30/#example-237)

This markdown:


````````````markdown
> ```
foo
```

````````````

Gives this correct output:


````````````html
<blockquote>
<pre><code></code></pre>
</blockquote>
<p>foo</p>
<pre><code></code></pre>

````````````

### [Example 238](https://spec.commonmark.org/0.30/#example-238)

This markdown:


````````````markdown
> foo
    - bar

````````````

Gives this correct output:


````````````html
<blockquote>
<p>foo
- bar</p>
</blockquote>

````````````

### [Example 239](https://spec.commonmark.org/0.30/#example-239)

This markdown:


````````````markdown
>

````````````

Gives this correct output:


````````````html
<blockquote>
</blockquote>

````````````

### [Example 240](https://spec.commonmark.org/0.30/#example-240)

This markdown:


````````````markdown
>
>  
> 

````````````

Gives this correct output:


````````````html
<blockquote>
</blockquote>

````````````

### [Example 241](https://spec.commonmark.org/0.30/#example-241)

This markdown:


````````````markdown
>
> foo
>  

````````````

Gives this correct output:


````````````html
<blockquote>
<p>foo</p>
</blockquote>

````````````

### [Example 242](https://spec.commonmark.org/0.30/#example-242)

This markdown:


````````````markdown
> foo

> bar

````````````

Gives this correct output:


````````````html
<blockquote>
<p>foo</p>
</blockquote>
<blockquote>
<p>bar</p>
</blockquote>

````````````

### [Example 243](https://spec.commonmark.org/0.30/#example-243)

This markdown:


````````````markdown
> foo
> bar

````````````

Gives this correct output:


````````````html
<blockquote>
<p>foo
bar</p>
</blockquote>

````````````

### [Example 244](https://spec.commonmark.org/0.30/#example-244)

This markdown:


````````````markdown
> foo
>
> bar

````````````

Gives this correct output:


````````````html
<blockquote>
<p>foo</p>
<p>bar</p>
</blockquote>

````````````

### [Example 245](https://spec.commonmark.org/0.30/#example-245)

This markdown:


````````````markdown
foo
> bar

````````````

Gives this correct output:


````````````html
<p>foo</p>
<blockquote>
<p>bar</p>
</blockquote>

````````````

### [Example 246](https://spec.commonmark.org/0.30/#example-246)

This markdown:


````````````markdown
> aaa
***
> bbb

````````````

Gives this correct output:


````````````html
<blockquote>
<p>aaa</p>
</blockquote>
<hr />
<blockquote>
<p>bbb</p>
</blockquote>

````````````

### [Example 247](https://spec.commonmark.org/0.30/#example-247)

This markdown:


````````````markdown
> bar
baz

````````````

Gives this correct output:


````````````html
<blockquote>
<p>bar
baz</p>
</blockquote>

````````````

### [Example 248](https://spec.commonmark.org/0.30/#example-248)

This markdown:


````````````markdown
> bar

baz

````````````

Gives this correct output:


````````````html
<blockquote>
<p>bar</p>
</blockquote>
<p>baz</p>

````````````

### [Example 249](https://spec.commonmark.org/0.30/#example-249)

This markdown:


````````````markdown
> bar
>
baz

````````````

Gives this correct output:


````````````html
<blockquote>
<p>bar</p>
</blockquote>
<p>baz</p>

````````````

### [Example 250](https://spec.commonmark.org/0.30/#example-250)

This markdown:


````````````markdown
> > > foo
bar

````````````

Gives this correct output:


````````````html
<blockquote>
<blockquote>
<blockquote>
<p>foo
bar</p>
</blockquote>
</blockquote>
</blockquote>

````````````

### [Example 251](https://spec.commonmark.org/0.30/#example-251)

This markdown:


````````````markdown
>>> foo
> bar
>>baz

````````````

Gives this correct output:


````````````html
<blockquote>
<blockquote>
<blockquote>
<p>foo
bar
baz</p>
</blockquote>
</blockquote>
</blockquote>

````````````

### [Example 252](https://spec.commonmark.org/0.30/#example-252)

This markdown:


````````````markdown
>     code

>    not code

````````````

Gives this correct output:


````````````html
<blockquote>
<pre><code>code
</code></pre>
</blockquote>
<blockquote>
<p>not code</p>
</blockquote>

````````````

## Code spans

### [Example 328](https://spec.commonmark.org/0.30/#example-328)

This markdown:


````````````markdown
`foo`

````````````

Gives this correct output:


````````````html
<p><code>foo</code></p>

````````````

### [Example 329](https://spec.commonmark.org/0.30/#example-329)

This markdown:


````````````markdown
`` foo ` bar ``

````````````

Gives this correct output:


````````````html
<p><code>foo ` bar</code></p>

````````````

### [Example 330](https://spec.commonmark.org/0.30/#example-330)

This markdown:


````````````markdown
` `` `

````````````

Gives this correct output:


````````````html
<p><code>``</code></p>

````````````

### [Example 331](https://spec.commonmark.org/0.30/#example-331)

This markdown:


````````````markdown
`  ``  `

````````````

Gives this correct output:


````````````html
<p><code> `` </code></p>

````````````

### [Example 332](https://spec.commonmark.org/0.30/#example-332)

This markdown:


````````````markdown
` a`

````````````

Gives this correct output:


````````````html
<p><code> a</code></p>

````````````

### [Example 333](https://spec.commonmark.org/0.30/#example-333)

This markdown:


````````````markdown
` b `

````````````

Gives this correct output:


````````````html
<p><code> b </code></p>

````````````

### [Example 334](https://spec.commonmark.org/0.30/#example-334)

This markdown:


````````````markdown
` `
`  `

````````````

Gives this correct output:


````````````html
<p><code> </code>
<code>  </code></p>

````````````

### [Example 335](https://spec.commonmark.org/0.30/#example-335)

This markdown:


````````````markdown
``
foo
bar  
baz
``

````````````

Gives this correct output:


````````````html
<p><code>foo bar   baz</code></p>

````````````

### [Example 336](https://spec.commonmark.org/0.30/#example-336)

This markdown:


````````````markdown
``
foo 
``

````````````

Gives this correct output:


````````````html
<p><code>foo </code></p>

````````````

### [Example 337](https://spec.commonmark.org/0.30/#example-337)

This markdown:


````````````markdown
`foo   bar 
baz`

````````````

Gives this correct output:


````````````html
<p><code>foo   bar  baz</code></p>

````````````

### [Example 338](https://spec.commonmark.org/0.30/#example-338)

This markdown:


````````````markdown
`foo\`bar`

````````````

Gives this correct output:


````````````html
<p><code>foo\</code>bar`</p>

````````````

### [Example 339](https://spec.commonmark.org/0.30/#example-339)

This markdown:


````````````markdown
``foo`bar``

````````````

Gives this correct output:


````````````html
<p><code>foo`bar</code></p>

````````````

### [Example 340](https://spec.commonmark.org/0.30/#example-340)

This markdown:


````````````markdown
` foo `` bar `

````````````

Gives this correct output:


````````````html
<p><code>foo `` bar</code></p>

````````````

### [Example 341](https://spec.commonmark.org/0.30/#example-341)

This markdown:


````````````markdown
*foo`*`

````````````

Gives this correct output:


````````````html
<p>*foo<code>*</code></p>

````````````

### [Example 342](https://spec.commonmark.org/0.30/#example-342)

This markdown:


````````````markdown
[not a `link](/foo`)

````````````

Gives this correct output:


````````````html
<p>[not a <code>link](/foo</code>)</p>

````````````

### [Example 343](https://spec.commonmark.org/0.30/#example-343)

This markdown:


````````````markdown
`<a href="`">`

````````````

Gives this correct output:


````````````html
<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>

````````````

### [Example 345](https://spec.commonmark.org/0.30/#example-345)

This markdown:


````````````markdown
`<http://foo.bar.`baz>`

````````````

Gives this correct output:


````````````html
<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>

````````````

### [Example 346](https://spec.commonmark.org/0.30/#example-346)

This markdown:


````````````markdown
<http://foo.bar.`baz>`

````````````

Gives this correct output:


````````````html
<p><a href="http://foo.bar.%60baz">http://foo.bar.`baz</a>`</p>

````````````

### [Example 347](https://spec.commonmark.org/0.30/#example-347)

This markdown:


````````````markdown
```foo``

````````````

Gives this correct output:


````````````html
<p>```foo``</p>

````````````

### [Example 348](https://spec.commonmark.org/0.30/#example-348)

This markdown:


````````````markdown
`foo

````````````

Gives this correct output:


````````````html
<p>`foo</p>

````````````

### [Example 349](https://spec.commonmark.org/0.30/#example-349)

This markdown:


````````````markdown
`foo``bar``

````````````

Gives this correct output:


````````````html
<p>`foo<code>bar</code></p>

````````````

## Emphasis and strong emphasis

### [Example 350](https://spec.commonmark.org/0.30/#example-350)

This markdown:


````````````markdown
*foo bar*

````````````

Gives this correct output:


````````````html
<p><em>foo bar</em></p>

````````````

### [Example 351](https://spec.commonmark.org/0.30/#example-351)

This markdown:


````````````markdown
a * foo bar*

````````````

Gives this correct output:


````````````html
<p>a * foo bar*</p>

````````````

### [Example 352](https://spec.commonmark.org/0.30/#example-352)

This markdown:


````````````markdown
a*"foo"*

````````````

Gives this correct output:


````````````html
<p>a*&quot;foo&quot;*</p>

````````````

### [Example 353](https://spec.commonmark.org/0.30/#example-353)

This markdown:


````````````markdown
* a *

````````````

Gives this correct output:


````````````html
<p>* a *</p>

````````````

### [Example 354](https://spec.commonmark.org/0.30/#example-354)

This markdown:


````````````markdown
foo*bar*

````````````

Gives this correct output:


````````````html
<p>foo<em>bar</em></p>

````````````

### [Example 355](https://spec.commonmark.org/0.30/#example-355)

This markdown:


````````````markdown
5*6*78

````````````

Gives this correct output:


````````````html
<p>5<em>6</em>78</p>

````````````

### [Example 356](https://spec.commonmark.org/0.30/#example-356)

This markdown:


````````````markdown
_foo bar_

````````````

Gives this correct output:


````````````html
<p><em>foo bar</em></p>

````````````

### [Example 357](https://spec.commonmark.org/0.30/#example-357)

This markdown:


````````````markdown
_ foo bar_

````````````

Gives this correct output:


````````````html
<p>_ foo bar_</p>

````````````

### [Example 358](https://spec.commonmark.org/0.30/#example-358)

This markdown:


````````````markdown
a_"foo"_

````````````

Gives this correct output:


````````````html
<p>a_&quot;foo&quot;_</p>

````````````

### [Example 359](https://spec.commonmark.org/0.30/#example-359)

This markdown:


````````````markdown
foo_bar_

````````````

Gives this correct output:


````````````html
<p>foo_bar_</p>

````````````

### [Example 360](https://spec.commonmark.org/0.30/#example-360)

This markdown:


````````````markdown
5_6_78

````````````

Gives this correct output:


````````````html
<p>5_6_78</p>

````````````

### [Example 361](https://spec.commonmark.org/0.30/#example-361)

This markdown:


````````````markdown
пристаням_стремятся_

````````````

Gives this correct output:


````````````html
<p>пристаням_стремятся_</p>

````````````

### [Example 362](https://spec.commonmark.org/0.30/#example-362)

This markdown:


````````````markdown
aa_"bb"_cc

````````````

Gives this correct output:


````````````html
<p>aa_&quot;bb&quot;_cc</p>

````````````

### [Example 363](https://spec.commonmark.org/0.30/#example-363)

This markdown:


````````````markdown
foo-_(bar)_

````````````

Gives this correct output:


````````````html
<p>foo-<em>(bar)</em></p>

````````````

### [Example 364](https://spec.commonmark.org/0.30/#example-364)

This markdown:


````````````markdown
_foo*

````````````

Gives this correct output:


````````````html
<p>_foo*</p>

````````````

### [Example 365](https://spec.commonmark.org/0.30/#example-365)

This markdown:


````````````markdown
*foo bar *

````````````

Gives this correct output:


````````````html
<p>*foo bar *</p>

````````````

### [Example 366](https://spec.commonmark.org/0.30/#example-366)

This markdown:


````````````markdown
*foo bar
*

````````````

Gives this correct output:


````````````html
<p>*foo bar
*</p>

````````````

### [Example 367](https://spec.commonmark.org/0.30/#example-367)

This markdown:


````````````markdown
*(*foo)

````````````

Gives this correct output:


````````````html
<p>*(*foo)</p>

````````````

### [Example 368](https://spec.commonmark.org/0.30/#example-368)

This markdown:


````````````markdown
*(*foo*)*

````````````

Gives this correct output:


````````````html
<p><em>(<em>foo</em>)</em></p>

````````````

### [Example 369](https://spec.commonmark.org/0.30/#example-369)

This markdown:


````````````markdown
*foo*bar

````````````

Gives this correct output:


````````````html
<p><em>foo</em>bar</p>

````````````

### [Example 370](https://spec.commonmark.org/0.30/#example-370)

This markdown:


````````````markdown
_foo bar _

````````````

Gives this correct output:


````````````html
<p>_foo bar _</p>

````````````

### [Example 371](https://spec.commonmark.org/0.30/#example-371)

This markdown:


````````````markdown
_(_foo)

````````````

Gives this correct output:


````````````html
<p>_(_foo)</p>

````````````

### [Example 372](https://spec.commonmark.org/0.30/#example-372)

This markdown:


````````````markdown
_(_foo_)_

````````````

Gives this correct output:


````````````html
<p><em>(<em>foo</em>)</em></p>

````````````

### [Example 373](https://spec.commonmark.org/0.30/#example-373)

This markdown:


````````````markdown
_foo_bar

````````````

Gives this correct output:


````````````html
<p>_foo_bar</p>

````````````

### [Example 374](https://spec.commonmark.org/0.30/#example-374)

This markdown:


````````````markdown
_пристаням_стремятся

````````````

Gives this correct output:


````````````html
<p>_пристаням_стремятся</p>

````````````

### [Example 375](https://spec.commonmark.org/0.30/#example-375)

This markdown:


````````````markdown
_foo_bar_baz_

````````````

Gives this correct output:


````````````html
<p><em>foo_bar_baz</em></p>

````````````

### [Example 376](https://spec.commonmark.org/0.30/#example-376)

This markdown:


````````````markdown
_(bar)_.

````````````

Gives this correct output:


````````````html
<p><em>(bar)</em>.</p>

````````````

### [Example 377](https://spec.commonmark.org/0.30/#example-377)

This markdown:


````````````markdown
**foo bar**

````````````

Gives this correct output:


````````````html
<p><strong>foo bar</strong></p>

````````````

### [Example 378](https://spec.commonmark.org/0.30/#example-378)

This markdown:


````````````markdown
** foo bar**

````````````

Gives this correct output:


````````````html
<p>** foo bar**</p>

````````````

### [Example 379](https://spec.commonmark.org/0.30/#example-379)

This markdown:


````````````markdown
a**"foo"**

````````````

Gives this correct output:


````````````html
<p>a**&quot;foo&quot;**</p>

````````````

### [Example 380](https://spec.commonmark.org/0.30/#example-380)

This markdown:


````````````markdown
foo**bar**

````````````

Gives this correct output:


````````````html
<p>foo<strong>bar</strong></p>

````````````

### [Example 381](https://spec.commonmark.org/0.30/#example-381)

This markdown:


````````````markdown
__foo bar__

````````````

Gives this correct output:


````````````html
<p><strong>foo bar</strong></p>

````````````

### [Example 382](https://spec.commonmark.org/0.30/#example-382)

This markdown:


````````````markdown
__ foo bar__

````````````

Gives this correct output:


````````````html
<p>__ foo bar__</p>

````````````

### [Example 383](https://spec.commonmark.org/0.30/#example-383)

This markdown:


````````````markdown
__
foo bar__

````````````

Gives this correct output:


````````````html
<p>__
foo bar__</p>

````````````

### [Example 384](https://spec.commonmark.org/0.30/#example-384)

This markdown:


````````````markdown
a__"foo"__

````````````

Gives this correct output:


````````````html
<p>a__&quot;foo&quot;__</p>

````````````

### [Example 385](https://spec.commonmark.org/0.30/#example-385)

This markdown:


````````````markdown
foo__bar__

````````````

Gives this correct output:


````````````html
<p>foo__bar__</p>

````````````

### [Example 386](https://spec.commonmark.org/0.30/#example-386)

This markdown:


````````````markdown
5__6__78

````````````

Gives this correct output:


````````````html
<p>5__6__78</p>

````````````

### [Example 387](https://spec.commonmark.org/0.30/#example-387)

This markdown:


````````````markdown
пристаням__стремятся__

````````````

Gives this correct output:


````````````html
<p>пристаням__стремятся__</p>

````````````

### [Example 388](https://spec.commonmark.org/0.30/#example-388)

This markdown:


````````````markdown
__foo, __bar__, baz__

````````````

Gives this correct output:


````````````html
<p><strong>foo, <strong>bar</strong>, baz</strong></p>

````````````

### [Example 389](https://spec.commonmark.org/0.30/#example-389)

This markdown:


````````````markdown
foo-__(bar)__

````````````

Gives this correct output:


````````````html
<p>foo-<strong>(bar)</strong></p>

````````````

### [Example 390](https://spec.commonmark.org/0.30/#example-390)

This markdown:


````````````markdown
**foo bar **

````````````

Gives this correct output:


````````````html
<p>**foo bar **</p>

````````````

### [Example 391](https://spec.commonmark.org/0.30/#example-391)

This markdown:


````````````markdown
**(**foo)

````````````

Gives this correct output:


````````````html
<p>**(**foo)</p>

````````````

### [Example 392](https://spec.commonmark.org/0.30/#example-392)

This markdown:


````````````markdown
*(**foo**)*

````````````

Gives this correct output:


````````````html
<p><em>(<strong>foo</strong>)</em></p>

````````````

### [Example 393](https://spec.commonmark.org/0.30/#example-393)

This markdown:


````````````markdown
**Gomphocarpus (*Gomphocarpus physocarpus*, syn.
*Asclepias physocarpa*)**

````````````

Gives this correct output:


````````````html
<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.
<em>Asclepias physocarpa</em>)</strong></p>

````````````

### [Example 394](https://spec.commonmark.org/0.30/#example-394)

This markdown:


````````````markdown
**foo "*bar*" foo**

````````````

Gives this correct output:


````````````html
<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>

````````````

### [Example 395](https://spec.commonmark.org/0.30/#example-395)

This markdown:


````````````markdown
**foo**bar

````````````

Gives this correct output:


````````````html
<p><strong>foo</strong>bar</p>

````````````

### [Example 396](https://spec.commonmark.org/0.30/#example-396)

This markdown:


````````````markdown
__foo bar __

````````````

Gives this correct output:


````````````html
<p>__foo bar __</p>

````````````

### [Example 397](https://spec.commonmark.org/0.30/#example-397)

This markdown:


````````````markdown
__(__foo)

````````````

Gives this correct output:


````````````html
<p>__(__foo)</p>

````````````

### [Example 398](https://spec.commonmark.org/0.30/#example-398)

This markdown:


````````````markdown
_(__foo__)_

````````````

Gives this correct output:


````````````html
<p><em>(<strong>foo</strong>)</em></p>

````````````

### [Example 399](https://spec.commonmark.org/0.30/#example-399)

This markdown:


````````````markdown
__foo__bar

````````````

Gives this correct output:


````````````html
<p>__foo__bar</p>

````````````

### [Example 400](https://spec.commonmark.org/0.30/#example-400)

This markdown:


````````````markdown
__пристаням__стремятся

````````````

Gives this correct output:


````````````html
<p>__пристаням__стремятся</p>

````````````

### [Example 401](https://spec.commonmark.org/0.30/#example-401)

This markdown:


````````````markdown
__foo__bar__baz__

````````````

Gives this correct output:


````````````html
<p><strong>foo__bar__baz</strong></p>

````````````

### [Example 402](https://spec.commonmark.org/0.30/#example-402)

This markdown:


````````````markdown
__(bar)__.

````````````

Gives this correct output:


````````````html
<p><strong>(bar)</strong>.</p>

````````````

### [Example 403](https://spec.commonmark.org/0.30/#example-403)

This markdown:


````````````markdown
*foo [bar](/url)*

````````````

Gives this correct output:


````````````html
<p><em>foo <a href="/url">bar</a></em></p>

````````````

### [Example 404](https://spec.commonmark.org/0.30/#example-404)

This markdown:


````````````markdown
*foo
bar*

````````````

Gives this correct output:


````````````html
<p><em>foo
bar</em></p>

````````````

### [Example 405](https://spec.commonmark.org/0.30/#example-405)

This markdown:


````````````markdown
_foo __bar__ baz_

````````````

Gives this correct output:


````````````html
<p><em>foo <strong>bar</strong> baz</em></p>

````````````

### [Example 406](https://spec.commonmark.org/0.30/#example-406)

This markdown:


````````````markdown
_foo _bar_ baz_

````````````

Gives this correct output:


````````````html
<p><em>foo <em>bar</em> baz</em></p>

````````````

### [Example 407](https://spec.commonmark.org/0.30/#example-407)

This markdown:


````````````markdown
__foo_ bar_

````````````

Gives this correct output:


````````````html
<p><em><em>foo</em> bar</em></p>

````````````

### [Example 408](https://spec.commonmark.org/0.30/#example-408)

This markdown:


````````````markdown
*foo *bar**

````````````

Gives this correct output:


````````````html
<p><em>foo <em>bar</em></em></p>

````````````

### [Example 409](https://spec.commonmark.org/0.30/#example-409)

This markdown:


````````````markdown
*foo **bar** baz*

````````````

Gives this correct output:


````````````html
<p><em>foo <strong>bar</strong> baz</em></p>

````````````

### [Example 410](https://spec.commonmark.org/0.30/#example-410)

This markdown:


````````````markdown
*foo**bar**baz*

````````````

Gives this correct output:


````````````html
<p><em>foo<strong>bar</strong>baz</em></p>

````````````

### [Example 411](https://spec.commonmark.org/0.30/#example-411)

This markdown:


````````````markdown
*foo**bar*

````````````

Gives this correct output:


````````````html
<p><em>foo**bar</em></p>

````````````

### [Example 412](https://spec.commonmark.org/0.30/#example-412)

This markdown:


````````````markdown
***foo** bar*

````````````

Gives this correct output:


````````````html
<p><em><strong>foo</strong> bar</em></p>

````````````

### [Example 413](https://spec.commonmark.org/0.30/#example-413)

This markdown:


````````````markdown
*foo **bar***

````````````

Gives this correct output:


````````````html
<p><em>foo <strong>bar</strong></em></p>

````````````

### [Example 414](https://spec.commonmark.org/0.30/#example-414)

This markdown:


````````````markdown
*foo**bar***

````````````

Gives this correct output:


````````````html
<p><em>foo<strong>bar</strong></em></p>

````````````

### [Example 415](https://spec.commonmark.org/0.30/#example-415)

This markdown:


````````````markdown
foo***bar***baz

````````````

Gives this correct output:


````````````html
<p>foo<em><strong>bar</strong></em>baz</p>

````````````

### [Example 416](https://spec.commonmark.org/0.30/#example-416)

This markdown:


````````````markdown
foo******bar*********baz

````````````

Gives this correct output:


````````````html
<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>

````````````

### [Example 417](https://spec.commonmark.org/0.30/#example-417)

This markdown:


````````````markdown
*foo **bar *baz* bim** bop*

````````````

Gives this correct output:


````````````html
<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>

````````````

### [Example 418](https://spec.commonmark.org/0.30/#example-418)

This markdown:


````````````markdown
*foo [*bar*](/url)*

````````````

Gives this correct output:


````````````html
<p><em>foo <a href="/url"><em>bar</em></a></em></p>

````````````

### [Example 419](https://spec.commonmark.org/0.30/#example-419)

This markdown:


````````````markdown
** is not an empty emphasis

````````````

Gives this correct output:


````````````html
<p>** is not an empty emphasis</p>

````````````

### [Example 420](https://spec.commonmark.org/0.30/#example-420)

This markdown:


````````````markdown
**** is not an empty strong emphasis

````````````

Gives this correct output:


````````````html
<p>**** is not an empty strong emphasis</p>

````````````

### [Example 421](https://spec.commonmark.org/0.30/#example-421)

This markdown:


````````````markdown
**foo [bar](/url)**

````````````

Gives this correct output:


````````````html
<p><strong>foo <a href="/url">bar</a></strong></p>

````````````

### [Example 422](https://spec.commonmark.org/0.30/#example-422)

This markdown:


````````````markdown
**foo
bar**

````````````

Gives this correct output:


````````````html
<p><strong>foo
bar</strong></p>

````````````

### [Example 423](https://spec.commonmark.org/0.30/#example-423)

This markdown:


````````````markdown
__foo _bar_ baz__

````````````

Gives this correct output:


````````````html
<p><strong>foo <em>bar</em> baz</strong></p>

````````````

### [Example 424](https://spec.commonmark.org/0.30/#example-424)

This markdown:


````````````markdown
__foo __bar__ baz__

````````````

Gives this correct output:


````````````html
<p><strong>foo <strong>bar</strong> baz</strong></p>

````````````

### [Example 425](https://spec.commonmark.org/0.30/#example-425)

This markdown:


````````````markdown
____foo__ bar__

````````````

Gives this correct output:


````````````html
<p><strong><strong>foo</strong> bar</strong></p>

````````````

### [Example 426](https://spec.commonmark.org/0.30/#example-426)

This markdown:


````````````markdown
**foo **bar****

````````````

Gives this correct output:


````````````html
<p><strong>foo <strong>bar</strong></strong></p>

````````````

### [Example 427](https://spec.commonmark.org/0.30/#example-427)

This markdown:


````````````markdown
**foo *bar* baz**

````````````

Gives this correct output:


````````````html
<p><strong>foo <em>bar</em> baz</strong></p>

````````````

### [Example 428](https://spec.commonmark.org/0.30/#example-428)

This markdown:


````````````markdown
**foo*bar*baz**

````````````

Gives this correct output:


````````````html
<p><strong>foo<em>bar</em>baz</strong></p>

````````````

### [Example 429](https://spec.commonmark.org/0.30/#example-429)

This markdown:


````````````markdown
***foo* bar**

````````````

Gives this correct output:


````````````html
<p><strong><em>foo</em> bar</strong></p>

````````````

### [Example 430](https://spec.commonmark.org/0.30/#example-430)

This markdown:


````````````markdown
**foo *bar***

````````````

Gives this correct output:


````````````html
<p><strong>foo <em>bar</em></strong></p>

````````````

### [Example 431](https://spec.commonmark.org/0.30/#example-431)

This markdown:


````````````markdown
**foo *bar **baz**
bim* bop**

````````````

Gives this correct output:


````````````html
<p><strong>foo <em>bar <strong>baz</strong>
bim</em> bop</strong></p>

````````````

### [Example 432](https://spec.commonmark.org/0.30/#example-432)

This markdown:


````````````markdown
**foo [*bar*](/url)**

````````````

Gives this correct output:


````````````html
<p><strong>foo <a href="/url"><em>bar</em></a></strong></p>

````````````

### [Example 433](https://spec.commonmark.org/0.30/#example-433)

This markdown:


````````````markdown
__ is not an empty emphasis

````````````

Gives this correct output:


````````````html
<p>__ is not an empty emphasis</p>

````````````

### [Example 434](https://spec.commonmark.org/0.30/#example-434)

This markdown:


````````````markdown
____ is not an empty strong emphasis

````````````

Gives this correct output:


````````````html
<p>____ is not an empty strong emphasis</p>

````````````

### [Example 435](https://spec.commonmark.org/0.30/#example-435)

This markdown:


````````````markdown
foo ***

````````````

Gives this correct output:


````````````html
<p>foo ***</p>

````````````

### [Example 436](https://spec.commonmark.org/0.30/#example-436)

This markdown:


````````````markdown
foo *\**

````````````

Gives this correct output:


````````````html
<p>foo <em>*</em></p>

````````````

### [Example 437](https://spec.commonmark.org/0.30/#example-437)

This markdown:


````````````markdown
foo *_*

````````````

Gives this correct output:


````````````html
<p>foo <em>_</em></p>

````````````

### [Example 438](https://spec.commonmark.org/0.30/#example-438)

This markdown:


````````````markdown
foo *****

````````````

Gives this correct output:


````````````html
<p>foo *****</p>

````````````

### [Example 439](https://spec.commonmark.org/0.30/#example-439)

This markdown:


````````````markdown
foo **\***

````````````

Gives this correct output:


````````````html
<p>foo <strong>*</strong></p>

````````````

### [Example 440](https://spec.commonmark.org/0.30/#example-440)

This markdown:


````````````markdown
foo **_**

````````````

Gives this correct output:


````````````html
<p>foo <strong>_</strong></p>

````````````

### [Example 441](https://spec.commonmark.org/0.30/#example-441)

This markdown:


````````````markdown
**foo*

````````````

Gives this correct output:


````````````html
<p>*<em>foo</em></p>

````````````

### [Example 442](https://spec.commonmark.org/0.30/#example-442)

This markdown:


````````````markdown
*foo**

````````````

Gives this correct output:


````````````html
<p><em>foo</em>*</p>

````````````

### [Example 443](https://spec.commonmark.org/0.30/#example-443)

This markdown:


````````````markdown
***foo**

````````````

Gives this correct output:


````````````html
<p>*<strong>foo</strong></p>

````````````

### [Example 444](https://spec.commonmark.org/0.30/#example-444)

This markdown:


````````````markdown
****foo*

````````````

Gives this correct output:


````````````html
<p>***<em>foo</em></p>

````````````

### [Example 445](https://spec.commonmark.org/0.30/#example-445)

This markdown:


````````````markdown
**foo***

````````````

Gives this correct output:


````````````html
<p><strong>foo</strong>*</p>

````````````

### [Example 446](https://spec.commonmark.org/0.30/#example-446)

This markdown:


````````````markdown
*foo****

````````````

Gives this correct output:


````````````html
<p><em>foo</em>***</p>

````````````

### [Example 447](https://spec.commonmark.org/0.30/#example-447)

This markdown:


````````````markdown
foo ___

````````````

Gives this correct output:


````````````html
<p>foo ___</p>

````````````

### [Example 448](https://spec.commonmark.org/0.30/#example-448)

This markdown:


````````````markdown
foo _\__

````````````

Gives this correct output:


````````````html
<p>foo <em>_</em></p>

````````````

### [Example 449](https://spec.commonmark.org/0.30/#example-449)

This markdown:


````````````markdown
foo _*_

````````````

Gives this correct output:


````````````html
<p>foo <em>*</em></p>

````````````

### [Example 450](https://spec.commonmark.org/0.30/#example-450)

This markdown:


````````````markdown
foo _____

````````````

Gives this correct output:


````````````html
<p>foo _____</p>

````````````

### [Example 451](https://spec.commonmark.org/0.30/#example-451)

This markdown:


````````````markdown
foo __\___

````````````

Gives this correct output:


````````````html
<p>foo <strong>_</strong></p>

````````````

### [Example 452](https://spec.commonmark.org/0.30/#example-452)

This markdown:


````````````markdown
foo __*__

````````````

Gives this correct output:


````````````html
<p>foo <strong>*</strong></p>

````````````

### [Example 453](https://spec.commonmark.org/0.30/#example-453)

This markdown:


````````````markdown
__foo_

````````````

Gives this correct output:


````````````html
<p>_<em>foo</em></p>

````````````

### [Example 454](https://spec.commonmark.org/0.30/#example-454)

This markdown:


````````````markdown
_foo__

````````````

Gives this correct output:


````````````html
<p><em>foo</em>_</p>

````````````

### [Example 455](https://spec.commonmark.org/0.30/#example-455)

This markdown:


````````````markdown
___foo__

````````````

Gives this correct output:


````````````html
<p>_<strong>foo</strong></p>

````````````

### [Example 456](https://spec.commonmark.org/0.30/#example-456)

This markdown:


````````````markdown
____foo_

````````````

Gives this correct output:


````````````html
<p>___<em>foo</em></p>

````````````

### [Example 457](https://spec.commonmark.org/0.30/#example-457)

This markdown:


````````````markdown
__foo___

````````````

Gives this correct output:


````````````html
<p><strong>foo</strong>_</p>

````````````

### [Example 458](https://spec.commonmark.org/0.30/#example-458)

This markdown:


````````````markdown
_foo____

````````````

Gives this correct output:


````````````html
<p><em>foo</em>___</p>

````````````

### [Example 459](https://spec.commonmark.org/0.30/#example-459)

This markdown:


````````````markdown
**foo**

````````````

Gives this correct output:


````````````html
<p><strong>foo</strong></p>

````````````

### [Example 460](https://spec.commonmark.org/0.30/#example-460)

This markdown:


````````````markdown
*_foo_*

````````````

Gives this correct output:


````````````html
<p><em><em>foo</em></em></p>

````````````

### [Example 461](https://spec.commonmark.org/0.30/#example-461)

This markdown:


````````````markdown
__foo__

````````````

Gives this correct output:


````````````html
<p><strong>foo</strong></p>

````````````

### [Example 462](https://spec.commonmark.org/0.30/#example-462)

This markdown:


````````````markdown
_*foo*_

````````````

Gives this correct output:


````````````html
<p><em><em>foo</em></em></p>

````````````

### [Example 463](https://spec.commonmark.org/0.30/#example-463)

This markdown:


````````````markdown
****foo****

````````````

Gives this correct output:


````````````html
<p><strong><strong>foo</strong></strong></p>

````````````

### [Example 464](https://spec.commonmark.org/0.30/#example-464)

This markdown:


````````````markdown
____foo____

````````````

Gives this correct output:


````````````html
<p><strong><strong>foo</strong></strong></p>

````````````

### [Example 465](https://spec.commonmark.org/0.30/#example-465)

This markdown:


````````````markdown
******foo******

````````````

Gives this correct output:


````````````html
<p><strong><strong><strong>foo</strong></strong></strong></p>

````````````

### [Example 466](https://spec.commonmark.org/0.30/#example-466)

This markdown:


````````````markdown
***foo***

````````````

Gives this correct output:


````````````html
<p><em><strong>foo</strong></em></p>

````````````

### [Example 467](https://spec.commonmark.org/0.30/#example-467)

This markdown:


````````````markdown
_____foo_____

````````````

Gives this correct output:


````````````html
<p><em><strong><strong>foo</strong></strong></em></p>

````````````

### [Example 468](https://spec.commonmark.org/0.30/#example-468)

This markdown:


````````````markdown
*foo _bar* baz_

````````````

Gives this correct output:


````````````html
<p><em>foo _bar</em> baz_</p>

````````````

### [Example 469](https://spec.commonmark.org/0.30/#example-469)

This markdown:


````````````markdown
*foo __bar *baz bim__ bam*

````````````

Gives this correct output:


````````````html
<p><em>foo <strong>bar *baz bim</strong> bam</em></p>

````````````

### [Example 470](https://spec.commonmark.org/0.30/#example-470)

This markdown:


````````````markdown
**foo **bar baz**

````````````

Gives this correct output:


````````````html
<p>**foo <strong>bar baz</strong></p>

````````````

### [Example 471](https://spec.commonmark.org/0.30/#example-471)

This markdown:


````````````markdown
*foo *bar baz*

````````````

Gives this correct output:


````````````html
<p>*foo <em>bar baz</em></p>

````````````

### [Example 472](https://spec.commonmark.org/0.30/#example-472)

This markdown:


````````````markdown
*[bar*](/url)

````````````

Gives this correct output:


````````````html
<p>*<a href="/url">bar*</a></p>

````````````

### [Example 473](https://spec.commonmark.org/0.30/#example-473)

This markdown:


````````````markdown
_foo [bar_](/url)

````````````

Gives this correct output:


````````````html
<p>_foo <a href="/url">bar_</a></p>

````````````

### [Example 474](https://spec.commonmark.org/0.30/#example-474)

This markdown:


````````````markdown
*<img src="foo" title="*"/>

````````````

Gives this correct output:


````````````html
<p>*<img src="foo" title="*"/></p>

````````````

### [Example 477](https://spec.commonmark.org/0.30/#example-477)

This markdown:


````````````markdown
*a `*`*

````````````

Gives this correct output:


````````````html
<p><em>a <code>*</code></em></p>

````````````

### [Example 478](https://spec.commonmark.org/0.30/#example-478)

This markdown:


````````````markdown
_a `_`_

````````````

Gives this correct output:


````````````html
<p><em>a <code>_</code></em></p>

````````````

### [Example 479](https://spec.commonmark.org/0.30/#example-479)

This markdown:


````````````markdown
**a<http://foo.bar/?q=**>

````````````

Gives this correct output:


````````````html
<p>**a<a href="http://foo.bar/?q=**">http://foo.bar/?q=**</a></p>

````````````

### [Example 480](https://spec.commonmark.org/0.30/#example-480)

This markdown:


````````````markdown
__a<http://foo.bar/?q=__>

````````````

Gives this correct output:


````````````html
<p>__a<a href="http://foo.bar/?q=__">http://foo.bar/?q=__</a></p>

````````````

## Entity and numeric character references

### [Example 26](https://spec.commonmark.org/0.30/#example-26)

This markdown:


````````````markdown
&#35; &#1234; &#992; &#0;

````````````

Gives this correct output:


````````````html
<p># Ӓ Ϡ �</p>

````````````

### [Example 27](https://spec.commonmark.org/0.30/#example-27)

This markdown:


````````````markdown
&#X22; &#XD06; &#xcab;

````````````

Gives this correct output:


````````````html
<p>&quot; ആ ಫ</p>

````````````

### [Example 29](https://spec.commonmark.org/0.30/#example-29)

This markdown:


````````````markdown
&copy

````````````

Gives this correct output:


````````````html
<p>&amp;copy</p>

````````````

### [Example 30](https://spec.commonmark.org/0.30/#example-30)

This markdown:


````````````markdown
&MadeUpEntity;

````````````

Gives this correct output:


````````````html
<p>&amp;MadeUpEntity;</p>

````````````

### [Example 32](https://spec.commonmark.org/0.30/#example-32)

This markdown:


````````````markdown
[foo](/f&ouml;&ouml; "f&ouml;&ouml;")

````````````

Gives this correct output:


````````````html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>

````````````

### [Example 33](https://spec.commonmark.org/0.30/#example-33)

This markdown:


````````````markdown
[foo]

[foo]: /f&ouml;&ouml; "f&ouml;&ouml;"

````````````

Gives this correct output:


````````````html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>

````````````

### [Example 34](https://spec.commonmark.org/0.30/#example-34)

This markdown:


````````````markdown
``` f&ouml;&ouml;
foo
```

````````````

Gives this correct output:


````````````html
<pre><code class="language-föö">foo
</code></pre>

````````````

### [Example 35](https://spec.commonmark.org/0.30/#example-35)

This markdown:


````````````markdown
`f&ouml;&ouml;`

````````````

Gives this correct output:


````````````html
<p><code>f&amp;ouml;&amp;ouml;</code></p>

````````````

### [Example 36](https://spec.commonmark.org/0.30/#example-36)

This markdown:


````````````markdown
    f&ouml;f&ouml;

````````````

Gives this correct output:


````````````html
<pre><code>f&amp;ouml;f&amp;ouml;
</code></pre>

````````````

### [Example 37](https://spec.commonmark.org/0.30/#example-37)

This markdown:


````````````markdown
&#42;foo&#42;
*foo*

````````````

Gives this correct output:


````````````html
<p>*foo*
<em>foo</em></p>

````````````

### [Example 38](https://spec.commonmark.org/0.30/#example-38)

This markdown:


````````````markdown
&#42; foo

* foo

````````````

Gives this correct output:


````````````html
<p>* foo</p>
<ul>
<li>foo</li>
</ul>

````````````

### [Example 39](https://spec.commonmark.org/0.30/#example-39)

This markdown:


````````````markdown
foo&#10;&#10;bar

````````````

Gives this correct output:


````````````html
<p>foo

bar</p>

````````````

### [Example 40](https://spec.commonmark.org/0.30/#example-40)

This markdown:


````````````markdown
&#9;foo

````````````

Gives this correct output:


````````````html
<p>	foo</p>

````````````

### [Example 41](https://spec.commonmark.org/0.30/#example-41)

This markdown:


````````````markdown
[a](url &quot;tit&quot;)

````````````

Gives this correct output:


````````````html
<p>[a](url &quot;tit&quot;)</p>

````````````

## Fenced code blocks

### [Example 119](https://spec.commonmark.org/0.30/#example-119)

This markdown:


````````````markdown
```
<
 >
```

````````````

Gives this correct output:


````````````html
<pre><code>&lt;
 &gt;
</code></pre>

````````````

### [Example 120](https://spec.commonmark.org/0.30/#example-120)

This markdown:


````````````markdown
~~~
<
 >
~~~

````````````

Gives this correct output:


````````````html
<pre><code>&lt;
 &gt;
</code></pre>

````````````

### [Example 121](https://spec.commonmark.org/0.30/#example-121)

This markdown:


````````````markdown
``
foo
``

````````````

Gives this correct output:


````````````html
<p><code>foo</code></p>

````````````

### [Example 122](https://spec.commonmark.org/0.30/#example-122)

This markdown:


````````````markdown
```
aaa
~~~
```

````````````

Gives this correct output:


````````````html
<pre><code>aaa
~~~
</code></pre>

````````````

### [Example 123](https://spec.commonmark.org/0.30/#example-123)

This markdown:


````````````markdown
~~~
aaa
```
~~~

````````````

Gives this correct output:


````````````html
<pre><code>aaa
```
</code></pre>

````````````

### [Example 124](https://spec.commonmark.org/0.30/#example-124)

This markdown:


````````````markdown
````
aaa
```
``````

````````````

Gives this correct output:


````````````html
<pre><code>aaa
```
</code></pre>

````````````

### [Example 125](https://spec.commonmark.org/0.30/#example-125)

This markdown:


````````````markdown
~~~~
aaa
~~~
~~~~

````````````

Gives this correct output:


````````````html
<pre><code>aaa
~~~
</code></pre>

````````````

### [Example 126](https://spec.commonmark.org/0.30/#example-126)

This markdown:


````````````markdown
```

````````````

Gives this correct output:


````````````html
<pre><code></code></pre>

````````````

### [Example 127](https://spec.commonmark.org/0.30/#example-127)

This markdown:


````````````markdown
`````

```
aaa

````````````

Gives this correct output:


````````````html
<pre><code>
```
aaa
</code></pre>

````````````

### [Example 128](https://spec.commonmark.org/0.30/#example-128)

This markdown:


````````````markdown
> ```
> aaa

bbb

````````````

Gives this correct output:


````````````html
<blockquote>
<pre><code>aaa
</code></pre>
</blockquote>
<p>bbb</p>

````````````

### [Example 129](https://spec.commonmark.org/0.30/#example-129)

This markdown:


````````````markdown
```

  
```

````````````

Gives this correct output:


````````````html
<pre><code>
  
</code></pre>

````````````

### [Example 130](https://spec.commonmark.org/0.30/#example-130)

This markdown:


````````````markdown
```
```

````````````

Gives this correct output:


````````````html
<pre><code></code></pre>

````````````

### [Example 131](https://spec.commonmark.org/0.30/#example-131)

This markdown:


````````````markdown
 ```
 aaa
aaa
```

````````````

Gives this correct output:


````````````html
<pre><code>aaa
aaa
</code></pre>

````````````

### [Example 132](https://spec.commonmark.org/0.30/#example-132)

This markdown:


````````````markdown
  ```
aaa
  aaa
aaa
  ```

````````````

Gives this correct output:


````````````html
<pre><code>aaa
aaa
aaa
</code></pre>

````````````

### [Example 133](https://spec.commonmark.org/0.30/#example-133)

This markdown:


````````````markdown
   ```
   aaa
    aaa
  aaa
   ```

````````````

Gives this correct output:


````````````html
<pre><code>aaa
 aaa
aaa
</code></pre>

````````````

### [Example 134](https://spec.commonmark.org/0.30/#example-134)

This markdown:


````````````markdown
    ```
    aaa
    ```

````````````

Gives this correct output:


````````````html
<pre><code>```
aaa
```
</code></pre>

````````````

### [Example 135](https://spec.commonmark.org/0.30/#example-135)

This markdown:


````````````markdown
```
aaa
  ```

````````````

Gives this correct output:


````````````html
<pre><code>aaa
</code></pre>

````````````

### [Example 136](https://spec.commonmark.org/0.30/#example-136)

This markdown:


````````````markdown
   ```
aaa
  ```

````````````

Gives this correct output:


````````````html
<pre><code>aaa
</code></pre>

````````````

### [Example 137](https://spec.commonmark.org/0.30/#example-137)

This markdown:


````````````markdown
```
aaa
    ```

````````````

Gives this correct output:


````````````html
<pre><code>aaa
    ```
</code></pre>

````````````

### [Example 138](https://spec.commonmark.org/0.30/#example-138)

This markdown:


````````````markdown
``` ```
aaa

````````````

Gives this correct output:


````````````html
<p><code> </code>
aaa</p>

````````````

### [Example 139](https://spec.commonmark.org/0.30/#example-139)

This markdown:


````````````markdown
~~~~~~
aaa
~~~ ~~

````````````

Gives this correct output:


````````````html
<pre><code>aaa
~~~ ~~
</code></pre>

````````````

### [Example 140](https://spec.commonmark.org/0.30/#example-140)

This markdown:


````````````markdown
foo
```
bar
```
baz

````````````

Gives this correct output:


````````````html
<p>foo</p>
<pre><code>bar
</code></pre>
<p>baz</p>

````````````

### [Example 141](https://spec.commonmark.org/0.30/#example-141)

This markdown:


````````````markdown
foo
---
~~~
bar
~~~
# baz

````````````

Gives this correct output:


````````````html
<h2>foo</h2>
<pre><code>bar
</code></pre>
<h1>baz</h1>

````````````

### [Example 142](https://spec.commonmark.org/0.30/#example-142)

This markdown:


````````````markdown
```ruby
def foo(x)
  return 3
end
```

````````````

Gives this correct output:


````````````html
<pre><code class="language-ruby">def foo(x)
  return 3
end
</code></pre>

````````````

### [Example 143](https://spec.commonmark.org/0.30/#example-143)

This markdown:


````````````markdown
~~~~    ruby startline=3 $%@#$
def foo(x)
  return 3
end
~~~~~~~

````````````

Gives this correct output:


````````````html
<pre><code class="language-ruby">def foo(x)
  return 3
end
</code></pre>

````````````

### [Example 144](https://spec.commonmark.org/0.30/#example-144)

This markdown:


````````````markdown
````;
````

````````````

Gives this correct output:


````````````html
<pre><code class="language-;"></code></pre>

````````````

### [Example 145](https://spec.commonmark.org/0.30/#example-145)

This markdown:


````````````markdown
``` aa ```
foo

````````````

Gives this correct output:


````````````html
<p><code>aa</code>
foo</p>

````````````

### [Example 146](https://spec.commonmark.org/0.30/#example-146)

This markdown:


````````````markdown
~~~ aa ``` ~~~
foo
~~~

````````````

Gives this correct output:


````````````html
<pre><code class="language-aa">foo
</code></pre>

````````````

### [Example 147](https://spec.commonmark.org/0.30/#example-147)

This markdown:


````````````markdown
```
``` aaa
```

````````````

Gives this correct output:


````````````html
<pre><code>``` aaa
</code></pre>

````````````

## HTML blocks

### [Example 152](https://spec.commonmark.org/0.30/#example-152)

This markdown:


````````````markdown
<DIV CLASS="foo">

*Markdown*

</DIV>

````````````

Gives this correct output:


````````````html
<DIV CLASS="foo">
<p><em>Markdown</em></p>
</DIV>

````````````

### [Example 153](https://spec.commonmark.org/0.30/#example-153)

This markdown:


````````````markdown
<div id="foo"
  class="bar">
</div>

````````````

Gives this correct output:


````````````html
<div id="foo"
  class="bar">
</div>

````````````

### [Example 154](https://spec.commonmark.org/0.30/#example-154)

This markdown:


````````````markdown
<div id="foo" class="bar
  baz">
</div>

````````````

Gives this correct output:


````````````html
<div id="foo" class="bar
  baz">
</div>

````````````

### [Example 167](https://spec.commonmark.org/0.30/#example-167)

This markdown:


````````````markdown
<del>

*foo*

</del>

````````````

Gives this correct output:


````````````html
<del>
<p><em>foo</em></p>
</del>

````````````

### [Example 179](https://spec.commonmark.org/0.30/#example-179)

This markdown:


````````````markdown
<!-- Foo

bar
   baz -->
okay

````````````

Gives this correct output:


````````````html
<!-- Foo

bar
   baz -->
<p>okay</p>

````````````

### [Example 182](https://spec.commonmark.org/0.30/#example-182)

This markdown:


````````````markdown
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
okay

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 188](https://spec.commonmark.org/0.30/#example-188)

This markdown:


````````````markdown
<div>

*Emphasized* text.

</div>

````````````

Gives this correct output:


````````````html
<div>
<p><em>Emphasized</em> text.</p>
</div>

````````````

## Hard line breaks

### [Example 633](https://spec.commonmark.org/0.30/#example-633)

This markdown:


````````````markdown
foo  
baz

````````````

Gives this correct output:


````````````html
<p>foo<br />
baz</p>

````````````

### [Example 634](https://spec.commonmark.org/0.30/#example-634)

This markdown:


````````````markdown
foo\
baz

````````````

Gives this correct output:


````````````html
<p>foo<br />
baz</p>

````````````

### [Example 635](https://spec.commonmark.org/0.30/#example-635)

This markdown:


````````````markdown
foo       
baz

````````````

Gives this correct output:


````````````html
<p>foo<br />
baz</p>

````````````

### [Example 636](https://spec.commonmark.org/0.30/#example-636)

This markdown:


````````````markdown
foo  
     bar

````````````

Gives this correct output:


````````````html
<p>foo<br />
bar</p>

````````````

### [Example 637](https://spec.commonmark.org/0.30/#example-637)

This markdown:


````````````markdown
foo\
     bar

````````````

Gives this correct output:


````````````html
<p>foo<br />
bar</p>

````````````

### [Example 638](https://spec.commonmark.org/0.30/#example-638)

This markdown:


````````````markdown
*foo  
bar*

````````````

Gives this correct output:


````````````html
<p><em>foo<br />
bar</em></p>

````````````

### [Example 639](https://spec.commonmark.org/0.30/#example-639)

This markdown:


````````````markdown
*foo\
bar*

````````````

Gives this correct output:


````````````html
<p><em>foo<br />
bar</em></p>

````````````

### [Example 640](https://spec.commonmark.org/0.30/#example-640)

This markdown:


````````````markdown
`code  
span`

````````````

Gives this correct output:


````````````html
<p><code>code   span</code></p>

````````````

### [Example 641](https://spec.commonmark.org/0.30/#example-641)

This markdown:


````````````markdown
`code\
span`

````````````

Gives this correct output:


````````````html
<p><code>code\ span</code></p>

````````````

### [Example 644](https://spec.commonmark.org/0.30/#example-644)

This markdown:


````````````markdown
foo\

````````````

Gives this correct output:


````````````html
<p>foo\</p>

````````````

### [Example 645](https://spec.commonmark.org/0.30/#example-645)

This markdown:


````````````markdown
foo  

````````````

Gives this correct output:


````````````html
<p>foo</p>

````````````

### [Example 646](https://spec.commonmark.org/0.30/#example-646)

This markdown:


````````````markdown
### foo\

````````````

Gives this correct output:


````````````html
<h3>foo\</h3>

````````````

### [Example 647](https://spec.commonmark.org/0.30/#example-647)

This markdown:


````````````markdown
### foo  

````````````

Gives this correct output:


````````````html
<h3>foo</h3>

````````````

## Images

### [Example 571](https://spec.commonmark.org/0.30/#example-571)

This markdown:


````````````markdown
![foo](/url "title")

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="foo" title="title" /></p>

````````````

### [Example 572](https://spec.commonmark.org/0.30/#example-572)

This markdown:


````````````markdown
![foo *bar*]

[foo *bar*]: train.jpg "train & tracks"

````````````

Gives this correct output:


````````````html
<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>

````````````

### [Example 573](https://spec.commonmark.org/0.30/#example-573)

This markdown:


````````````markdown
![foo ![bar](/url)](/url2)

````````````

Gives this correct output:


````````````html
<p><img src="/url2" alt="foo bar" /></p>

````````````

### [Example 574](https://spec.commonmark.org/0.30/#example-574)

This markdown:


````````````markdown
![foo [bar](/url)](/url2)

````````````

Gives this correct output:


````````````html
<p><img src="/url2" alt="foo bar" /></p>

````````````

### [Example 575](https://spec.commonmark.org/0.30/#example-575)

This markdown:


````````````markdown
![foo *bar*][]

[foo *bar*]: train.jpg "train & tracks"

````````````

Gives this correct output:


````````````html
<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>

````````````

### [Example 576](https://spec.commonmark.org/0.30/#example-576)

This markdown:


````````````markdown
![foo *bar*][foobar]

[FOOBAR]: train.jpg "train & tracks"

````````````

Gives this correct output:


````````````html
<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>

````````````

### [Example 577](https://spec.commonmark.org/0.30/#example-577)

This markdown:


````````````markdown
![foo](train.jpg)

````````````

Gives this correct output:


````````````html
<p><img src="train.jpg" alt="foo" /></p>

````````````

### [Example 578](https://spec.commonmark.org/0.30/#example-578)

This markdown:


````````````markdown
My ![foo bar](/path/to/train.jpg  "title"   )

````````````

Gives this correct output:


````````````html
<p>My <img src="/path/to/train.jpg" alt="foo bar" title="title" /></p>

````````````

### [Example 579](https://spec.commonmark.org/0.30/#example-579)

This markdown:


````````````markdown
![foo](<url>)

````````````

Gives this correct output:


````````````html
<p><img src="url" alt="foo" /></p>

````````````

### [Example 580](https://spec.commonmark.org/0.30/#example-580)

This markdown:


````````````markdown
![](/url)

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="" /></p>

````````````

### [Example 581](https://spec.commonmark.org/0.30/#example-581)

This markdown:


````````````markdown
![foo][bar]

[bar]: /url

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="foo" /></p>

````````````

### [Example 582](https://spec.commonmark.org/0.30/#example-582)

This markdown:


````````````markdown
![foo][bar]

[BAR]: /url

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="foo" /></p>

````````````

### [Example 583](https://spec.commonmark.org/0.30/#example-583)

This markdown:


````````````markdown
![foo][]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="foo" title="title" /></p>

````````````

### [Example 584](https://spec.commonmark.org/0.30/#example-584)

This markdown:


````````````markdown
![*foo* bar][]

[*foo* bar]: /url "title"

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="foo bar" title="title" /></p>

````````````

### [Example 585](https://spec.commonmark.org/0.30/#example-585)

This markdown:


````````````markdown
![Foo][]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="Foo" title="title" /></p>

````````````

### [Example 586](https://spec.commonmark.org/0.30/#example-586)

This markdown:


````````````markdown
![foo] 
[]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="foo" title="title" />
[]</p>

````````````

### [Example 587](https://spec.commonmark.org/0.30/#example-587)

This markdown:


````````````markdown
![foo]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="foo" title="title" /></p>

````````````

### [Example 588](https://spec.commonmark.org/0.30/#example-588)

This markdown:


````````````markdown
![*foo* bar]

[*foo* bar]: /url "title"

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="foo bar" title="title" /></p>

````````````

### [Example 589](https://spec.commonmark.org/0.30/#example-589)

This markdown:


````````````markdown
![[foo]]

[[foo]]: /url "title"

````````````

Gives this correct output:


````````````html
<p>![[foo]]</p>
<p>[[foo]]: /url &quot;title&quot;</p>

````````````

### [Example 590](https://spec.commonmark.org/0.30/#example-590)

This markdown:


````````````markdown
![Foo]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><img src="/url" alt="Foo" title="title" /></p>

````````````

### [Example 591](https://spec.commonmark.org/0.30/#example-591)

This markdown:


````````````markdown
!\[foo]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p>![foo]</p>

````````````

### [Example 592](https://spec.commonmark.org/0.30/#example-592)

This markdown:


````````````markdown
\![foo]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p>!<a href="/url" title="title">foo</a></p>

````````````

## Indented code blocks

### [Example 107](https://spec.commonmark.org/0.30/#example-107)

This markdown:


````````````markdown
    a simple
      indented code block

````````````

Gives this correct output:


````````````html
<pre><code>a simple
  indented code block
</code></pre>

````````````

### [Example 108](https://spec.commonmark.org/0.30/#example-108)

This markdown:


````````````markdown
  - foo

    bar

````````````

Gives this correct output:


````````````html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

````````````

### [Example 109](https://spec.commonmark.org/0.30/#example-109)

This markdown:


````````````markdown
1.  foo

    - bar

````````````

Gives this correct output:


````````````html
<ol>
<li>
<p>foo</p>
<ul>
<li>bar</li>
</ul>
</li>
</ol>

````````````

### [Example 110](https://spec.commonmark.org/0.30/#example-110)

This markdown:


````````````markdown
    <a/>
    *hi*

    - one

````````````

Gives this correct output:


````````````html
<pre><code>&lt;a/&gt;
*hi*

- one
</code></pre>

````````````

### [Example 111](https://spec.commonmark.org/0.30/#example-111)

This markdown:


````````````markdown
    chunk1

    chunk2
  
 
 
    chunk3

````````````

Gives this correct output:


````````````html
<pre><code>chunk1

chunk2



chunk3
</code></pre>

````````````

### [Example 112](https://spec.commonmark.org/0.30/#example-112)

This markdown:


````````````markdown
    chunk1
      
      chunk2

````````````

Gives this correct output:


````````````html
<pre><code>chunk1
  
  chunk2
</code></pre>

````````````

### [Example 113](https://spec.commonmark.org/0.30/#example-113)

This markdown:


````````````markdown
Foo
    bar


````````````

Gives this correct output:


````````````html
<p>Foo
bar</p>

````````````

### [Example 114](https://spec.commonmark.org/0.30/#example-114)

This markdown:


````````````markdown
    foo
bar

````````````

Gives this correct output:


````````````html
<pre><code>foo
</code></pre>
<p>bar</p>

````````````

### [Example 115](https://spec.commonmark.org/0.30/#example-115)

This markdown:


````````````markdown
# Heading
    foo
Heading
------
    foo
----

````````````

Gives this correct output:


````````````html
<h1>Heading</h1>
<pre><code>foo
</code></pre>
<h2>Heading</h2>
<pre><code>foo
</code></pre>
<hr />

````````````

### [Example 116](https://spec.commonmark.org/0.30/#example-116)

This markdown:


````````````markdown
        foo
    bar

````````````

Gives this correct output:


````````````html
<pre><code>    foo
bar
</code></pre>

````````````

### [Example 117](https://spec.commonmark.org/0.30/#example-117)

This markdown:


````````````markdown

    
    foo
    


````````````

Gives this correct output:


````````````html
<pre><code>foo
</code></pre>

````````````

### [Example 118](https://spec.commonmark.org/0.30/#example-118)

This markdown:


````````````markdown
    foo  

````````````

Gives this correct output:


````````````html
<pre><code>foo  
</code></pre>

````````````

## Inlines

### [Example 327](https://spec.commonmark.org/0.30/#example-327)

This markdown:


````````````markdown
`hi`lo`

````````````

Gives this correct output:


````````````html
<p><code>hi</code>lo`</p>

````````````

## Link reference definitions

### [Example 192](https://spec.commonmark.org/0.30/#example-192)

This markdown:


````````````markdown
[foo]: /url "title"

[foo]

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title">foo</a></p>

````````````

### [Example 193](https://spec.commonmark.org/0.30/#example-193)

This markdown:


````````````markdown
   [foo]: 
      /url  
           'the title'  

[foo]

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="the title">foo</a></p>

````````````

### [Example 195](https://spec.commonmark.org/0.30/#example-195)

This markdown:


````````````markdown
[Foo bar]:
<my url>
'title'

[Foo bar]

````````````

Gives this correct output:


````````````html
<p><a href="my%20url" title="title">Foo bar</a></p>

````````````

### [Example 196](https://spec.commonmark.org/0.30/#example-196)

This markdown:


````````````markdown
[foo]: /url '
title
line1
line2
'

[foo]

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="
title
line1
line2
">foo</a></p>

````````````

### [Example 197](https://spec.commonmark.org/0.30/#example-197)

This markdown:


````````````markdown
[foo]: /url 'title

with blank line'

[foo]

````````````

Gives this correct output:


````````````html
<p>[foo]: /url 'title</p>
<p>with blank line'</p>
<p>[foo]</p>

````````````

### [Example 198](https://spec.commonmark.org/0.30/#example-198)

This markdown:


````````````markdown
[foo]:
/url

[foo]

````````````

Gives this correct output:


````````````html
<p><a href="/url">foo</a></p>

````````````

### [Example 199](https://spec.commonmark.org/0.30/#example-199)

This markdown:


````````````markdown
[foo]:

[foo]

````````````

Gives this correct output:


````````````html
<p>[foo]:</p>
<p>[foo]</p>

````````````

### [Example 200](https://spec.commonmark.org/0.30/#example-200)

This markdown:


````````````markdown
[foo]: <>

[foo]

````````````

Gives this correct output:


````````````html
<p><a href="">foo</a></p>

````````````

### [Example 203](https://spec.commonmark.org/0.30/#example-203)

This markdown:


````````````markdown
[foo]

[foo]: url

````````````

Gives this correct output:


````````````html
<p><a href="url">foo</a></p>

````````````

### [Example 204](https://spec.commonmark.org/0.30/#example-204)

This markdown:


````````````markdown
[foo]

[foo]: first
[foo]: second

````````````

Gives this correct output:


````````````html
<p><a href="first">foo</a></p>

````````````

### [Example 205](https://spec.commonmark.org/0.30/#example-205)

This markdown:


````````````markdown
[FOO]: /url

[Foo]

````````````

Gives this correct output:


````````````html
<p><a href="/url">Foo</a></p>

````````````

### [Example 206](https://spec.commonmark.org/0.30/#example-206)

This markdown:


````````````markdown
[ΑΓΩ]: /φου

[αγω]

````````````

Gives this correct output:


````````````html
<p><a href="/%CF%86%CE%BF%CF%85">αγω</a></p>

````````````

### [Example 207](https://spec.commonmark.org/0.30/#example-207)

This markdown:


````````````markdown
[foo]: /url

````````````

Gives this correct output:


````````````html

````````````

### [Example 208](https://spec.commonmark.org/0.30/#example-208)

This markdown:


````````````markdown
[
foo
]: /url
bar

````````````

Gives this correct output:


````````````html
<p>bar</p>

````````````

### [Example 209](https://spec.commonmark.org/0.30/#example-209)

This markdown:


````````````markdown
[foo]: /url "title" ok

````````````

Gives this correct output:


````````````html
<p>[foo]: /url &quot;title&quot; ok</p>

````````````

### [Example 210](https://spec.commonmark.org/0.30/#example-210)

This markdown:


````````````markdown
[foo]: /url
"title" ok

````````````

Gives this correct output:


````````````html
<p>&quot;title&quot; ok</p>

````````````

### [Example 211](https://spec.commonmark.org/0.30/#example-211)

This markdown:


````````````markdown
    [foo]: /url "title"

[foo]

````````````

Gives this correct output:


````````````html
<pre><code>[foo]: /url &quot;title&quot;
</code></pre>
<p>[foo]</p>

````````````

### [Example 212](https://spec.commonmark.org/0.30/#example-212)

This markdown:


````````````markdown
```
[foo]: /url
```

[foo]

````````````

Gives this correct output:


````````````html
<pre><code>[foo]: /url
</code></pre>
<p>[foo]</p>

````````````

### [Example 214](https://spec.commonmark.org/0.30/#example-214)

This markdown:


````````````markdown
# [Foo]
[foo]: /url
> bar

````````````

Gives this correct output:


````````````html
<h1><a href="/url">Foo</a></h1>
<blockquote>
<p>bar</p>
</blockquote>

````````````

### [Example 215](https://spec.commonmark.org/0.30/#example-215)

This markdown:


````````````markdown
[foo]: /url
bar
===
[foo]

````````````

Gives this correct output:


````````````html
<h1>bar</h1>
<p><a href="/url">foo</a></p>

````````````

### [Example 216](https://spec.commonmark.org/0.30/#example-216)

This markdown:


````````````markdown
[foo]: /url
===
[foo]

````````````

Gives this correct output:


````````````html
<p>===
<a href="/url">foo</a></p>

````````````

### [Example 217](https://spec.commonmark.org/0.30/#example-217)

This markdown:


````````````markdown
[foo]: /foo-url "foo"
[bar]: /bar-url
  "bar"
[baz]: /baz-url

[foo],
[bar],
[baz]

````````````

Gives this correct output:


````````````html
<p><a href="/foo-url" title="foo">foo</a>,
<a href="/bar-url" title="bar">bar</a>,
<a href="/baz-url">baz</a></p>

````````````

### [Example 218](https://spec.commonmark.org/0.30/#example-218)

This markdown:


````````````markdown
[foo]

> [foo]: /url

````````````

Gives this correct output:


````````````html
<p><a href="/url">foo</a></p>
<blockquote>
</blockquote>

````````````

## Links

### [Example 481](https://spec.commonmark.org/0.30/#example-481)

This markdown:


````````````markdown
[link](/uri "title")

````````````

Gives this correct output:


````````````html
<p><a href="/uri" title="title">link</a></p>

````````````

### [Example 482](https://spec.commonmark.org/0.30/#example-482)

This markdown:


````````````markdown
[link](/uri)

````````````

Gives this correct output:


````````````html
<p><a href="/uri">link</a></p>

````````````

### [Example 483](https://spec.commonmark.org/0.30/#example-483)

This markdown:


````````````markdown
[](./target.md)

````````````

Gives this correct output:


````````````html
<p><a href="./target.md"></a></p>

````````````

### [Example 484](https://spec.commonmark.org/0.30/#example-484)

This markdown:


````````````markdown
[link]()

````````````

Gives this correct output:


````````````html
<p><a href="">link</a></p>

````````````

### [Example 485](https://spec.commonmark.org/0.30/#example-485)

This markdown:


````````````markdown
[link](<>)

````````````

Gives this correct output:


````````````html
<p><a href="">link</a></p>

````````````

### [Example 486](https://spec.commonmark.org/0.30/#example-486)

This markdown:


````````````markdown
[]()

````````````

Gives this correct output:


````````````html
<p><a href=""></a></p>

````````````

### [Example 487](https://spec.commonmark.org/0.30/#example-487)

This markdown:


````````````markdown
[link](/my uri)

````````````

Gives this correct output:


````````````html
<p>[link](/my uri)</p>

````````````

### [Example 488](https://spec.commonmark.org/0.30/#example-488)

This markdown:


````````````markdown
[link](</my uri>)

````````````

Gives this correct output:


````````````html
<p><a href="/my%20uri">link</a></p>

````````````

### [Example 489](https://spec.commonmark.org/0.30/#example-489)

This markdown:


````````````markdown
[link](foo
bar)

````````````

Gives this correct output:


````````````html
<p>[link](foo
bar)</p>

````````````

### [Example 491](https://spec.commonmark.org/0.30/#example-491)

This markdown:


````````````markdown
[a](<b)c>)

````````````

Gives this correct output:


````````````html
<p><a href="b)c">a</a></p>

````````````

### [Example 494](https://spec.commonmark.org/0.30/#example-494)

This markdown:


````````````markdown
[link](\(foo\))

````````````

Gives this correct output:


````````````html
<p><a href="(foo)">link</a></p>

````````````

### [Example 496](https://spec.commonmark.org/0.30/#example-496)

This markdown:


````````````markdown
[link](foo(and(bar))

````````````

Gives this correct output:


````````````html
<p>[link](foo(and(bar))</p>

````````````

### [Example 497](https://spec.commonmark.org/0.30/#example-497)

This markdown:


````````````markdown
[link](foo\(and\(bar\))

````````````

Gives this correct output:


````````````html
<p><a href="foo(and(bar)">link</a></p>

````````````

### [Example 498](https://spec.commonmark.org/0.30/#example-498)

This markdown:


````````````markdown
[link](<foo(and(bar)>)

````````````

Gives this correct output:


````````````html
<p><a href="foo(and(bar)">link</a></p>

````````````

### [Example 499](https://spec.commonmark.org/0.30/#example-499)

This markdown:


````````````markdown
[link](foo\)\:)

````````````

Gives this correct output:


````````````html
<p><a href="foo):">link</a></p>

````````````

### [Example 500](https://spec.commonmark.org/0.30/#example-500)

This markdown:


````````````markdown
[link](#fragment)

[link](http://example.com#fragment)

[link](http://example.com?foo=3#frag)

````````````

Gives this correct output:


````````````html
<p><a href="#fragment">link</a></p>
<p><a href="http://example.com#fragment">link</a></p>
<p><a href="http://example.com?foo=3#frag">link</a></p>

````````````

### [Example 501](https://spec.commonmark.org/0.30/#example-501)

This markdown:


````````````markdown
[link](foo\bar)

````````````

Gives this correct output:


````````````html
<p><a href="foo%5Cbar">link</a></p>

````````````

### [Example 502](https://spec.commonmark.org/0.30/#example-502)

This markdown:


````````````markdown
[link](foo%20b&auml;)

````````````

Gives this correct output:


````````````html
<p><a href="foo%20b%C3%A4">link</a></p>

````````````

### [Example 503](https://spec.commonmark.org/0.30/#example-503)

This markdown:


````````````markdown
[link]("title")

````````````

Gives this correct output:


````````````html
<p><a href="%22title%22">link</a></p>

````````````

### [Example 504](https://spec.commonmark.org/0.30/#example-504)

This markdown:


````````````markdown
[link](/url "title")
[link](/url 'title')
[link](/url (title))

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title">link</a>
<a href="/url" title="title">link</a>
<a href="/url" title="title">link</a></p>

````````````

### [Example 506](https://spec.commonmark.org/0.30/#example-506)

This markdown:


````````````markdown
[link](/url "title")

````````````

Gives this correct output:


````````````html
<p><a href="/url%C2%A0%22title%22">link</a></p>

````````````

### [Example 507](https://spec.commonmark.org/0.30/#example-507)

This markdown:


````````````markdown
[link](/url "title "and" title")

````````````

Gives this correct output:


````````````html
<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>

````````````

### [Example 509](https://spec.commonmark.org/0.30/#example-509)

This markdown:


````````````markdown
[link](   /uri
  "title"  )

````````````

Gives this correct output:


````````````html
<p><a href="/uri" title="title">link</a></p>

````````````

### [Example 510](https://spec.commonmark.org/0.30/#example-510)

This markdown:


````````````markdown
[link] (/uri)

````````````

Gives this correct output:


````````````html
<p>[link] (/uri)</p>

````````````

### [Example 511](https://spec.commonmark.org/0.30/#example-511)

This markdown:


````````````markdown
[link [foo [bar]]](/uri)

````````````

Gives this correct output:


````````````html
<p><a href="/uri">link [foo [bar]]</a></p>

````````````

### [Example 512](https://spec.commonmark.org/0.30/#example-512)

This markdown:


````````````markdown
[link] bar](/uri)

````````````

Gives this correct output:


````````````html
<p>[link] bar](/uri)</p>

````````````

### [Example 513](https://spec.commonmark.org/0.30/#example-513)

This markdown:


````````````markdown
[link [bar](/uri)

````````````

Gives this correct output:


````````````html
<p>[link <a href="/uri">bar</a></p>

````````````

### [Example 514](https://spec.commonmark.org/0.30/#example-514)

This markdown:


````````````markdown
[link \[bar](/uri)

````````````

Gives this correct output:


````````````html
<p><a href="/uri">link [bar</a></p>

````````````

### [Example 515](https://spec.commonmark.org/0.30/#example-515)

This markdown:


````````````markdown
[link *foo **bar** `#`*](/uri)

````````````

Gives this correct output:


````````````html
<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>

````````````

### [Example 516](https://spec.commonmark.org/0.30/#example-516)

This markdown:


````````````markdown
[![moon](moon.jpg)](/uri)

````````````

Gives this correct output:


````````````html
<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>

````````````

### [Example 517](https://spec.commonmark.org/0.30/#example-517)

This markdown:


````````````markdown
[foo [bar](/uri)](/uri)

````````````

Gives this correct output:


````````````html
<p>[foo <a href="/uri">bar</a>](/uri)</p>

````````````

### [Example 518](https://spec.commonmark.org/0.30/#example-518)

This markdown:


````````````markdown
[foo *[bar [baz](/uri)](/uri)*](/uri)

````````````

Gives this correct output:


````````````html
<p>[foo <em>[bar <a href="/uri">baz</a>](/uri)</em>](/uri)</p>

````````````

### [Example 519](https://spec.commonmark.org/0.30/#example-519)

This markdown:


````````````markdown
![[[foo](uri1)](uri2)](uri3)

````````````

Gives this correct output:


````````````html
<p><img src="uri3" alt="[foo](uri2)" /></p>

````````````

### [Example 520](https://spec.commonmark.org/0.30/#example-520)

This markdown:


````````````markdown
*[foo*](/uri)

````````````

Gives this correct output:


````````````html
<p>*<a href="/uri">foo*</a></p>

````````````

### [Example 521](https://spec.commonmark.org/0.30/#example-521)

This markdown:


````````````markdown
[foo *bar](baz*)

````````````

Gives this correct output:


````````````html
<p><a href="baz*">foo *bar</a></p>

````````````

### [Example 522](https://spec.commonmark.org/0.30/#example-522)

This markdown:


````````````markdown
*foo [bar* baz]

````````````

Gives this correct output:


````````````html
<p><em>foo [bar</em> baz]</p>

````````````

### [Example 524](https://spec.commonmark.org/0.30/#example-524)

This markdown:


````````````markdown
[foo`](/uri)`

````````````

Gives this correct output:


````````````html
<p>[foo<code>](/uri)</code></p>

````````````

### [Example 525](https://spec.commonmark.org/0.30/#example-525)

This markdown:


````````````markdown
[foo<http://example.com/?search=](uri)>

````````````

Gives this correct output:


````````````html
<p>[foo<a href="http://example.com/?search=%5D(uri)">http://example.com/?search=](uri)</a></p>

````````````

### [Example 526](https://spec.commonmark.org/0.30/#example-526)

This markdown:


````````````markdown
[foo][bar]

[bar]: /url "title"

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title">foo</a></p>

````````````

### [Example 527](https://spec.commonmark.org/0.30/#example-527)

This markdown:


````````````markdown
[link [foo [bar]]][ref]

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p><a href="/uri">link [foo [bar]]</a></p>

````````````

### [Example 528](https://spec.commonmark.org/0.30/#example-528)

This markdown:


````````````markdown
[link \[bar][ref]

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p><a href="/uri">link [bar</a></p>

````````````

### [Example 529](https://spec.commonmark.org/0.30/#example-529)

This markdown:


````````````markdown
[link *foo **bar** `#`*][ref]

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>

````````````

### [Example 530](https://spec.commonmark.org/0.30/#example-530)

This markdown:


````````````markdown
[![moon](moon.jpg)][ref]

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>

````````````

### [Example 531](https://spec.commonmark.org/0.30/#example-531)

This markdown:


````````````markdown
[foo [bar](/uri)][ref]

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p>[foo <a href="/uri">bar</a>]<a href="/uri">ref</a></p>

````````````

### [Example 532](https://spec.commonmark.org/0.30/#example-532)

This markdown:


````````````markdown
[foo *bar [baz][ref]*][ref]

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p>[foo <em>bar <a href="/uri">baz</a></em>]<a href="/uri">ref</a></p>

````````````

### [Example 533](https://spec.commonmark.org/0.30/#example-533)

This markdown:


````````````markdown
*[foo*][ref]

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p>*<a href="/uri">foo*</a></p>

````````````

### [Example 534](https://spec.commonmark.org/0.30/#example-534)

This markdown:


````````````markdown
[foo *bar][ref]*

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p><a href="/uri">foo *bar</a>*</p>

````````````

### [Example 536](https://spec.commonmark.org/0.30/#example-536)

This markdown:


````````````markdown
[foo`][ref]`

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p>[foo<code>][ref]</code></p>

````````````

### [Example 537](https://spec.commonmark.org/0.30/#example-537)

This markdown:


````````````markdown
[foo<http://example.com/?search=][ref]>

[ref]: /uri

````````````

Gives this correct output:


````````````html
<p>[foo<a href="http://example.com/?search=%5D%5Bref%5D">http://example.com/?search=][ref]</a></p>

````````````

### [Example 538](https://spec.commonmark.org/0.30/#example-538)

This markdown:


````````````markdown
[foo][BaR]

[bar]: /url "title"

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title">foo</a></p>

````````````

### [Example 541](https://spec.commonmark.org/0.30/#example-541)

This markdown:


````````````markdown
[foo] [bar]

[bar]: /url "title"

````````````

Gives this correct output:


````````````html
<p>[foo] <a href="/url" title="title">bar</a></p>

````````````

### [Example 542](https://spec.commonmark.org/0.30/#example-542)

This markdown:


````````````markdown
[foo]
[bar]

[bar]: /url "title"

````````````

Gives this correct output:


````````````html
<p>[foo]
<a href="/url" title="title">bar</a></p>

````````````

### [Example 543](https://spec.commonmark.org/0.30/#example-543)

This markdown:


````````````markdown
[foo]: /url1

[foo]: /url2

[bar][foo]

````````````

Gives this correct output:


````````````html
<p><a href="/url1">bar</a></p>

````````````

### [Example 544](https://spec.commonmark.org/0.30/#example-544)

This markdown:


````````````markdown
[bar][foo\!]

[foo!]: /url

````````````

Gives this correct output:


````````````html
<p>[bar][foo!]</p>

````````````

### [Example 546](https://spec.commonmark.org/0.30/#example-546)

This markdown:


````````````markdown
[foo][ref[bar]]

[ref[bar]]: /uri

````````````

Gives this correct output:


````````````html
<p>[foo][ref[bar]]</p>
<p>[ref[bar]]: /uri</p>

````````````

### [Example 547](https://spec.commonmark.org/0.30/#example-547)

This markdown:


````````````markdown
[[[foo]]]

[[[foo]]]: /url

````````````

Gives this correct output:


````````````html
<p>[[[foo]]]</p>
<p>[[[foo]]]: /url</p>

````````````

### [Example 548](https://spec.commonmark.org/0.30/#example-548)

This markdown:


````````````markdown
[foo][ref\[]

[ref\[]: /uri

````````````

Gives this correct output:


````````````html
<p><a href="/uri">foo</a></p>

````````````

### [Example 549](https://spec.commonmark.org/0.30/#example-549)

This markdown:


````````````markdown
[bar\\]: /uri

[bar\\]

````````````

Gives this correct output:


````````````html
<p><a href="/uri">bar\</a></p>

````````````

### [Example 552](https://spec.commonmark.org/0.30/#example-552)

This markdown:


````````````markdown
[foo][]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title">foo</a></p>

````````````

### [Example 553](https://spec.commonmark.org/0.30/#example-553)

This markdown:


````````````markdown
[*foo* bar][]

[*foo* bar]: /url "title"

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title"><em>foo</em> bar</a></p>

````````````

### [Example 554](https://spec.commonmark.org/0.30/#example-554)

This markdown:


````````````markdown
[Foo][]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title">Foo</a></p>

````````````

### [Example 555](https://spec.commonmark.org/0.30/#example-555)

This markdown:


````````````markdown
[foo] 
[]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title">foo</a>
[]</p>

````````````

### [Example 556](https://spec.commonmark.org/0.30/#example-556)

This markdown:


````````````markdown
[foo]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title">foo</a></p>

````````````

### [Example 557](https://spec.commonmark.org/0.30/#example-557)

This markdown:


````````````markdown
[*foo* bar]

[*foo* bar]: /url "title"

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title"><em>foo</em> bar</a></p>

````````````

### [Example 558](https://spec.commonmark.org/0.30/#example-558)

This markdown:


````````````markdown
[[*foo* bar]]

[*foo* bar]: /url "title"

````````````

Gives this correct output:


````````````html
<p>[<a href="/url" title="title"><em>foo</em> bar</a>]</p>

````````````

### [Example 559](https://spec.commonmark.org/0.30/#example-559)

This markdown:


````````````markdown
[[bar [foo]

[foo]: /url

````````````

Gives this correct output:


````````````html
<p>[[bar <a href="/url">foo</a></p>

````````````

### [Example 560](https://spec.commonmark.org/0.30/#example-560)

This markdown:


````````````markdown
[Foo]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p><a href="/url" title="title">Foo</a></p>

````````````

### [Example 561](https://spec.commonmark.org/0.30/#example-561)

This markdown:


````````````markdown
[foo] bar

[foo]: /url

````````````

Gives this correct output:


````````````html
<p><a href="/url">foo</a> bar</p>

````````````

### [Example 562](https://spec.commonmark.org/0.30/#example-562)

This markdown:


````````````markdown
\[foo]

[foo]: /url "title"

````````````

Gives this correct output:


````````````html
<p>[foo]</p>

````````````

### [Example 563](https://spec.commonmark.org/0.30/#example-563)

This markdown:


````````````markdown
[foo*]: /url

*[foo*]

````````````

Gives this correct output:


````````````html
<p>*<a href="/url">foo*</a></p>

````````````

### [Example 564](https://spec.commonmark.org/0.30/#example-564)

This markdown:


````````````markdown
[foo][bar]

[foo]: /url1
[bar]: /url2

````````````

Gives this correct output:


````````````html
<p><a href="/url2">foo</a></p>

````````````

### [Example 565](https://spec.commonmark.org/0.30/#example-565)

This markdown:


````````````markdown
[foo][]

[foo]: /url1

````````````

Gives this correct output:


````````````html
<p><a href="/url1">foo</a></p>

````````````

### [Example 566](https://spec.commonmark.org/0.30/#example-566)

This markdown:


````````````markdown
[foo]()

[foo]: /url1

````````````

Gives this correct output:


````````````html
<p><a href="">foo</a></p>

````````````

### [Example 567](https://spec.commonmark.org/0.30/#example-567)

This markdown:


````````````markdown
[foo](not a link)

[foo]: /url1

````````````

Gives this correct output:


````````````html
<p><a href="/url1">foo</a>(not a link)</p>

````````````

### [Example 568](https://spec.commonmark.org/0.30/#example-568)

This markdown:


````````````markdown
[foo][bar][baz]

[baz]: /url

````````````

Gives this correct output:


````````````html
<p>[foo]<a href="/url">bar</a></p>

````````````

### [Example 569](https://spec.commonmark.org/0.30/#example-569)

This markdown:


````````````markdown
[foo][bar][baz]

[baz]: /url1
[bar]: /url2

````````````

Gives this correct output:


````````````html
<p><a href="/url2">foo</a><a href="/url1">baz</a></p>

````````````

### [Example 570](https://spec.commonmark.org/0.30/#example-570)

This markdown:


````````````markdown
[foo][bar][baz]

[baz]: /url1
[foo]: /url2

````````````

Gives this correct output:


````````````html
<p>[foo]<a href="/url1">bar</a></p>

````````````

## List items

### [Example 253](https://spec.commonmark.org/0.30/#example-253)

This markdown:


````````````markdown
A paragraph
with two lines.

    indented code

> A block quote.

````````````

Gives this correct output:


````````````html
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>

````````````

### [Example 254](https://spec.commonmark.org/0.30/#example-254)

This markdown:


````````````markdown
1.  A paragraph
    with two lines.

        indented code

    > A block quote.

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 255](https://spec.commonmark.org/0.30/#example-255)

This markdown:


````````````markdown
- one

 two

````````````

Gives this correct output:


````````````html
<ul>
<li>one</li>
</ul>
<p>two</p>

````````````

### [Example 256](https://spec.commonmark.org/0.30/#example-256)

This markdown:


````````````markdown
- one

  two

````````````

Gives this correct output:


````````````html
<ul>
<li>
<p>one</p>
<p>two</p>
</li>
</ul>

````````````

### [Example 257](https://spec.commonmark.org/0.30/#example-257)

This markdown:


````````````markdown
 -    one

     two

````````````

Gives this correct output:


````````````html
<ul>
<li>one</li>
</ul>
<pre><code> two
</code></pre>

````````````

### [Example 258](https://spec.commonmark.org/0.30/#example-258)

This markdown:


````````````markdown
 -    one

      two

````````````

Gives this correct output:


````````````html
<ul>
<li>
<p>one</p>
<p>two</p>
</li>
</ul>

````````````

### [Example 259](https://spec.commonmark.org/0.30/#example-259)

This markdown:


````````````markdown
   > > 1.  one
>>
>>     two

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 260](https://spec.commonmark.org/0.30/#example-260)

This markdown:


````````````markdown
>>- one
>>
  >  > two

````````````

Gives this correct output:


````````````html
<blockquote>
<blockquote>
<ul>
<li>one</li>
</ul>
<p>two</p>
</blockquote>
</blockquote>

````````````

### [Example 261](https://spec.commonmark.org/0.30/#example-261)

This markdown:


````````````markdown
-one

2.two

````````````

Gives this correct output:


````````````html
<p>-one</p>
<p>2.two</p>

````````````

### [Example 262](https://spec.commonmark.org/0.30/#example-262)

This markdown:


````````````markdown
- foo


  bar

````````````

Gives this correct output:


````````````html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

````````````

### [Example 263](https://spec.commonmark.org/0.30/#example-263)

This markdown:


````````````markdown
1.  foo

    ```
    bar
    ```

    baz

    > bam

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 264](https://spec.commonmark.org/0.30/#example-264)

This markdown:


````````````markdown
- Foo

      bar


      baz

````````````

Gives this correct output:


````````````html
<ul>
<li>
<p>Foo</p>
<pre><code>bar


baz
</code></pre>
</li>
</ul>

````````````

### [Example 265](https://spec.commonmark.org/0.30/#example-265)

This markdown:


````````````markdown
123456789. ok

````````````

Gives this correct output:


````````````html
<ol start="123456789">
<li>ok</li>
</ol>

````````````

### [Example 266](https://spec.commonmark.org/0.30/#example-266)

This markdown:


````````````markdown
1234567890. not ok

````````````

Gives this correct output:


````````````html
<p>1234567890. not ok</p>

````````````

### [Example 267](https://spec.commonmark.org/0.30/#example-267)

This markdown:


````````````markdown
0. ok

````````````

Gives this correct output:


````````````html
<ol start="0">
<li>ok</li>
</ol>

````````````

### [Example 268](https://spec.commonmark.org/0.30/#example-268)

This markdown:


````````````markdown
003. ok

````````````

Gives this correct output:


````````````html
<ol start="3">
<li>ok</li>
</ol>

````````````

### [Example 269](https://spec.commonmark.org/0.30/#example-269)

This markdown:


````````````markdown
-1. not ok

````````````

Gives this correct output:


````````````html
<p>-1. not ok</p>

````````````

### [Example 270](https://spec.commonmark.org/0.30/#example-270)

This markdown:


````````````markdown
- foo

      bar

````````````

Gives this correct output:


````````````html
<ul>
<li>
<p>foo</p>
<pre><code>bar
</code></pre>
</li>
</ul>

````````````

### [Example 271](https://spec.commonmark.org/0.30/#example-271)

This markdown:


````````````markdown
  10.  foo

           bar

````````````

Gives this correct output:


````````````html
<ol start="10">
<li>
<p>foo</p>
<pre><code>bar
</code></pre>
</li>
</ol>

````````````

### [Example 272](https://spec.commonmark.org/0.30/#example-272)

This markdown:


````````````markdown
    indented code

paragraph

    more code

````````````

Gives this correct output:


````````````html
<pre><code>indented code
</code></pre>
<p>paragraph</p>
<pre><code>more code
</code></pre>

````````````

### [Example 273](https://spec.commonmark.org/0.30/#example-273)

This markdown:


````````````markdown
1.     indented code

   paragraph

       more code

````````````

Gives this correct output:


````````````html
<ol>
<li>
<pre><code>indented code
</code></pre>
<p>paragraph</p>
<pre><code>more code
</code></pre>
</li>
</ol>

````````````

### [Example 274](https://spec.commonmark.org/0.30/#example-274)

This markdown:


````````````markdown
1.      indented code

   paragraph

       more code

````````````

Gives this correct output:


````````````html
<ol>
<li>
<pre><code> indented code
</code></pre>
<p>paragraph</p>
<pre><code>more code
</code></pre>
</li>
</ol>

````````````

### [Example 275](https://spec.commonmark.org/0.30/#example-275)

This markdown:


````````````markdown
   foo

bar

````````````

Gives this correct output:


````````````html
<p>foo</p>
<p>bar</p>

````````````

### [Example 276](https://spec.commonmark.org/0.30/#example-276)

This markdown:


````````````markdown
-    foo

  bar

````````````

Gives this correct output:


````````````html
<ul>
<li>foo</li>
</ul>
<p>bar</p>

````````````

### [Example 277](https://spec.commonmark.org/0.30/#example-277)

This markdown:


````````````markdown
-  foo

   bar

````````````

Gives this correct output:


````````````html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

````````````

### [Example 278](https://spec.commonmark.org/0.30/#example-278)

This markdown:


````````````markdown
-
  foo
-
  ```
  bar
  ```
-
      baz

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 279](https://spec.commonmark.org/0.30/#example-279)

This markdown:


````````````markdown
-   
  foo

````````````

Gives this correct output:


````````````html
<ul>
<li>foo</li>
</ul>

````````````

### [Example 280](https://spec.commonmark.org/0.30/#example-280)

This markdown:


````````````markdown
-

  foo

````````````

Gives this correct output:


````````````html
<ul>
<li></li>
</ul>
<p>foo</p>

````````````

### [Example 281](https://spec.commonmark.org/0.30/#example-281)

This markdown:


````````````markdown
- foo
-
- bar

````````````

Gives this correct output:


````````````html
<ul>
<li>foo</li>
<li></li>
<li>bar</li>
</ul>

````````````

### [Example 282](https://spec.commonmark.org/0.30/#example-282)

This markdown:


````````````markdown
- foo
-   
- bar

````````````

Gives this correct output:


````````````html
<ul>
<li>foo</li>
<li></li>
<li>bar</li>
</ul>

````````````

### [Example 283](https://spec.commonmark.org/0.30/#example-283)

This markdown:


````````````markdown
1. foo
2.
3. bar

````````````

Gives this correct output:


````````````html
<ol>
<li>foo</li>
<li></li>
<li>bar</li>
</ol>

````````````

### [Example 284](https://spec.commonmark.org/0.30/#example-284)

This markdown:


````````````markdown
*

````````````

Gives this correct output:


````````````html
<ul>
<li></li>
</ul>

````````````

### [Example 285](https://spec.commonmark.org/0.30/#example-285)

This markdown:


````````````markdown
foo
*

foo
1.

````````````

Gives this correct output:


````````````html
<p>foo
*</p>
<p>foo
1.</p>

````````````

### [Example 286](https://spec.commonmark.org/0.30/#example-286)

This markdown:


````````````markdown
 1.  A paragraph
     with two lines.

         indented code

     > A block quote.

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 287](https://spec.commonmark.org/0.30/#example-287)

This markdown:


````````````markdown
  1.  A paragraph
      with two lines.

          indented code

      > A block quote.

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 288](https://spec.commonmark.org/0.30/#example-288)

This markdown:


````````````markdown
   1.  A paragraph
       with two lines.

           indented code

       > A block quote.

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 289](https://spec.commonmark.org/0.30/#example-289)

This markdown:


````````````markdown
    1.  A paragraph
        with two lines.

            indented code

        > A block quote.

````````````

Gives this correct output:


````````````html
<pre><code>1.  A paragraph
    with two lines.

        indented code

    &gt; A block quote.
</code></pre>

````````````

### [Example 290](https://spec.commonmark.org/0.30/#example-290)

This markdown:


````````````markdown
  1.  A paragraph
with two lines.

          indented code

      > A block quote.

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 291](https://spec.commonmark.org/0.30/#example-291)

This markdown:


````````````markdown
  1.  A paragraph
    with two lines.

````````````

Gives this correct output:


````````````html
<ol>
<li>A paragraph
with two lines.</li>
</ol>

````````````

### [Example 292](https://spec.commonmark.org/0.30/#example-292)

This markdown:


````````````markdown
> 1. > Blockquote
continued here.

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 293](https://spec.commonmark.org/0.30/#example-293)

This markdown:


````````````markdown
> 1. > Blockquote
> continued here.

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 294](https://spec.commonmark.org/0.30/#example-294)

This markdown:


````````````markdown
- foo
  - bar
    - baz
      - boo

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 295](https://spec.commonmark.org/0.30/#example-295)

This markdown:


````````````markdown
- foo
 - bar
  - baz
   - boo

````````````

Gives this correct output:


````````````html
<ul>
<li>foo</li>
<li>bar</li>
<li>baz</li>
<li>boo</li>
</ul>

````````````

### [Example 296](https://spec.commonmark.org/0.30/#example-296)

This markdown:


````````````markdown
10) foo
    - bar

````````````

Gives this correct output:


````````````html
<ol start="10">
<li>foo
<ul>
<li>bar</li>
</ul>
</li>
</ol>

````````````

### [Example 297](https://spec.commonmark.org/0.30/#example-297)

This markdown:


````````````markdown
10) foo
   - bar

````````````

Gives this correct output:


````````````html
<ol start="10">
<li>foo</li>
</ol>
<ul>
<li>bar</li>
</ul>

````````````

### [Example 298](https://spec.commonmark.org/0.30/#example-298)

This markdown:


````````````markdown
- - foo

````````````

Gives this correct output:


````````````html
<ul>
<li>
<ul>
<li>foo</li>
</ul>
</li>
</ul>

````````````

### [Example 299](https://spec.commonmark.org/0.30/#example-299)

This markdown:


````````````markdown
1. - 2. foo

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 300](https://spec.commonmark.org/0.30/#example-300)

This markdown:


````````````markdown
- # Foo
- Bar
  ---
  baz

````````````

Gives this correct output:


````````````html
<ul>
<li>
<h1>Foo</h1>
</li>
<li>
<h2>Bar</h2>
baz</li>
</ul>

````````````

## Lists

### [Example 301](https://spec.commonmark.org/0.30/#example-301)

This markdown:


````````````markdown
- foo
- bar
+ baz

````````````

Gives this correct output:


````````````html
<ul>
<li>foo</li>
<li>bar</li>
</ul>
<ul>
<li>baz</li>
</ul>

````````````

### [Example 302](https://spec.commonmark.org/0.30/#example-302)

This markdown:


````````````markdown
1. foo
2. bar
3) baz

````````````

Gives this correct output:


````````````html
<ol>
<li>foo</li>
<li>bar</li>
</ol>
<ol start="3">
<li>baz</li>
</ol>

````````````

### [Example 303](https://spec.commonmark.org/0.30/#example-303)

This markdown:


````````````markdown
Foo
- bar
- baz

````````````

Gives this correct output:


````````````html
<p>Foo</p>
<ul>
<li>bar</li>
<li>baz</li>
</ul>

````````````

### [Example 304](https://spec.commonmark.org/0.30/#example-304)

This markdown:


````````````markdown
The number of windows in my house is
14.  The number of doors is 6.

````````````

Gives this correct output:


````````````html
<p>The number of windows in my house is
14.  The number of doors is 6.</p>

````````````

### [Example 305](https://spec.commonmark.org/0.30/#example-305)

This markdown:


````````````markdown
The number of windows in my house is
1.  The number of doors is 6.

````````````

Gives this correct output:


````````````html
<p>The number of windows in my house is</p>
<ol>
<li>The number of doors is 6.</li>
</ol>

````````````

### [Example 306](https://spec.commonmark.org/0.30/#example-306)

This markdown:


````````````markdown
- foo

- bar


- baz

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 307](https://spec.commonmark.org/0.30/#example-307)

This markdown:


````````````markdown
- foo
  - bar
    - baz


      bim

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 308](https://spec.commonmark.org/0.30/#example-308)

This markdown:


````````````markdown
- foo
- bar

<!-- -->

- baz
- bim

````````````

Gives this correct output:


````````````html
<ul>
<li>foo</li>
<li>bar</li>
</ul>
<!-- -->
<ul>
<li>baz</li>
<li>bim</li>
</ul>

````````````

### [Example 309](https://spec.commonmark.org/0.30/#example-309)

This markdown:


````````````markdown
-   foo

    notcode

-   foo

<!-- -->

    code

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 310](https://spec.commonmark.org/0.30/#example-310)

This markdown:


````````````markdown
- a
 - b
  - c
   - d
  - e
 - f
- g

````````````

Gives this correct output:


````````````html
<ul>
<li>a</li>
<li>b</li>
<li>c</li>
<li>d</li>
<li>e</li>
<li>f</li>
<li>g</li>
</ul>

````````````

### [Example 311](https://spec.commonmark.org/0.30/#example-311)

This markdown:


````````````markdown
1. a

  2. b

   3. c

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 312](https://spec.commonmark.org/0.30/#example-312)

This markdown:


````````````markdown
- a
 - b
  - c
   - d
    - e

````````````

Gives this correct output:


````````````html
<ul>
<li>a</li>
<li>b</li>
<li>c</li>
<li>d
- e</li>
</ul>

````````````

### [Example 313](https://spec.commonmark.org/0.30/#example-313)

This markdown:


````````````markdown
1. a

  2. b

    3. c

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 314](https://spec.commonmark.org/0.30/#example-314)

This markdown:


````````````markdown
- a
- b

- c

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 315](https://spec.commonmark.org/0.30/#example-315)

This markdown:


````````````markdown
* a
*

* c

````````````

Gives this correct output:


````````````html
<ul>
<li>
<p>a</p>
</li>
<li></li>
<li>
<p>c</p>
</li>
</ul>

````````````

### [Example 316](https://spec.commonmark.org/0.30/#example-316)

This markdown:


````````````markdown
- a
- b

  c
- d

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 317](https://spec.commonmark.org/0.30/#example-317)

This markdown:


````````````markdown
- a
- b

  [ref]: /url
- d

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 318](https://spec.commonmark.org/0.30/#example-318)

This markdown:


````````````markdown
- a
- ```
  b


  ```
- c

````````````

Gives this correct output:


````````````html
<ul>
<li>a</li>
<li>
<pre><code>b


</code></pre>
</li>
<li>c</li>
</ul>

````````````

### [Example 319](https://spec.commonmark.org/0.30/#example-319)

This markdown:


````````````markdown
- a
  - b

    c
- d

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 320](https://spec.commonmark.org/0.30/#example-320)

This markdown:


````````````markdown
* a
  > b
  >
* c

````````````

Gives this correct output:


````````````html
<ul>
<li>a
<blockquote>
<p>b</p>
</blockquote>
</li>
<li>c</li>
</ul>

````````````

### [Example 321](https://spec.commonmark.org/0.30/#example-321)

This markdown:


````````````markdown
- a
  > b
  ```
  c
  ```
- d

````````````

Gives this correct output:


````````````html
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

````````````

### [Example 322](https://spec.commonmark.org/0.30/#example-322)

This markdown:


````````````markdown
- a

````````````

Gives this correct output:


````````````html
<ul>
<li>a</li>
</ul>

````````````

### [Example 323](https://spec.commonmark.org/0.30/#example-323)

This markdown:


````````````markdown
- a
  - b

````````````

Gives this correct output:


````````````html
<ul>
<li>a
<ul>
<li>b</li>
</ul>
</li>
</ul>

````````````

### [Example 324](https://spec.commonmark.org/0.30/#example-324)

This markdown:


````````````markdown
1. ```
   foo
   ```

   bar

````````````

Gives this correct output:


````````````html
<ol>
<li>
<pre><code>foo
</code></pre>
<p>bar</p>
</li>
</ol>

````````````

### [Example 325](https://spec.commonmark.org/0.30/#example-325)

This markdown:


````````````markdown
* foo
  * bar

  baz

````````````

Gives this correct output:


````````````html
<ul>
<li>
<p>foo</p>
<ul>
<li>bar</li>
</ul>
<p>baz</p>
</li>
</ul>

````````````

### [Example 326](https://spec.commonmark.org/0.30/#example-326)

This markdown:


````````````markdown
- a
  - b
  - c

- d
  - e
  - f

````````````

Gives this correct output:


````````````html
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

````````````

## Paragraphs

### [Example 219](https://spec.commonmark.org/0.30/#example-219)

This markdown:


````````````markdown
aaa

bbb

````````````

Gives this correct output:


````````````html
<p>aaa</p>
<p>bbb</p>

````````````

### [Example 220](https://spec.commonmark.org/0.30/#example-220)

This markdown:


````````````markdown
aaa
bbb

ccc
ddd

````````````

Gives this correct output:


````````````html
<p>aaa
bbb</p>
<p>ccc
ddd</p>

````````````

### [Example 221](https://spec.commonmark.org/0.30/#example-221)

This markdown:


````````````markdown
aaa


bbb

````````````

Gives this correct output:


````````````html
<p>aaa</p>
<p>bbb</p>

````````````

### [Example 222](https://spec.commonmark.org/0.30/#example-222)

This markdown:


````````````markdown
  aaa
 bbb

````````````

Gives this correct output:


````````````html
<p>aaa
bbb</p>

````````````

### [Example 223](https://spec.commonmark.org/0.30/#example-223)

This markdown:


````````````markdown
aaa
             bbb
                                       ccc

````````````

Gives this correct output:


````````````html
<p>aaa
bbb
ccc</p>

````````````

### [Example 224](https://spec.commonmark.org/0.30/#example-224)

This markdown:


````````````markdown
   aaa
bbb

````````````

Gives this correct output:


````````````html
<p>aaa
bbb</p>

````````````

### [Example 225](https://spec.commonmark.org/0.30/#example-225)

This markdown:


````````````markdown
    aaa
bbb

````````````

Gives this correct output:


````````````html
<pre><code>aaa
</code></pre>
<p>bbb</p>

````````````

### [Example 226](https://spec.commonmark.org/0.30/#example-226)

This markdown:


````````````markdown
aaa     
bbb     

````````````

Gives this correct output:


````````````html
<p>aaa<br />
bbb</p>

````````````

## Precedence

### [Example 42](https://spec.commonmark.org/0.30/#example-42)

This markdown:


````````````markdown
- `one
- two`

````````````

Gives this correct output:


````````````html
<ul>
<li>`one</li>
<li>two`</li>
</ul>

````````````

## Raw HTML

### [Example 624](https://spec.commonmark.org/0.30/#example-624)

This markdown:


````````````markdown
foo <!-- this is a
comment - with hyphen -->

````````````

Gives this correct output:


````````````html
<p>foo <!-- this is a
comment - with hyphen --></p>

````````````

### [Example 627](https://spec.commonmark.org/0.30/#example-627)

This markdown:


````````````markdown
foo <?php echo $a; ?>

````````````

Gives this correct output:


````````````html
<p>foo <?php echo $a; ?></p>

````````````

### [Example 628](https://spec.commonmark.org/0.30/#example-628)

This markdown:


````````````markdown
foo <!ELEMENT br EMPTY>

````````````

Gives this correct output:


````````````html
<p>foo <!ELEMENT br EMPTY></p>

````````````

## Setext headings

### [Example 80](https://spec.commonmark.org/0.30/#example-80)

This markdown:


````````````markdown
Foo *bar*
=========

Foo *bar*
---------

````````````

Gives this correct output:


````````````html
<h1>Foo <em>bar</em></h1>
<h2>Foo <em>bar</em></h2>

````````````

### [Example 81](https://spec.commonmark.org/0.30/#example-81)

This markdown:


````````````markdown
Foo *bar
baz*
====

````````````

Gives this correct output:


````````````html
<h1>Foo <em>bar
baz</em></h1>

````````````

### [Example 82](https://spec.commonmark.org/0.30/#example-82)

This markdown:


````````````markdown
  Foo *bar
baz*	
====

````````````

Gives this correct output:


````````````html
<h1>Foo <em>bar
baz</em></h1>

````````````

### [Example 83](https://spec.commonmark.org/0.30/#example-83)

This markdown:


````````````markdown
Foo
-------------------------

Foo
=

````````````

Gives this correct output:


````````````html
<h2>Foo</h2>
<h1>Foo</h1>

````````````

### [Example 84](https://spec.commonmark.org/0.30/#example-84)

This markdown:


````````````markdown
   Foo
---

  Foo
-----

  Foo
  ===

````````````

Gives this correct output:


````````````html
<h2>Foo</h2>
<h2>Foo</h2>
<h1>Foo</h1>

````````````

### [Example 85](https://spec.commonmark.org/0.30/#example-85)

This markdown:


````````````markdown
    Foo
    ---

    Foo
---

````````````

Gives this correct output:


````````````html
<pre><code>Foo
---

Foo
</code></pre>
<hr />

````````````

### [Example 86](https://spec.commonmark.org/0.30/#example-86)

This markdown:


````````````markdown
Foo
   ----      

````````````

Gives this correct output:


````````````html
<h2>Foo</h2>

````````````

### [Example 87](https://spec.commonmark.org/0.30/#example-87)

This markdown:


````````````markdown
Foo
    ---

````````````

Gives this correct output:


````````````html
<p>Foo
---</p>

````````````

### [Example 88](https://spec.commonmark.org/0.30/#example-88)

This markdown:


````````````markdown
Foo
= =

Foo
--- -

````````````

Gives this correct output:


````````````html
<p>Foo
= =</p>
<p>Foo</p>
<hr />

````````````

### [Example 89](https://spec.commonmark.org/0.30/#example-89)

This markdown:


````````````markdown
Foo  
-----

````````````

Gives this correct output:


````````````html
<h2>Foo</h2>

````````````

### [Example 90](https://spec.commonmark.org/0.30/#example-90)

This markdown:


````````````markdown
Foo\
----

````````````

Gives this correct output:


````````````html
<h2>Foo\</h2>

````````````

### [Example 92](https://spec.commonmark.org/0.30/#example-92)

This markdown:


````````````markdown
> Foo
---

````````````

Gives this correct output:


````````````html
<blockquote>
<p>Foo</p>
</blockquote>
<hr />

````````````

### [Example 94](https://spec.commonmark.org/0.30/#example-94)

This markdown:


````````````markdown
- Foo
---

````````````

Gives this correct output:


````````````html
<ul>
<li>Foo</li>
</ul>
<hr />

````````````

### [Example 95](https://spec.commonmark.org/0.30/#example-95)

This markdown:


````````````markdown
Foo
Bar
---

````````````

Gives this correct output:


````````````html
<h2>Foo
Bar</h2>

````````````

### [Example 96](https://spec.commonmark.org/0.30/#example-96)

This markdown:


````````````markdown
---
Foo
---
Bar
---
Baz

````````````

Gives this correct output:


````````````html
<hr />
<h2>Foo</h2>
<h2>Bar</h2>
<p>Baz</p>

````````````

### [Example 97](https://spec.commonmark.org/0.30/#example-97)

This markdown:


````````````markdown

====

````````````

Gives this correct output:


````````````html
<p>====</p>

````````````

### [Example 98](https://spec.commonmark.org/0.30/#example-98)

This markdown:


````````````markdown
---
---

````````````

Gives this correct output:


````````````html
<hr />
<hr />

````````````

### [Example 99](https://spec.commonmark.org/0.30/#example-99)

This markdown:


````````````markdown
- foo
-----

````````````

Gives this correct output:


````````````html
<ul>
<li>foo</li>
</ul>
<hr />

````````````

### [Example 100](https://spec.commonmark.org/0.30/#example-100)

This markdown:


````````````markdown
    foo
---

````````````

Gives this correct output:


````````````html
<pre><code>foo
</code></pre>
<hr />

````````````

### [Example 101](https://spec.commonmark.org/0.30/#example-101)

This markdown:


````````````markdown
> foo
-----

````````````

Gives this correct output:


````````````html
<blockquote>
<p>foo</p>
</blockquote>
<hr />

````````````

### [Example 102](https://spec.commonmark.org/0.30/#example-102)

This markdown:


````````````markdown
\> foo
------

````````````

Gives this correct output:


````````````html
<h2>&gt; foo</h2>

````````````

### [Example 103](https://spec.commonmark.org/0.30/#example-103)

This markdown:


````````````markdown
Foo

bar
---
baz

````````````

Gives this correct output:


````````````html
<p>Foo</p>
<h2>bar</h2>
<p>baz</p>

````````````

### [Example 104](https://spec.commonmark.org/0.30/#example-104)

This markdown:


````````````markdown
Foo
bar

---

baz

````````````

Gives this correct output:


````````````html
<p>Foo
bar</p>
<hr />
<p>baz</p>

````````````

### [Example 105](https://spec.commonmark.org/0.30/#example-105)

This markdown:


````````````markdown
Foo
bar
* * *
baz

````````````

Gives this correct output:


````````````html
<p>Foo
bar</p>
<hr />
<p>baz</p>

````````````

### [Example 106](https://spec.commonmark.org/0.30/#example-106)

This markdown:


````````````markdown
Foo
bar
\---
baz

````````````

Gives this correct output:


````````````html
<p>Foo
bar
---
baz</p>

````````````

## Soft line breaks

### [Example 648](https://spec.commonmark.org/0.30/#example-648)

This markdown:


````````````markdown
foo
baz

````````````

Gives this correct output:


````````````html
<p>foo
baz</p>

````````````

### [Example 649](https://spec.commonmark.org/0.30/#example-649)

This markdown:


````````````markdown
foo 
 baz

````````````

Gives this correct output:


````````````html
<p>foo
baz</p>

````````````

## Tabs

### [Example 1](https://spec.commonmark.org/0.30/#example-1)

This markdown:


````````````markdown
	foo	baz		bim

````````````

Gives this correct output:


````````````html
<pre><code>foo	baz		bim
</code></pre>

````````````

### [Example 2](https://spec.commonmark.org/0.30/#example-2)

This markdown:


````````````markdown
  	foo	baz		bim

````````````

Gives this correct output:


````````````html
<pre><code>foo	baz		bim
</code></pre>

````````````

### [Example 3](https://spec.commonmark.org/0.30/#example-3)

This markdown:


````````````markdown
    a	a
    ὐ	a

````````````

Gives this correct output:


````````````html
<pre><code>a	a
ὐ	a
</code></pre>

````````````

### [Example 6](https://spec.commonmark.org/0.30/#example-6)

This markdown:


````````````markdown
>		foo

````````````

Gives this correct output:


````````````html
<blockquote>
<pre><code>  foo
</code></pre>
</blockquote>

````````````

### [Example 8](https://spec.commonmark.org/0.30/#example-8)

This markdown:


````````````markdown
    foo
	bar

````````````

Gives this correct output:


````````````html
<pre><code>foo
bar
</code></pre>

````````````

### [Example 10](https://spec.commonmark.org/0.30/#example-10)

This markdown:


````````````markdown
#	Foo

````````````

Gives this correct output:


````````````html
<h1>Foo</h1>

````````````

### [Example 11](https://spec.commonmark.org/0.30/#example-11)

This markdown:


````````````markdown
*	*	*	

````````````

Gives this correct output:


````````````html
<hr />

````````````

## Textual content

### [Example 650](https://spec.commonmark.org/0.30/#example-650)

This markdown:


````````````markdown
hello $.;'there

````````````

Gives this correct output:


````````````html
<p>hello $.;'there</p>

````````````

### [Example 651](https://spec.commonmark.org/0.30/#example-651)

This markdown:


````````````markdown
Foo χρῆν

````````````

Gives this correct output:


````````````html
<p>Foo χρῆν</p>

````````````

### [Example 652](https://spec.commonmark.org/0.30/#example-652)

This markdown:


````````````markdown
Multiple     spaces

````````````

Gives this correct output:


````````````html
<p>Multiple     spaces</p>

````````````

## Thematic breaks

### [Example 43](https://spec.commonmark.org/0.30/#example-43)

This markdown:


````````````markdown
***
---
___

````````````

Gives this correct output:


````````````html
<hr />
<hr />
<hr />

````````````

### [Example 44](https://spec.commonmark.org/0.30/#example-44)

This markdown:


````````````markdown
+++

````````````

Gives this correct output:


````````````html
<p>+++</p>

````````````

### [Example 45](https://spec.commonmark.org/0.30/#example-45)

This markdown:


````````````markdown
===

````````````

Gives this correct output:


````````````html
<p>===</p>

````````````

### [Example 46](https://spec.commonmark.org/0.30/#example-46)

This markdown:


````````````markdown
--
**
__

````````````

Gives this correct output:


````````````html
<p>--
**
__</p>

````````````

### [Example 47](https://spec.commonmark.org/0.30/#example-47)

This markdown:


````````````markdown
 ***
  ***
   ***

````````````

Gives this correct output:


````````````html
<hr />
<hr />
<hr />

````````````

### [Example 48](https://spec.commonmark.org/0.30/#example-48)

This markdown:


````````````markdown
    ***

````````````

Gives this correct output:


````````````html
<pre><code>***
</code></pre>

````````````

### [Example 49](https://spec.commonmark.org/0.30/#example-49)

This markdown:


````````````markdown
Foo
    ***

````````````

Gives this correct output:


````````````html
<p>Foo
***</p>

````````````

### [Example 50](https://spec.commonmark.org/0.30/#example-50)

This markdown:


````````````markdown
_____________________________________

````````````

Gives this correct output:


````````````html
<hr />

````````````

### [Example 51](https://spec.commonmark.org/0.30/#example-51)

This markdown:


````````````markdown
 - - -

````````````

Gives this correct output:


````````````html
<hr />

````````````

### [Example 52](https://spec.commonmark.org/0.30/#example-52)

This markdown:


````````````markdown
 **  * ** * ** * **

````````````

Gives this correct output:


````````````html
<hr />

````````````

### [Example 53](https://spec.commonmark.org/0.30/#example-53)

This markdown:


````````````markdown
-     -      -      -

````````````

Gives this correct output:


````````````html
<hr />

````````````

### [Example 54](https://spec.commonmark.org/0.30/#example-54)

This markdown:


````````````markdown
- - - -    

````````````

Gives this correct output:


````````````html
<hr />

````````````

### [Example 55](https://spec.commonmark.org/0.30/#example-55)

This markdown:


````````````markdown
_ _ _ _ a

a------

---a---

````````````

Gives this correct output:


````````````html
<p>_ _ _ _ a</p>
<p>a------</p>
<p>---a---</p>

````````````

### [Example 56](https://spec.commonmark.org/0.30/#example-56)

This markdown:


````````````markdown
 *-*

````````````

Gives this correct output:


````````````html
<p><em>-</em></p>

````````````

### [Example 57](https://spec.commonmark.org/0.30/#example-57)

This markdown:


````````````markdown
- foo
***
- bar

````````````

Gives this correct output:


````````````html
<ul>
<li>foo</li>
</ul>
<hr />
<ul>
<li>bar</li>
</ul>

````````````

### [Example 58](https://spec.commonmark.org/0.30/#example-58)

This markdown:


````````````markdown
Foo
***
bar

````````````

Gives this correct output:


````````````html
<p>Foo</p>
<hr />
<p>bar</p>

````````````

### [Example 59](https://spec.commonmark.org/0.30/#example-59)

This markdown:


````````````markdown
Foo
---
bar

````````````

Gives this correct output:


````````````html
<h2>Foo</h2>
<p>bar</p>

````````````

### [Example 60](https://spec.commonmark.org/0.30/#example-60)

This markdown:


````````````markdown
* Foo
* * *
* Bar

````````````

Gives this correct output:


````````````html
<ul>
<li>Foo</li>
</ul>
<hr />
<ul>
<li>Bar</li>
</ul>

````````````

### [Example 61](https://spec.commonmark.org/0.30/#example-61)

This markdown:


````````````markdown
- Foo
- * * *

````````````

Gives this correct output:


````````````html
<ul>
<li>Foo</li>
<li>
<hr />
</li>
</ul>

````````````

## [extension] Strikethrough

### [Example 491](https://github.github.com/gfm/#example-491)

This markdown:


````````````markdown
~~Hi~~ Hello, world!
````````````

Gives this correct output:


````````````html
<p><del>Hi</del> Hello, world!</p>
````````````

### [Example 492](https://github.github.com/gfm/#example-492)

This markdown:


````````````markdown
This ~~has a

new paragraph~~.
````````````

Gives this correct output:


````````````html
<p>This ~~has a</p>
<p>new paragraph~~.</p>
````````````

## [extension] Tables

### [Example 198](https://github.github.com/gfm/#example-198)

This markdown:


````````````markdown
| foo | bar |
| --- | --- |
| baz | bim |
````````````

Gives this correct output:


````````````html
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
````````````

### [Example 199](https://github.github.com/gfm/#example-199)

This markdown:


````````````markdown
| abc | defghi |
:-: | -----------:
bar | baz
````````````

Gives this correct output:


````````````html
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
````````````

### [Example 200](https://github.github.com/gfm/#example-200)

This markdown:


````````````markdown
| f\|oo  |
| ------ |
| b `\|` az |
| b **\|** im |
````````````

Gives this correct output:


````````````html
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
````````````

### [Example 201](https://github.github.com/gfm/#example-201)

This markdown:


````````````markdown
| abc | def |
| --- | --- |
| bar | baz |
> bar
````````````

Gives this correct output:


````````````html
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
````````````

### [Example 202](https://github.github.com/gfm/#example-202)

This markdown:


````````````markdown
| abc | def |
| --- | --- |
| bar | baz |
bar

bar
````````````

Gives this correct output:


````````````html
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
````````````

### [Example 203](https://github.github.com/gfm/#example-203)

This markdown:


````````````markdown
| abc | def |
| --- |
| bar |
````````````

Gives this correct output:


````````````html
<p>| abc | def |
| --- |
| bar |</p>
````````````

### [Example 204](https://github.github.com/gfm/#example-204)

This markdown:


````````````markdown
| abc | def |
| --- | --- |
| bar |
| bar | baz | boo |
````````````

Gives this correct output:


````````````html
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
````````````

### [Example 205](https://github.github.com/gfm/#example-205)

This markdown:


````````````markdown
| abc | def |
| --- | --- |
````````````

Gives this correct output:


````````````html
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
</table>
````````````

## [extension] Task list items

### [Example 279](https://github.github.com/gfm/#example-279)

This markdown:


````````````markdown
- [ ] foo
- [x] bar
````````````

Gives this correct output:


````````````html
<ul>
<li><input disabled="" type="checkbox"> foo</li>
<li><input checked="" disabled="" type="checkbox"> bar</li>
</ul>
````````````

### [Example 280](https://github.github.com/gfm/#example-280)

This markdown:


````````````markdown
- [x] foo
  - [ ] bar
  - [x] baz
- [ ] bim
````````````

Gives this correct output:


````````````html
<ul>
<li><input checked="" disabled="" type="checkbox"> foo
<ul>
<li><input disabled="" type="checkbox"> bar</li>
<li><input checked="" disabled="" type="checkbox"> baz</li>
</ul>
</li>
<li><input disabled="" type="checkbox"> bim</li>
</ul>
````````````


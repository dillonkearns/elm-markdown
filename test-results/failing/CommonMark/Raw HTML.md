# CommonMark - Raw HTML

## [Example 612](https://spec.commonmark.org/0.30/#example-612)

This markdown:

````````````markdown
<a><bab><c2c>

````````````

Should give output:

````````````html
<p><a><bab><c2c></p>
````````````

But instead was:

````````````html
<p>&lt;a&gt;&lt;bab&gt;&lt;c2c&gt;</p>
````````````
## [Example 613](https://spec.commonmark.org/0.30/#example-613)

This markdown:

````````````markdown
<a/><b2/>

````````````

Should give output:

````````````html
<p><a><b2></p>
````````````

But instead was:

````````````html
<a></a><b2></b2>
````````````
## [Example 614](https://spec.commonmark.org/0.30/#example-614)

This markdown:

````````````markdown
<a  /><b2
data="foo" >

````````````

Should give output:

````````````html
<p><a><b2 data="foo"></p>
````````````

But instead was:

````````````html
<p><a></a>&lt;b2 data=&quot;foo&quot; &gt;</p>
````````````
## [Example 615](https://spec.commonmark.org/0.30/#example-615)

This markdown:

````````````markdown
<a foo="bar" bam = 'baz <em>"</em>'
_boolean zoop:33=zoop:33 />

````````````

Should give output:

````````````html
<p><a _boolean="" bam="baz &lt;em&gt;&quot;&lt;/em&gt;" foo="bar" zoop:33="zoop:33"></p>
````````````

But instead was:

````````````html
<p>&lt;a foo=&quot;bar&quot; bam = &#39;baz &lt;em&gt;&quot;&lt;/em&gt;&#39; _boolean zoop:33=zoop:33 /&gt;</p>
````````````
## [Example 616](https://spec.commonmark.org/0.30/#example-616)

This markdown:

````````````markdown
Foo <responsive-image src="foo.jpg" />

````````````

Should give output:

````````````html
<p>Foo<responsive-image src="foo.jpg"></p>
````````````

But instead was:

````````````html
<p>Foo<responsive-image src="foo.jpg"></responsive-image></p>
````````````
## [Example 625](https://spec.commonmark.org/0.30/#example-625)

This markdown:

````````````markdown
foo <!-- not a comment -- two hyphens -->

````````````

Should give output:

````````````html
<p>foo &lt;!-- not a comment -- two hyphens --&gt;</p>
````````````

But instead was:

````````````html
<p>foo</p>
````````````
## [Example 626](https://spec.commonmark.org/0.30/#example-626)

This markdown:

````````````markdown
foo <!--> foo -->

foo <!-- foo--->

````````````

Should give output:

````````````html
<p>foo &lt;!--&gt; foo --&gt;</p><p>foo &lt;!-- foo---&gt;</p>
````````````

But instead was:

````````````html
<p>foo</p><p>foo</p>
````````````
## [Example 629](https://spec.commonmark.org/0.30/#example-629)

This markdown:

````````````markdown
foo <![CDATA[>&<]]>

````````````

Should give output:

````````````html
<p>foo&amp;&lt;]]&gt;</p>
````````````

But instead was:

````````````html
<p>foo</p>
````````````
## [Example 630](https://spec.commonmark.org/0.30/#example-630)

This markdown:

````````````markdown
foo <a href="&ouml;">

````````````

Should give output:

````````````html
<p>foo<a href="ö"></p>
````````````

But instead was:

````````````html
<p>foo &lt;a href=&quot;ö&quot;&gt;</p>
````````````
## [Example 631](https://spec.commonmark.org/0.30/#example-631)

This markdown:

````````````markdown
foo <a href="\*">

````````````

Should give output:

````````````html
<p>foo<a href="\*"></p>
````````````

But instead was:

````````````html
<p>foo &lt;a href=&quot;*&quot;&gt;</p>
````````````

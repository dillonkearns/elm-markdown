# CommonMark - HTML blocks

## [Example 148](https://spec.commonmark.org/0.30/#example-148)

This markdown:

````````````markdown
<table><tr><td>
<pre>
**Hello**,

_world_.
</pre>
</td></tr></table>

````````````

Should give output:

````````````html
<table><tr><td><pre>**Hello**,<p><em>world</em>.</pre></p></td></tr></table>
````````````

But instead was:

````````````html
<table><tr><td><pre><p><strong>Hello</strong>,</p><p><em>world</em>.</p></pre></td></tr></table>
````````````
## [Example 149](https://spec.commonmark.org/0.30/#example-149)

This markdown:

````````````markdown
<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>

okay.

````````````

Should give output:

````````````html
<table><tr><td>hi</td></tr></table><p>okay.</p>
````````````

But instead was:

````````````html
<table><tr><pre><code></code></pre><td><pre><code>hi</code></pre></td></tr></table><p>okay.</p>
````````````
## [Example 150](https://spec.commonmark.org/0.30/#example-150)

This markdown:

````````````markdown
 <div>
  *hello*
         <foo><a>

````````````

Should give output:

````````````html
<div>*hello*<foo><a>
````````````

But instead was:

````````````html
<p>&lt;div&gt;<em>hello</em>&lt;foo&gt;&lt;a&gt;</p>
````````````
## [Example 151](https://spec.commonmark.org/0.30/#example-151)

This markdown:

````````````markdown
</div>
*foo*

````````````

Should give output:

````````````html
</div>*foo*
````````````

But instead was:

````````````html
ERROR Problem at row 1 Expecting at least 1 tag name character
````````````
## [Example 155](https://spec.commonmark.org/0.30/#example-155)

This markdown:

````````````markdown
<div>
*foo*

*bar*

````````````

Should give output:

````````````html
<div>*foo*<p><em>bar</em></p>
````````````

But instead was:

````````````html
ERROR Problem at row 5 Expecting symbol
````````````
## [Example 156](https://spec.commonmark.org/0.30/#example-156)

This markdown:

````````````markdown
<div id="foo"
*hi*

````````````

Should give output:

````````````html

````````````

But instead was:

````````````html
ERROR Problem at row 3 Expecting symbol =
````````````
## [Example 157](https://spec.commonmark.org/0.30/#example-157)

This markdown:

````````````markdown
<div class
foo

````````````

Should give output:

````````````html

````````````

But instead was:

````````````html
ERROR Problem at row 2 Expecting symbol =
````````````
## [Example 158](https://spec.commonmark.org/0.30/#example-158)

This markdown:

````````````markdown
<div *???-&&&-<---
*foo*

````````````

Should give output:

````````````html

````````````

But instead was:

````````````html
ERROR Problem at row 1 Expecting symbol =
````````````
## [Example 159](https://spec.commonmark.org/0.30/#example-159)

This markdown:

````````````markdown
<div><a href="bar">*foo*</a></div>

````````````

Should give output:

````````````html
<div><a href="bar">*foo*</a></div>
````````````

But instead was:

````````````html
<div><a href="bar"><p><em>foo</em></p></a></div>
````````````
## [Example 160](https://spec.commonmark.org/0.30/#example-160)

This markdown:

````````````markdown
<table><tr><td>
foo
</td></tr></table>

````````````

Should give output:

````````````html
<table><tr><td>foo</td></tr></table>
````````````

But instead was:

````````````html
<table><tr><td><p>foo</p></td></tr></table>
````````````
## [Example 161](https://spec.commonmark.org/0.30/#example-161)

This markdown:

````````````markdown
<div></div>
``` c
int x = 33;
```

````````````

Should give output:

````````````html
<div></div>``` c int x = 33; ```
````````````

But instead was:

````````````html
<div></div><pre><code class="language-c">int x = 33;</code></pre>
````````````
## [Example 162](https://spec.commonmark.org/0.30/#example-162)

This markdown:

````````````markdown
<a href="foo">
*bar*
</a>

````````````

Should give output:

````````````html
<a href="foo">*bar*</a>
````````````

But instead was:

````````````html
<a href="foo"><p><em>bar</em></p></a>
````````````
## [Example 163](https://spec.commonmark.org/0.30/#example-163)

This markdown:

````````````markdown
<Warning>
*bar*
</Warning>

````````````

Should give output:

````````````html
<warning>*bar*</warning>
````````````

But instead was:

````````````html
<warning><p><em>bar</em></p></warning>
````````````
## [Example 164](https://spec.commonmark.org/0.30/#example-164)

This markdown:

````````````markdown
<i class="foo">
*bar*
</i>

````````````

Should give output:

````````````html
<i class="foo">*bar*</i>
````````````

But instead was:

````````````html
<i class="foo"><p><em>bar</em></p></i>
````````````
## [Example 165](https://spec.commonmark.org/0.30/#example-165)

This markdown:

````````````markdown
</ins>
*bar*

````````````

Should give output:

````````````html
</ins>*bar*
````````````

But instead was:

````````````html
ERROR Problem at row 1 Expecting at least 1 tag name character
````````````
## [Example 166](https://spec.commonmark.org/0.30/#example-166)

This markdown:

````````````markdown
<del>
*foo*
</del>

````````````

Should give output:

````````````html
<del>*foo*</del>
````````````

But instead was:

````````````html
<del><p><em>foo</em></p></del>
````````````
## [Example 168](https://spec.commonmark.org/0.30/#example-168)

This markdown:

````````````markdown
<del>*foo*</del>

````````````

Should give output:

````````````html
<p><del><em>foo</em></del></p>
````````````

But instead was:

````````````html
<del><p><em>foo</em></p></del>
````````````
## [Example 169](https://spec.commonmark.org/0.30/#example-169)

This markdown:

````````````markdown
<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
okay

````````````

Should give output:

````````````html
<pre language="haskell"><code>import Text.HTML.TagSoup main :: IO () main = print $ parseTags tags</code></pre><p>okay</p>
````````````

But instead was:

````````````html
<pre language="haskell"><code><p>import Text.HTML.TagSoup</p><p>main :: IO () main = print $ parseTags tags</p></code></pre><p>okay</p>
````````````
## [Example 170](https://spec.commonmark.org/0.30/#example-170)

This markdown:

````````````markdown
<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
</script>
okay

````````````

Should give output:

````````````html
<script type="text/javascript">// JavaScript example document.getElementById(&quot;demo&quot;).innerHTML = &quot;Hello JavaScript!&quot;;</script><p>okay</p>
````````````

But instead was:

````````````html
<script type="text/javascript">&lt;p&gt;// JavaScript example&lt;/p&gt;&lt;p&gt;document.getElementById(&quot;demo&quot;).innerHTML = &quot;Hello JavaScript!&quot;;&lt;/p&gt;</script><p>okay</p>
````````````
## [Example 171](https://spec.commonmark.org/0.30/#example-171)

This markdown:

````````````markdown
<textarea>

*foo*

_bar_

</textarea>

````````````

Should give output:

````````````html
<textarea>*foo* _bar_</textarea>
````````````

But instead was:

````````````html
<textarea>&lt;p&gt;&lt;em&gt;foo&lt;/em&gt;&lt;/p&gt;&lt;p&gt;&lt;em&gt;bar&lt;/em&gt;&lt;/p&gt;</textarea>
````````````
## [Example 172](https://spec.commonmark.org/0.30/#example-172)

This markdown:

````````````markdown
<style
  type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
okay

````````````

Should give output:

````````````html
<style type="text/css">h1 {color:red;} p {color:blue;}</style><p>okay</p>
````````````

But instead was:

````````````html
<style type="text/css">&lt;p&gt;h1 {color:red;}&lt;/p&gt;&lt;p&gt;p {color:blue;}&lt;/p&gt;</style><p>okay</p>
````````````
## [Example 173](https://spec.commonmark.org/0.30/#example-173)

This markdown:

````````````markdown
<style
  type="text/css">

foo

````````````

Should give output:

````````````html
<style type="text/css">foo
````````````

But instead was:

````````````html
ERROR Problem at row 5 Expecting symbol
````````````
## [Example 174](https://spec.commonmark.org/0.30/#example-174)

This markdown:

````````````markdown
> <div>
> foo

bar

````````````

Should give output:

````````````html
<blockquote><div>foo</blockquote><p>bar</p>
````````````

But instead was:

````````````html
ERROR Problem at row 4 Problem at row 2 Expecting symbol
````````````
## [Example 175](https://spec.commonmark.org/0.30/#example-175)

This markdown:

````````````markdown
- <div>
- foo

````````````

Should give output:

````````````html
<ul><li><div></li><li>foo</li></ul>
````````````

But instead was:

````````````html
ERROR Problem at row 3 Problem at row 1 Expecting symbol
````````````
## [Example 176](https://spec.commonmark.org/0.30/#example-176)

This markdown:

````````````markdown
<style>p{color:red;}</style>
*foo*

````````````

Should give output:

````````````html
<style>p{color:red;}</style><p><em>foo</em></p>
````````````

But instead was:

````````````html
<style>&lt;p&gt;p{color:red;}&lt;/p&gt;</style><p><em>foo</em></p>
````````````
## [Example 177](https://spec.commonmark.org/0.30/#example-177)

This markdown:

````````````markdown
<!-- foo -->*bar*
*baz*

````````````

Should give output:

````````````html
*bar*<p><em>baz</em></p>
````````````

But instead was:

````````````html
<p><em>bar</em><em>baz</em></p>
````````````
## [Example 178](https://spec.commonmark.org/0.30/#example-178)

This markdown:

````````````markdown
<script>
foo
</script>1. *bar*

````````````

Should give output:

````````````html
<script>foo</script>1. *bar*
````````````

But instead was:

````````````html
<script>&lt;p&gt;foo&lt;/p&gt;</script><ol><li><em>bar</em></li></ol>
````````````
## [Example 180](https://spec.commonmark.org/0.30/#example-180)

This markdown:

````````````markdown
<?php

  echo '>';

?>
okay

````````````

Should give output:

````````````html
&#39;; ?&gt;<p>okay</p>
````````````

But instead was:

````````````html
<p>okay</p>
````````````
## [Example 181](https://spec.commonmark.org/0.30/#example-181)

This markdown:

````````````markdown
<!DOCTYPE html>

````````````

Should give output:

````````````html
<!DOCTYPE HTML>
````````````

But instead was:

````````````html

````````````
## [Example 183](https://spec.commonmark.org/0.30/#example-183)

This markdown:

````````````markdown
  <!-- foo -->

    <!-- foo -->

````````````

Should give output:

````````````html
<pre><code>&lt;!-- foo --&gt;</code></pre>
````````````

But instead was:

````````````html
<p></p><pre><code>&lt;!-- foo --&gt;</code></pre>
````````````
## [Example 184](https://spec.commonmark.org/0.30/#example-184)

This markdown:

````````````markdown
  <div>

    <div>

````````````

Should give output:

````````````html
<div><pre><code>&lt;div&gt;</code></pre>
````````````

But instead was:

````````````html
<p>&lt;div&gt;</p><pre><code>&lt;div&gt;</code></pre>
````````````
## [Example 185](https://spec.commonmark.org/0.30/#example-185)

This markdown:

````````````markdown
Foo
<div>
bar
</div>

````````````

Should give output:

````````````html
<p>Foo</p><div>bar</div>
````````````

But instead was:

````````````html
<p>Foo</p><div><p>bar</p></div>
````````````
## [Example 186](https://spec.commonmark.org/0.30/#example-186)

This markdown:

````````````markdown
<div>
bar
</div>
*foo*

````````````

Should give output:

````````````html
<div>bar</div>*foo*
````````````

But instead was:

````````````html
<div><p>bar</p></div><p><em>foo</em></p>
````````````
## [Example 187](https://spec.commonmark.org/0.30/#example-187)

This markdown:

````````````markdown
Foo
<a href="bar">
baz

````````````

Should give output:

````````````html
<p>Foo<a href="bar">baz</p>
````````````

But instead was:

````````````html
ERROR Problem at row 4 Expecting symbol
````````````
## [Example 189](https://spec.commonmark.org/0.30/#example-189)

This markdown:

````````````markdown
<div>
*Emphasized* text.
</div>

````````````

Should give output:

````````````html
<div>*Emphasized* text.</div>
````````````

But instead was:

````````````html
<div><p><em>Emphasized</em>text.</p></div>
````````````
## [Example 190](https://spec.commonmark.org/0.30/#example-190)

This markdown:

````````````markdown
<table>

<tr>

<td>
Hi
</td>

</tr>

</table>

````````````

Should give output:

````````````html
<table><tr><td>Hi</td></tr></table>
````````````

But instead was:

````````````html
<table><tr><td><p>Hi</p></td></tr></table>
````````````
## [Example 191](https://spec.commonmark.org/0.30/#example-191)

This markdown:

````````````markdown
<table>

  <tr>

    <td>
      Hi
    </td>

  </tr>

</table>

````````````

Should give output:

````````````html
<table><tr><pre><code>&lt;td&gt; Hi &lt;/td&gt;</code></pre></tr></table>
````````````

But instead was:

````````````html
<table><tr><pre><code></code></pre><td><pre><code>Hi</code></pre></td></tr></table>
````````````

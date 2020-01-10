# GFM - HTML blocks

## [Example 118](https://github.github.com/gfm/#example-118)

This markdown:

```markdown
<table><tr><td>
<pre>
**Hello**,

_world_.
</pre>
</td></tr></table>

```

Should give output:

```html
<table><tr><td><pre>**Hello**,<p><em>world</em>.</pre></p></td></tr></table>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 119](https://github.github.com/gfm/#example-119)

This markdown:

```markdown
<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>

okay.

```

Should give output:

```html
<table><tr><td>hi</td></tr></table><p>okay.</p>
```

But instead was:

```html
ERROR Problem at row 7 Expecting Problem at row 2 Expecting --- Problem at row 2 Expecting *** Problem at row 2 Expecting ___
```
## [Example 120](https://github.github.com/gfm/#example-120)

This markdown:

```markdown
 <div>
  *hello*
         <foo><a>

```

Should give output:

```html
<div>*hello*<foo><a>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 121](https://github.github.com/gfm/#example-121)

This markdown:

```markdown
</div>
*foo*

```

Should give output:

```html
</div>*foo*
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
## [Example 122](https://github.github.com/gfm/#example-122)

This markdown:

```markdown
<DIV CLASS="foo">

*Markdown*

</DIV>

```

Should give output:

```html
<div class="foo"><p><em>Markdown</em></p></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 123](https://github.github.com/gfm/#example-123)

This markdown:

```markdown
<div id="foo"
  class="bar">
</div>

```

Should give output:

```html
<div class="bar" id="foo"></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 124](https://github.github.com/gfm/#example-124)

This markdown:

```markdown
<div id="foo" class="bar
  baz">
</div>

```

Should give output:

```html
<div class="bar
 baz" id="foo"></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 125](https://github.github.com/gfm/#example-125)

This markdown:

```markdown
<div>
*foo*

*bar*

```

Should give output:

```html
<div>*foo*<p><em>bar</em></p>
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol
```
## [Example 126](https://github.github.com/gfm/#example-126)

This markdown:

```markdown
<div id="foo"
*hi*

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol =
```
## [Example 127](https://github.github.com/gfm/#example-127)

This markdown:

```markdown
<div class
foo

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol =
```
## [Example 128](https://github.github.com/gfm/#example-128)

This markdown:

```markdown
<div *???-&&&-<---
*foo*

```

Should give output:

```html

```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol =
```
## [Example 129](https://github.github.com/gfm/#example-129)

This markdown:

```markdown
<div><a href="bar">*foo*</a></div>

```

Should give output:

```html
<div><a href="bar">*foo*</a></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 130](https://github.github.com/gfm/#example-130)

This markdown:

```markdown
<table><tr><td>
foo
</td></tr></table>

```

Should give output:

```html
<table><tr><td>foo</td></tr></table>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 131](https://github.github.com/gfm/#example-131)

This markdown:

```markdown
<div></div>
``` c
int x = 33;
```

```

Should give output:

```html
<div></div>``` c int x = 33; ```
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 132](https://github.github.com/gfm/#example-132)

This markdown:

```markdown
<a href="foo">
*bar*
</a>

```

Should give output:

```html
<a href="foo">*bar*</a>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 133](https://github.github.com/gfm/#example-133)

This markdown:

```markdown
<Warning>
*bar*
</Warning>

```

Should give output:

```html
<warning>*bar*</warning>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 134](https://github.github.com/gfm/#example-134)

This markdown:

```markdown
<i class="foo">
*bar*
</i>

```

Should give output:

```html
<i class="foo">*bar*</i>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 135](https://github.github.com/gfm/#example-135)

This markdown:

```markdown
</ins>
*bar*

```

Should give output:

```html
</ins>*bar*
```

But instead was:

```html
ERROR Problem at row 1 Bad repeat
```
## [Example 136](https://github.github.com/gfm/#example-136)

This markdown:

```markdown
<del>
*foo*
</del>

```

Should give output:

```html
<del>*foo*</del>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 137](https://github.github.com/gfm/#example-137)

This markdown:

```markdown
<del>

*foo*

</del>

```

Should give output:

```html
<del><p><em>foo</em></p></del>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 138](https://github.github.com/gfm/#example-138)

This markdown:

```markdown
<del>*foo*</del>

```

Should give output:

```html
<p><del><em>foo</em></del></p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 139](https://github.github.com/gfm/#example-139)

This markdown:

```markdown
<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
okay

```

Should give output:

```html
<pre language="haskell"><code>import Text.HTML.TagSoup main :: IO () main = print $ parseTags tags</code></pre><p>okay</p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 140](https://github.github.com/gfm/#example-140)

This markdown:

```markdown
<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
</script>
okay

```

Should give output:

```html
<script type="text/javascript">// JavaScript example document.getElementById(&quot;demo&quot;).innerHTML = &quot;Hello JavaScript!&quot;;</script><p>okay</p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 141](https://github.github.com/gfm/#example-141)

This markdown:

```markdown
<style
  type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
okay

```

Should give output:

```html
<style type="text/css">h1 {color:red;} p {color:blue;}</style><p>okay</p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 142](https://github.github.com/gfm/#example-142)

This markdown:

```markdown
<style
  type="text/css">

foo

```

Should give output:

```html
<style type="text/css">foo
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol
```
## [Example 143](https://github.github.com/gfm/#example-143)

This markdown:

```markdown
> <div>
> foo

bar

```

Should give output:

```html
<blockquote><div>foo</blockquote><p>bar</p>
```

But instead was:

```html
<p>&gt; &lt;div&gt; &gt; foo</p><p>bar</p>
```
## [Example 144](https://github.github.com/gfm/#example-144)

This markdown:

```markdown
- <div>
- foo

```

Should give output:

```html
<ul><li><div></li><li>foo</li></ul>
```

But instead was:

```html
<ul><li>&lt;div&gt;</li><li>foo</li></ul>
```
## [Example 145](https://github.github.com/gfm/#example-145)

This markdown:

```markdown
<style>p{color:red;}</style>
*foo*

```

Should give output:

```html
<style>p{color:red;}</style><p><em>foo</em></p>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 146](https://github.github.com/gfm/#example-146)

This markdown:

```markdown
<!-- foo -->*bar*
*baz*

```

Should give output:

```html
*bar*<p><em>baz</em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol =
```
## [Example 147](https://github.github.com/gfm/#example-147)

This markdown:

```markdown
<script>
foo
</script>1. *bar*

```

Should give output:

```html
<script>foo</script>1. *bar*
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 148](https://github.github.com/gfm/#example-148)

This markdown:

```markdown
<!-- Foo

bar
   baz -->
okay

```

Should give output:

```html
<p>okay</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol =
```
## [Example 149](https://github.github.com/gfm/#example-149)

This markdown:

```markdown
<?php

  echo '>';

?>
okay

```

Should give output:

```html
&#39;; ?&gt;<p>okay</p>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol =
```
## [Example 150](https://github.github.com/gfm/#example-150)

This markdown:

```markdown
<!DOCTYPE html>

```

Should give output:

```html
<!DOCTYPE HTML>
```

But instead was:

```html
ERROR Problem at row 1 Expecting symbol =
```
## [Example 151](https://github.github.com/gfm/#example-151)

This markdown:

```markdown
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

```

Should give output:

```html
<p>okay</p>
```

But instead was:

```html
ERROR Problem at row 2 Expecting symbol =
```
## [Example 152](https://github.github.com/gfm/#example-152)

This markdown:

```markdown
  <!-- foo -->

    <!-- foo -->

```

Should give output:

```html
<pre><code>&lt;!-- foo --&gt;</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 153](https://github.github.com/gfm/#example-153)

This markdown:

```markdown
  <div>

    <div>

```

Should give output:

```html
<div><pre><code>&lt;div&gt;</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 154](https://github.github.com/gfm/#example-154)

This markdown:

```markdown
Foo
<div>
bar
</div>

```

Should give output:

```html
<p>Foo</p><div>bar</div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 155](https://github.github.com/gfm/#example-155)

This markdown:

```markdown
<div>
bar
</div>
*foo*

```

Should give output:

```html
<div>bar</div>*foo*
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 156](https://github.github.com/gfm/#example-156)

This markdown:

```markdown
Foo
<a href="bar">
baz

```

Should give output:

```html
<p>Foo<a href="bar">baz</p>
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol
```
## [Example 157](https://github.github.com/gfm/#example-157)

This markdown:

```markdown
<div>

*Emphasized* text.

</div>

```

Should give output:

```html
<div><p><em>Emphasized</em>text.</p></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 158](https://github.github.com/gfm/#example-158)

This markdown:

```markdown
<div>
*Emphasized* text.
</div>

```

Should give output:

```html
<div>*Emphasized* text.</div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 159](https://github.github.com/gfm/#example-159)

This markdown:

```markdown
<table>

<tr>

<td>
Hi
</td>

</tr>

</table>

```

Should give output:

```html
<table><tr><td>Hi</td></tr></table>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## [Example 160](https://github.github.com/gfm/#example-160)

This markdown:

```markdown
<table>

  <tr>

    <td>
      Hi
    </td>

  </tr>

</table>

```

Should give output:

```html
<table><tr><pre><code>&lt;td&gt; Hi &lt;/td&gt;</code></pre></tr></table>
```

But instead was:

```html
ERROR Problem at row 11 Expecting Problem at row 3 Expecting --- Problem at row 3 Expecting *** Problem at row 3 Expecting ___
```

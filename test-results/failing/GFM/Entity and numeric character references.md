# GFM - Entity and numeric character references

## [Example 311](https://spec.commonmark.org/0.29/#example-311)

This markdown:

````````````markdown
&nbsp; &amp; &copy; &AElig; &Dcaron;
&frac34; &HilbertSpace; &DifferentialD;
&ClockwiseContourIntegral; &ngE;

````````````

Should give output:

````````````html
<p>&amp; © Æ Ď ¾ ℋ ⅆ ∲ ≧̸</p>
````````````

But instead was:

````````````html
<p>&amp; © Æ &amp;Dcaron; ¾ &amp;HilbertSpace; &amp;DifferentialD; &amp;ClockwiseContourIntegral; &amp;ngE;</p>
````````````
## [Example 317](https://spec.commonmark.org/0.29/#example-317)

This markdown:

````````````markdown
<a href="&ouml;&ouml;.html">

````````````

Should give output:

````````````html
<a href="öö.html">
````````````

But instead was:

````````````html
ERROR Problem at row 1 No entity named &quot;ö&quot; found.
````````````
## [Example 320](https://spec.commonmark.org/0.29/#example-320)

This markdown:

````````````markdown
``` f&ouml;&ouml;
foo
```

````````````

Should give output:

````````````html
<pre><code class="language-föö">foo</code></pre>
````````````

But instead was:

````````````html
<pre><code class="föö language-">foo</code></pre>
````````````

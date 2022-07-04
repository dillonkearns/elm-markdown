# GFM - Entity and numeric character references

## [Example 25](https://spec.commonmark.org/0.30/#example-25)

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
## [Example 28](https://spec.commonmark.org/0.30/#example-28)

This markdown:

````````````markdown
&nbsp &x; &#; &#x;
&#87654321;
&#abcdef0;
&ThisIsNotDefined; &hi?;

````````````

Should give output:

````````````html
<p>&amp;nbsp &amp;x; &amp;#; &amp;#x; &amp;#87654321; &amp;#abcdef0; &amp;ThisIsNotDefined; &amp;hi?;</p>
````````````

But instead was:

````````````html
<p>&amp;nbsp &amp;x; &amp;#; &amp;#x; � &amp;#abcdef0; &amp;ThisIsNotDefined; &amp;hi?;</p>
````````````
## [Example 31](https://spec.commonmark.org/0.30/#example-31)

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

# Security - sanitizer_bypass

## Example undefined

This markdown:

````````````markdown
AAA<script> <img <script> src=x onerror=alert(1) />BBB

AAA<sometag> <img <sometag> src=x onerror=alert(1)BBB

<a>a2<a2t>a2</a> b <c>c</c> d
# ![text](URL)
````````````

Should give output:

````````````html
<p>AAA&lt;script&gt; &lt;img &lt;script&gt; src=x onerror=alert(1) /&gt;BBB</p><p>AAA&lt;sometag&gt; &lt;img &lt;sometag&gt; src=x onerror=alert(1)BBB</p><p>&lt;a&gt;a2&lt;a2t&gt;a2&lt;/a&gt; b &lt;c&gt;c&lt;/c&gt; d</p><h1 id="text"><img alt="text" src="URL"></h1>
````````````

But instead was:

````````````html
ERROR Problem at row 5 tag name mismatch: a2t and a
````````````

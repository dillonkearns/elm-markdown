# ReDOS - redos_html_closing

## Example undefined

This markdown:

````````````markdown
<tag  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""  ""<" />a'a

````````````

Should give output:

````````````html
<p>&lt;tag &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot; &quot;&quot;&lt;&quot; /&gt;a&#39;a</p>
````````````

But instead was:

````````````html
ERROR Problem at row 1 Expecting symbol /&gt; Problem at row 1 Expecting symbol &gt;
````````````

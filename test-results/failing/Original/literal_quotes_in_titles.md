# Original - literal_quotes_in_titles

## Example undefined

This markdown:

````````````markdown
Foo [bar][].

Foo [bar](/url/ "Title with "quotes" inside").


  [bar]: /url/ "Title with "quotes" inside"


````````````

Should give output:

````````````html
<p>Foo<a href="/url/" title="Title with &quot;quotes&quot; inside">bar</a>.</p><p>Foo<a href="/url/" title="Title with &quot;quotes&quot; inside">bar</a>.</p>
````````````

But instead was:

````````````html
<p>Foo [bar][].</p><p>Foo [bar](/url/ &quot;Title with &quot;quotes&quot; inside&quot;).</p><p>[bar]: /url/ &quot;Title with &quot;quotes&quot; inside&quot;</p>
````````````

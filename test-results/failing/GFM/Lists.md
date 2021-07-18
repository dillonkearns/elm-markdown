# GFM - Lists

## [Example 281](https://spec.commonmark.org/0.29/#example-281)

This markdown:

````````````markdown
1. a

  2. b

   3. c

````````````

Should give output:

````````````html
<ol><li><p>a</p></li><li><p>b</p></li><li><p>c</p></li></ol>
````````````

But instead was:

````````````html
<ol><li><p>a</p></li><li><p>b</p><ol start="3"><li>c</li></ol></li></ol>
````````````
## [Example 283](https://spec.commonmark.org/0.29/#example-283)

This markdown:

````````````markdown
1. a

  2. b

    3. c

````````````

Should give output:

````````````html
<ol><li><p>a</p></li><li><p>b</p></li></ol><pre><code>3. c</code></pre>
````````````

But instead was:

````````````html
<ol><li><p>a</p></li><li><p>b</p><ol start="3"><li>c</li></ol></li></ol>
````````````

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
<ol><li>a</li></ol><p>2. b</p><p>3. c</p>
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
<ol><li>a</li></ol><p>2. b</p><pre><code>3. c</code></pre>
````````````

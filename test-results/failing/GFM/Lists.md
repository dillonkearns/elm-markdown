# GFM - Lists

## [Example 272](https://spec.commonmark.org/0.29/#example-272)

This markdown:

````````````markdown
1. foo
2. bar
3) baz

````````````

Should give output:

````````````html
<ol><li>foo</li><li>bar</li></ol><ol start="3"><li>baz</li></ol>
````````````

But instead was:

````````````html
<ol></ol><ol start="2"></ol><ol start="3"></ol>
````````````
## [Example 275](https://spec.commonmark.org/0.29/#example-275)

This markdown:

````````````markdown
The number of windows in my house is
1.  The number of doors is 6.

````````````

Should give output:

````````````html
<p>The number of windows in my house is</p><ol><li>The number of doors is 6.</li></ol>
````````````

But instead was:

````````````html
<p>The number of windows in my house is</p><ol></ol>
````````````
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
<ol></ol><p>2. b</p><p>3. c</p>
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
<ol></ol><p>2. b</p><pre><code>3. c</code></pre>
````````````
## [Example 294](https://spec.commonmark.org/0.29/#example-294)

This markdown:

````````````markdown
1. ```
   foo
   ```

   bar

````````````

Should give output:

````````````html
<ol><li><pre><code>foo</code></pre><p>bar</p></li></ol>
````````````

But instead was:

````````````html
<ol></ol><p>foo</p><pre><code>bar</code></pre>
````````````

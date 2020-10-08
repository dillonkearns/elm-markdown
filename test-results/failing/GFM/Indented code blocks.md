# GFM - Indented code blocks

## [Example 78](https://spec.commonmark.org/0.29/#example-78)

This markdown:

````````````markdown
  - foo

    bar

````````````

Should give output:

````````````html
<ul><li><p>foo</p><p>bar</p></li></ul>
````````````

But instead was:

````````````html
<p>- foo</p><pre><code>bar</code></pre>
````````````
## [Example 79](https://spec.commonmark.org/0.29/#example-79)

This markdown:

````````````markdown
1.  foo

    - bar

````````````

Should give output:

````````````html
<ol><li><p>foo</p><ul><li>bar</li></ul></li></ol>
````````````

But instead was:

````````````html
<ol><li>foo</li></ol><pre><code>- bar</code></pre>
````````````
## [Example 80](https://spec.commonmark.org/0.29/#example-80)

This markdown:

````````````markdown
    <a/>
    *hi*

    - one

````````````

Should give output:

````````````html
<pre><code>&lt;a/&gt; *hi* - one</code></pre>
````````````

But instead was:

````````````html
<pre><code>&lt;a/&gt; *hi*</code></pre><pre><code>- one</code></pre>
````````````
## [Example 81](https://spec.commonmark.org/0.29/#example-81)

This markdown:

````````````markdown
    chunk1

    chunk2
  
 
 
    chunk3

````````````

Should give output:

````````````html
<pre><code>chunk1 chunk2 chunk3</code></pre>
````````````

But instead was:

````````````html
<pre><code>chunk1</code></pre><pre><code>chunk2</code></pre><pre><code>chunk3</code></pre>
````````````
## [Example 82](https://spec.commonmark.org/0.29/#example-82)

This markdown:

````````````markdown
    chunk1
      
      chunk2

````````````

Should give output:

````````````html
<pre><code>chunk1 chunk2</code></pre>
````````````

But instead was:

````````````html
<pre><code>chunk1</code></pre><pre><code>chunk2</code></pre>
````````````

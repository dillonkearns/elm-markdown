# GFM - List items

## [Example 243](https://spec.commonmark.org/0.29/#example-243)

This markdown:

````````````markdown
1.     indented code

   paragraph

       more code

````````````

Should give output:

````````````html
<ol><li><pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>
````````````

But instead was:

````````````html
<ol><li>indented code</li></ol><p>paragraph</p><pre><code>more code</code></pre>
````````````
## [Example 244](https://spec.commonmark.org/0.29/#example-244)

This markdown:

````````````markdown
1.      indented code

   paragraph

       more code

````````````

Should give output:

````````````html
<ol><li><pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>
````````````

But instead was:

````````````html
<ol><li>indented code</li></ol><p>paragraph</p><pre><code>more code</code></pre>
````````````
## [Example 253](https://spec.commonmark.org/0.29/#example-253)

This markdown:

````````````markdown
1. foo
2.
3. bar

````````````

Should give output:

````````````html
<ol><li>foo</li><li></li><li>bar</li></ol>
````````````

But instead was:

````````````html
<ol><li>foo 2.</li><li>bar</li></ol>
````````````

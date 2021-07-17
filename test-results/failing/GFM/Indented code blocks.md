# GFM - Indented code blocks

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
<ol></ol><pre><code>- bar</code></pre>
````````````

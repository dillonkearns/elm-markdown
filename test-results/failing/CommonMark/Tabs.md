# CommonMark - Tabs

## [Example 4](https://spec.commonmark.org/0.30/#example-4)

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
<ul><li>foo</li></ul><pre><code>bar</code></pre>
````````````
## [Example 5](https://spec.commonmark.org/0.30/#example-5)

This markdown:

````````````markdown
- foo

		bar

````````````

Should give output:

````````````html
<ul><li><p>foo</p><pre><code>bar</code></pre></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo</li></ul><pre><code>bar</code></pre>
````````````
## [Example 7](https://spec.commonmark.org/0.30/#example-7)

This markdown:

````````````markdown
-		foo

````````````

Should give output:

````````````html
<ul><li><pre><code>foo</code></pre></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo</li></ul>
````````````
## [Example 9](https://spec.commonmark.org/0.30/#example-9)

This markdown:

````````````markdown
 - foo
   - bar
	 - baz

````````````

Should give output:

````````````html
<ul><li>foo<ul><li>bar<ul><li>baz</li></ul></li></ul></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo<ul><li>bar - baz</li></ul></li></ul>
````````````

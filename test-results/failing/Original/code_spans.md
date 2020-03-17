# Original - code_spans

## Example undefined

This markdown:

````````````markdown
`<test a="` content of attribute `">`

Fix for backticks within HTML tag: <span attr='`ticks`'>like this</span>

Here's how you put `` `backticks` `` in a code span.


````````````

Should give output:

````````````html
<p><code>&lt;test a=&quot;</code>content of attribute<code>&quot;&gt;</code></p><p>Fix for backticks within HTML tag:<span attr="`ticks`">like this</span></p><p>Here&#39;s how you put<code>`backticks`</code>in a code span.</p>
````````````

But instead was:

````````````html
<p><code>&lt;test a=&quot;</code>content of attribute<code>&quot;&gt;</code></p><p>Fix for backticks within HTML tag:<span attr="`ticks`"><p>like this</p></span></p><p>Here&#39;s how you put<code>`backticks`</code>in a code span.</p>
````````````

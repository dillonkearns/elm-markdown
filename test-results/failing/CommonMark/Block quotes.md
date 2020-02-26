# CommonMark - Block quotes

## [Example 207](https://spec.commonmark.org/0.29/#example-207)

This markdown:

````````````markdown
> ```
foo
```

````````````

Should give output:

````````````html
<blockquote><pre><code></code></pre></blockquote><p>foo</p><pre><code></code></pre>
````````````

But instead was:

````````````html
<blockquote><pre><code>foo</code></pre></blockquote>
````````````
## [Example 208](https://spec.commonmark.org/0.29/#example-208)

This markdown:

````````````markdown
> foo
    - bar

````````````

Should give output:

````````````html
<blockquote><p>foo - bar</p></blockquote>
````````````

But instead was:

````````````html
<blockquote><p>foo</p></blockquote><pre><code>- bar</code></pre>
````````````
## [Example 219](https://spec.commonmark.org/0.29/#example-219)

This markdown:

````````````markdown
> bar
>
baz

````````````

Should give output:

````````````html
<blockquote><p>bar</p></blockquote><p>baz</p>
````````````

But instead was:

````````````html
<blockquote><p>bar</p><p>baz</p></blockquote>
````````````

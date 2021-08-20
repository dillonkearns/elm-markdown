# GFM - Block quotes

## [Example 237](https://spec.commonmark.org/0.30/#example-237)

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
<blockquote><pre><code>foo</code></pre></blockquote><pre><code></code></pre>
````````````
## [Example 238](https://spec.commonmark.org/0.30/#example-238)

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
## [Example 249](https://spec.commonmark.org/0.30/#example-249)

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

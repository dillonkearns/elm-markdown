# GFM - Fenced code blocks

## [Example 96](https://spec.commonmark.org/0.29/#example-96)

This markdown:

````````````markdown
```

````````````

Should give output:

````````````html
<pre><code></code></pre>
````````````

But instead was:

````````````html
<p>```</p>
````````````
## [Example 97](https://spec.commonmark.org/0.29/#example-97)

This markdown:

````````````markdown
`````

```
aaa

````````````

Should give output:

````````````html
<pre><code>``` aaa</code></pre>
````````````

But instead was:

````````````html
<p>`````</p><p>``` aaa</p>
````````````
## [Example 98](https://spec.commonmark.org/0.29/#example-98)

This markdown:

````````````markdown
> ```
> aaa

bbb

````````````

Should give output:

````````````html
<blockquote><pre><code>aaa</code></pre></blockquote><p>bbb</p>
````````````

But instead was:

````````````html
<blockquote><p>``` aaa</p></blockquote><p>bbb</p>
````````````
## [Example 100](https://spec.commonmark.org/0.29/#example-100)

This markdown:

````````````markdown
```
```

````````````

Should give output:

````````````html
<pre><code></code></pre>
````````````

But instead was:

````````````html
<p><code></code></p>
````````````
## [Example 101](https://spec.commonmark.org/0.29/#example-101)

This markdown:

````````````markdown
 ```
 aaa
aaa
```

````````````

Should give output:

````````````html
<pre><code>aaa aaa</code></pre>
````````````

But instead was:

````````````html
<p><code>aaa aaa</code></p>
````````````
## [Example 102](https://spec.commonmark.org/0.29/#example-102)

This markdown:

````````````markdown
  ```
aaa
  aaa
aaa
  ```

````````````

Should give output:

````````````html
<pre><code>aaa aaa aaa</code></pre>
````````````

But instead was:

````````````html
<p><code>aaa aaa aaa</code></p>
````````````
## [Example 103](https://spec.commonmark.org/0.29/#example-103)

This markdown:

````````````markdown
   ```
   aaa
    aaa
  aaa
   ```

````````````

Should give output:

````````````html
<pre><code>aaa aaa aaa</code></pre>
````````````

But instead was:

````````````html
<p><code>aaa aaa aaa</code></p>
````````````
## [Example 105](https://spec.commonmark.org/0.29/#example-105)

This markdown:

````````````markdown
```
aaa
  ```

````````````

Should give output:

````````````html
<pre><code>aaa</code></pre>
````````````

But instead was:

````````````html
<p><code>aaa</code></p>
````````````
## [Example 106](https://spec.commonmark.org/0.29/#example-106)

This markdown:

````````````markdown
   ```
aaa
  ```

````````````

Should give output:

````````````html
<pre><code>aaa</code></pre>
````````````

But instead was:

````````````html
<p><code>aaa</code></p>
````````````
## [Example 107](https://spec.commonmark.org/0.29/#example-107)

This markdown:

````````````markdown
```
aaa
    ```

````````````

Should give output:

````````````html
<pre><code>aaa ```</code></pre>
````````````

But instead was:

````````````html
<p><code>aaa</code></p>
````````````
## [Example 109](https://spec.commonmark.org/0.29/#example-109)

This markdown:

````````````markdown
~~~~~~
aaa
~~~ ~~

````````````

Should give output:

````````````html
<pre><code>aaa ~~~ ~~</code></pre>
````````````

But instead was:

````````````html
<p>~~~~~~ aaa ~~~ ~~</p>
````````````
## [Example 113](https://spec.commonmark.org/0.29/#example-113)

This markdown:

````````````markdown
~~~~    ruby startline=3 $%@#$
def foo(x)
  return 3
end
~~~~~~~

````````````

Should give output:

````````````html
<pre><code class="language-ruby">def foo(x) return 3 end</code></pre>
````````````

But instead was:

````````````html
<pre><code class="$%@#$ language- ruby startline=3">def foo(x) return 3 end</code></pre>
````````````
## [Example 114](https://spec.commonmark.org/0.29/#example-114)

This markdown:

````````````markdown
````;
````

````````````

Should give output:

````````````html
<pre><code class="language-;"></code></pre>
````````````

But instead was:

````````````html
<p><code>;</code></p>
````````````
## [Example 116](https://spec.commonmark.org/0.29/#example-116)

This markdown:

````````````markdown
~~~ aa ``` ~~~
foo
~~~

````````````

Should give output:

````````````html
<pre><code class="language-aa">foo</code></pre>
````````````

But instead was:

````````````html
<pre><code class="``` aa language- ~~~">foo</code></pre>
````````````

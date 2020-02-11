# GFM - Fenced code blocks

## [Example 91](https://spec.commonmark.org/0.29/#example-91)

This markdown:

```markdown
``
foo
``

```

Should give output:

```html
<p><code>foo</code></p>
```

But instead was:

```html
<p>foo</p>
```
## [Example 94](https://spec.commonmark.org/0.29/#example-94)

This markdown:

```markdown
````
aaa
```
``````

```

Should give output:

```html
<pre><code>aaa ```</code></pre>
```

But instead was:

```html
ERROR Problem at row 8 Expecting symbol ```
```
## [Example 95](https://spec.commonmark.org/0.29/#example-95)

This markdown:

```markdown
~~~~
aaa
~~~
~~~~

```

Should give output:

```html
<pre><code>aaa ~~~</code></pre>
```

But instead was:

```html
ERROR Problem at row 8 Expecting symbol ~~~
```
## [Example 96](https://spec.commonmark.org/0.29/#example-96)

This markdown:

```markdown
```

```

Should give output:

```html
<pre><code></code></pre>
```

But instead was:

```html
ERROR Problem at row 3 Expecting symbol ```
```
## [Example 97](https://spec.commonmark.org/0.29/#example-97)

This markdown:

```markdown
`````

```
aaa

```

Should give output:

```html
<pre><code>``` aaa</code></pre>
```

But instead was:

```html
<pre><code></code></pre><p>aaa</p>
```
## [Example 98](https://spec.commonmark.org/0.29/#example-98)

This markdown:

```markdown
> ```
> aaa

bbb

```

Should give output:

```html
<blockquote><pre><code>aaa</code></pre></blockquote><p>bbb</p>
```

But instead was:

```html
ERROR Problem at row 6 Expecting symbol ```
```
## [Example 100](https://spec.commonmark.org/0.29/#example-100)

This markdown:

```markdown
```
```

```

Should give output:

```html
<pre><code></code></pre>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol ```
```
## [Example 101](https://spec.commonmark.org/0.29/#example-101)

This markdown:

```markdown
 ```
 aaa
aaa
```

```

Should give output:

```html
<pre><code>aaa aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 102](https://spec.commonmark.org/0.29/#example-102)

This markdown:

```markdown
  ```
aaa
  aaa
aaa
  ```

```

Should give output:

```html
<pre><code>aaa aaa aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 103](https://spec.commonmark.org/0.29/#example-103)

This markdown:

```markdown
   ```
   aaa
    aaa
  aaa
   ```

```

Should give output:

```html
<pre><code>aaa aaa aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 104](https://spec.commonmark.org/0.29/#example-104)

This markdown:

```markdown
    ```
    aaa
    ```

```

Should give output:

```html
<pre><code>``` aaa ```</code></pre>
```

But instead was:

```html
<pre><code>```</code></pre><pre><code>aaa</code></pre><pre><code>```</code></pre>
```
## [Example 105](https://spec.commonmark.org/0.29/#example-105)

This markdown:

```markdown
```
aaa
  ```

```

Should give output:

```html
<pre><code>aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol ```
```
## [Example 106](https://spec.commonmark.org/0.29/#example-106)

This markdown:

```markdown
   ```
aaa
  ```

```

Should give output:

```html
<pre><code>aaa</code></pre>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 107](https://spec.commonmark.org/0.29/#example-107)

This markdown:

```markdown
```
aaa
    ```

```

Should give output:

```html
<pre><code>aaa ```</code></pre>
```

But instead was:

```html
ERROR Problem at row 5 Expecting symbol ```
```
## [Example 108](https://spec.commonmark.org/0.29/#example-108)

This markdown:

```markdown
``` ```
aaa

```

Should give output:

```html
<p><code></code>aaa</p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol ```
```
## [Example 109](https://spec.commonmark.org/0.29/#example-109)

This markdown:

```markdown
~~~~~~
aaa
~~~ ~~

```

Should give output:

```html
<pre><code>aaa ~~~ ~~</code></pre>
```

But instead was:

```html
ERROR Problem at row 5 Expecting --- Problem at row 5 Expecting *** Problem at row 5 Expecting ___
```
## [Example 111](https://spec.commonmark.org/0.29/#example-111)

This markdown:

```markdown
foo
---
~~~
bar
~~~
# baz

```

Should give output:

```html
<h2>foo</h2><pre><code>bar</code></pre><h1>baz</h1>
```

But instead was:

```html
<p>foo</p><hr><pre><code>bar</code></pre><h1>baz</h1>
```
## [Example 112](https://spec.commonmark.org/0.29/#example-112)

This markdown:

```markdown
```ruby
def foo(x)
  return 3
end
```

```

Should give output:

```html
<pre><code class="language-ruby">def foo(x) return 3 end</code></pre>
```

But instead was:

```html
<pre><code>def foo(x) return 3 end</code></pre>
```
## [Example 113](https://spec.commonmark.org/0.29/#example-113)

This markdown:

```markdown
~~~~    ruby startline=3 $%@#$
def foo(x)
  return 3
end
~~~~~~~

```

Should give output:

```html
<pre><code class="language-ruby">def foo(x) return 3 end</code></pre>
```

But instead was:

```html
ERROR Problem at row 9 Expecting symbol ~~~
```
## [Example 114](https://spec.commonmark.org/0.29/#example-114)

This markdown:

```markdown
````;
````

```

Should give output:

```html
<pre><code class="language-;"></code></pre>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol ```
```
## [Example 115](https://spec.commonmark.org/0.29/#example-115)

This markdown:

```markdown
``` aa ```
foo

```

Should give output:

```html
<p><code>aa</code>foo</p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol ```
```
## [Example 116](https://spec.commonmark.org/0.29/#example-116)

This markdown:

```markdown
~~~ aa ``` ~~~
foo
~~~

```

Should give output:

```html
<pre><code class="language-aa">foo</code></pre>
```

But instead was:

```html
<pre><code>foo</code></pre>
```

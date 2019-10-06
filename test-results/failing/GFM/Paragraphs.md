# GFM - Paragraphs

## [Example 192](https://github.github.com/gfm/#example-192)

This markdown:

```markdown
  aaa
 bbb

```

Should give output:

```html
<p>aaa bbb</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 193](https://github.github.com/gfm/#example-193)

This markdown:

```markdown
aaa
             bbb
                                       ccc

```

Should give output:

```html
<p>aaa bbb ccc</p>
```

But instead was:

```html
<p>aaa</p><pre><code>bbb</code></pre><pre><code>ccc</code></pre>
```
## [Example 194](https://github.github.com/gfm/#example-194)

This markdown:

```markdown
   aaa
bbb

```

Should give output:

```html
<p>aaa bbb</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting --- Problem at row 1 Expecting *** Problem at row 1 Expecting ___
```
## [Example 196](https://github.github.com/gfm/#example-196)

This markdown:

```markdown
aaa     
bbb     

```

Should give output:

```html
<p>aaa<br>bbb</p>
```

But instead was:

```html
<p>aaa bbb</p>
```

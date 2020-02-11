# CommonMark - Paragraphs

## [Example 192](https://spec.commonmark.org/0.29/#example-192)

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
## [Example 193](https://spec.commonmark.org/0.29/#example-193)

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
<p>aaa</p><pre><code>bbb ccc</code></pre>
```
## [Example 194](https://spec.commonmark.org/0.29/#example-194)

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
## [Example 196](https://spec.commonmark.org/0.29/#example-196)

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

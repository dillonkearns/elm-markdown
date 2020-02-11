# New - toplevel_paragraphs

## Example undefined

This markdown:

```markdown
hello world
    text after spaces
    text after spaces

paragraph before code
```
text inside block code
```

paragraph before hr
* * *

paragraph before blockquote
> text for blockquote

paragraph before list
* text inside list

paragraph before div
<div>text inside div</div>

paragraph with span
<span>text inside span</span>

hello [world][how]

[how]: /are/you

<div>hello</div>

<span>hello</span>

```

Should give output:

```html
<p>hello world text after spaces text after spaces</p><p>paragraph before code</p><pre><code>text inside block code</code></pre><p>paragraph before hr</p><hr><p>paragraph before blockquote</p><blockquote><p>text for blockquote</p></blockquote><p>paragraph before list</p><ul><li>text inside list</li></ul><p>paragraph before div</p><div>text inside div</div><p>paragraph with span<span>text inside span</span></p><p>hello<a href="/are/you">world</a></p><div>hello</div><p><span>hello</span></p>
```

But instead was:

```html
ERROR Problem at row 23 Expecting --- Problem at row 23 Expecting *** Problem at row 23 Expecting ___
```

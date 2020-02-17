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
ERROR oneOf failed parsing this value:<span>Parsing failed in the following 2 ways: (1) Expected a but was span (2) Expected div but was span (3) Expected th but was span (4) Expected pre but was span (5) Expected td but was span (6) Expected tr but was span (7) Expected table but was span
```

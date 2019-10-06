# Original - inline_html_comments

## Example undefined

This markdown:

```markdown
Paragraph one.

<!-- This is a simple comment -->

<!--
	This is another comment.
-->

Paragraph two.

<!-- one comment block -- -- with two comments -->

The end.

```

Should give output:

```html
<p>Paragraph one.</p><p>Paragraph two.</p><p>The end.</p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol =
```

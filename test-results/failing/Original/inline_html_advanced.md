# Original - inline_html_advanced

## Example undefined

This markdown:

```markdown
Simple block on one line:

<div>foo</div>

And nested without indentation:

<div>
<div>
<div>
foo
</div>
<div style=">"/>
</div>
<div>bar</div>
</div>

```

Should give output:

```html
<p>Simple block on one line:</p><div>foo</div><p>And nested without indentation:</p><div><div><div>foo</div><div style="&gt;"></div><div>bar</div></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```

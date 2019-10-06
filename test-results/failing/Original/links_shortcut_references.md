# Original - links_shortcut_references

## Example undefined

This markdown:

```markdown
This is the [simple case].

[simple case]: /simple



This one has a [line
break].

This one has a [line 
break] with a line-ending space.

[line break]: /foo


[this] [that] and the [other]

[this]: /this
[that]: /that
[other]: /other

```

Should give output:

```html
<p>This is the<a href="/simple">simple case</a>.</p><p>This one has a<a href="/foo">line break</a>.</p><p>This one has a<a href="/foo">line break</a>with a line-ending space.</p><p><a href="/that">this</a>and the<a href="/other">other</a></p>
```

But instead was:

```html
ERROR Problem at row 32 Expecting Problem at row 1 Expecting symbol (
```

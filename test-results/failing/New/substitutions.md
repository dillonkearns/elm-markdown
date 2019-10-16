# New - substitutions

## Example undefined

This markdown:

```markdown
foo␤␤bar

* a *

```

Should give output:

```html
<p>foo␤␤bar</p><p>* a *</p>
```

But instead was:

```html
<p>foo␤␤bar</p><p><em>a</em></p>
```

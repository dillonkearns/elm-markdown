# New - same_bullet

## Example undefined

This markdown:

```markdown
* test
+ test
- test

```

Should give output:

```html
<ul><li>test</li><li>test</li><li>test</li></ul>
```

But instead was:

```html
<p><em>test + test</em></p><ul><li><p>test</p></li></ul>
```

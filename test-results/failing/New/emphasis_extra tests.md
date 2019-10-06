# New - emphasis_extra tests

## Example undefined

This markdown:

```markdown
_test_. _test_: _test_! _test_? _test_-
```

Should give output:

```html
<p><em>test</em>.<em>test</em>:<em>test</em>!<em>test</em>?<em>test</em>-</p>
```

But instead was:

```html
<p>_test_. _test_: _test_! _test_? _test_-</p>
```

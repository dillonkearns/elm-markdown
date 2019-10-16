# New - nested_em

## Example undefined

This markdown:

```markdown
*test **test** test*

_test __test__ test_

```

Should give output:

```html
<p><em>test<strong>test</strong>test</em></p><p><em>test<strong>test</strong>test</em></p>
```

But instead was:

```html
<ul><li><p>test<strong>test</strong>test</p></li></ul><p>_test __test__ test_</p>
```

# New - hr_list_break

## Example undefined

This markdown:

```markdown
* hello
world
* how
are
* * *
you today?

```

Should give output:

```html
<ul><li>hello world</li><li>how are</li></ul><hr><p>you today?</p>
```

But instead was:

```html
<p><em>hello world</em>how are<em></em><em>you today?</em></p>
```

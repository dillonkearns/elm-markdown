# New - list_loose_tasks

## Example undefined

This markdown:

```markdown
- Tasks
- [x] Task1

- [ ] <pre>Task2</pre>

```

Should give output:

```html
<ul><li><p>Tasks</p></li><li><p><input checked="" disabled="" type="checkbox">Task1</p></li><li><p><input disabled="" type="checkbox"></p><pre>Task2</pre></li></ul>
```

But instead was:

```html
ERROR Problem at row 8 Expecting Problem at row 1 Expecting symbol (
```

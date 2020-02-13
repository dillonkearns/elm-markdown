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
<ul><li>Tasks</li><li><input checked="" disabled="" type="checkbox">Task1</li></ul><ul><li><input disabled="" type="checkbox">&lt;pre&gt;Task2&lt;/pre&gt;</li></ul>
```

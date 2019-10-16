# New - tricky_list

## Example undefined

This markdown:

```markdown
**hello** _world_

* hello world

**hello** _world_

* hello world

**hello** _world_

* Hello world

**hello** _world_

* hello world

```

Should give output:

```html
<p><strong>hello</strong><em>world</em></p><ul><li>hello world</li></ul><p><strong>hello</strong><em>world</em></p><ul><li>hello world</li></ul><p><strong>hello</strong><em>world</em></p><ul><li>Hello world</li></ul><p><strong>hello</strong><em>world</em></p><ul><li>hello world</li></ul>
```

But instead was:

```html
<ul><li><p><em>hello</em><strong>_world_</strong></p></li></ul><ul><li><p>hello world</p></li></ul><ul><li><p><em>hello</em><strong>_world_</strong></p></li></ul><ul><li><p>hello world</p></li></ul><ul><li><p><em>hello</em><strong>_world_</strong></p></li></ul><ul><li><p>Hello world</p></li></ul><ul><li><p><em>hello</em><strong>_world_</strong></p></li></ul><ul><li><p>hello world</p></li></ul>
```

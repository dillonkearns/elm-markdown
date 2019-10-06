# New - nested_code

## Example undefined

This markdown:

```markdown
````` hi ther `` ok ``` `````

`` ` ``

``There is a literal backtick (`) here.``

A backtick-delimited string in a code span: `` `foo` ``

Please don't use any `<blink>` tags.
```

Should give output:

```html
<p><code>hi ther `` ok ```</code></p><p><code>`</code></p><p><code>There is a literal backtick (`) here.</code></p><p>A backtick-delimited string in a code span:<code>`foo`</code></p><p>Please don&#39;t use any<code>&lt;blink&gt;</code>tags.</p>
```

But instead was:

```html
ERROR Problem at row 10 Expecting symbol ```
```

# Original - code_spans

## Example undefined

This markdown:

```markdown
`<test a="` content of attribute `">`

Fix for backticks within HTML tag: <span attr='`ticks`'>like this</span>

Here's how you put `` `backticks` `` in a code span.


```

Should give output:

```html
<p><code>&lt;test a=&quot;</code>content of attribute<code>&quot;&gt;</code></p><p>Fix for backticks within HTML tag:<span attr="`ticks`">like this</span></p><p>Here&#39;s how you put<code>`backticks`</code>in a code span.</p>
```

But instead was:

```html
ERROR oneOf failed parsing this value:<span attr="`ticks`">Parsing failed in the following 2 ways: (1) Expected a but was span (2) Expected div but was span (3) Expected th but was span (4) Expected pre but was span (5) Expected td but was span (6) Expected tr but was span (7) Expected table but was span
```

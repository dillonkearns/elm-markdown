# Security - sanitizer_bypass_remove_tag

## Example undefined

This markdown:

```markdown
AAA<sometag> <img <sometag> src=x onerror=alert(1)BBB

```

Should give output:

```html
<p>AAA &lt;img src=x onerror=alert(1)BBB</p>
```

But instead was:

```html
ERROR oneOf failed parsing this value:<sometag>Parsing failed in the following 2 ways: (1) Expected a but was sometag (2) Expected div but was sometag (3) Expected th but was sometag (4) Expected pre but was sometag (5) Expected td but was sometag (6) Expected tr but was sometag (7) Expected table but was sometag
```

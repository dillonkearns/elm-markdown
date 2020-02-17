# Security - sanitizer_bypass_remove_script

## Example undefined

This markdown:

```markdown
AAA<script> <img <script> src=x onerror=alert(1) />BBB

```

Should give output:

```html
<p>AAA</p>
```

But instead was:

```html
ERROR oneOf failed parsing this value:<script>Parsing failed in the following 2 ways: (1) Expected a but was script (2) Expected div but was script (3) Expected th but was script (4) Expected pre but was script (5) Expected td but was script (6) Expected tr but was script (7) Expected table but was script
```

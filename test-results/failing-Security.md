## Security

### sanitizer_bypass_remove_generic

Example undefined

This markdown:

```markdown
<a>a2<a2t>a2</a> b <c>c</c> d
# ![text](URL)
```

Gives this incorrect output:

```html
<p>a2a2 b c d</p>
<h1 id="text"><img src="URL" alt="text"></h1>

```

### sanitizer_bypass

Example undefined

This markdown:

```markdown
AAA<script> <img <script> src=x onerror=alert(1) />BBB

AAA<sometag> <img <sometag> src=x onerror=alert(1)BBB

<a>a2<a2t>a2</a> b <c>c</c> d
# ![text](URL)
```

Gives this incorrect output:

```html
<p>AAA&lt;script&gt; &lt;img &lt;script&gt; src=x onerror=alert(1) /&gt;BBB</p>

<p>AAA&lt;sometag&gt; &lt;img &lt;sometag&gt; src=x onerror=alert(1)BBB</p>

<p>&lt;a&gt;a2&lt;a2t&gt;a2&lt;/a&gt; b &lt;c&gt;c&lt;/c&gt; d</p>
<h1 id="text"><img src="URL" alt="text"></h1>

```

### sanitizer_bypass_remove_script

Example undefined

This markdown:

```markdown
AAA<script> <img <script> src=x onerror=alert(1) />BBB

```

Gives this incorrect output:

```html
<p>AAA</p>

```

### sanitizer_bypass_remove_tag

Example undefined

This markdown:

```markdown
AAA<sometag> <img <sometag> src=x onerror=alert(1)BBB

```

Gives this incorrect output:

```html
<p>AAA &lt;img  src=x onerror=alert(1)BBB</p>

```


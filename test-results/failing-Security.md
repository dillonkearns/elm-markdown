## Security

### sanitizer_bypass_remove_script

Example undefined

```html
<p>AAA</p>

```

### sanitizer_bypass

Example undefined

```html
<p>AAA&lt;script&gt; &lt;img &lt;script&gt; src=x onerror=alert(1) /&gt;BBB</p>

<p>AAA&lt;sometag&gt; &lt;img &lt;sometag&gt; src=x onerror=alert(1)BBB</p>

<p>&lt;a&gt;a2&lt;a2t&gt;a2&lt;/a&gt; b &lt;c&gt;c&lt;/c&gt; d</p>
<h1 id="text"><img src="URL" alt="text"></h1>

```

### sanitizer_bypass_remove_tag

Example undefined

```html
<p>AAA &lt;img  src=x onerror=alert(1)BBB</p>

```

### sanitizer_bypass_remove_generic

Example undefined

```html
<p>a2a2 b c d</p>
<h1 id="text"><img src="URL" alt="text"></h1>

```


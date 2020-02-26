# New - smartypants_code

## Example undefined

This markdown:

````````````markdown
<pre>&amp;</pre>
<code>--foo</code>
<kbd>---foo</kbd>
<script>--foo</script>

Ensure that text such as custom tags that happen to
begin with the same letters as the above tags don't
match and thus benefit from Smartypants-ing.
<script-custom>--foo</script-custom>
`--foo` <codebar --foo codebar>

````````````

Should give output:

````````````html
<pre>&amp;</pre><p><code>--foo</code><kbd>---foo</kbd></p><script>--foo</script><p>Ensure that text such as custom tags that happen to begin with the same letters as the above tags don’t match and thus benefit from Smartypants-ing.</p><p><script-custom>–foo</script-custom><code>--foo</code>&lt;codebar –foo codebar&gt;</p>
````````````

But instead was:

````````````html
ERROR Problem at row 3 Expecting Problem at row 1 Expecting end Problem at row 1 Expecting newline
````````````

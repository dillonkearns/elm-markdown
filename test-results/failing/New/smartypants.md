# New - smartypants

## Example undefined

This markdown:

````````````markdown
Hello world 'how' "are" you -- today...

"It's a more 'challenging' smartypants test..."

'And,' as a bonus --- "one
multiline" test!

````````````

Should give output:

````````````html
<p>Hello world ‘how’ “are” you – today…</p><p>“It’s a more ‘challenging’ smartypants test…”</p><p>‘And,’ as a bonus — “one multiline” test!</p>
````````````

But instead was:

````````````html
<p>Hello world &#39;how&#39; &quot;are&quot; you -- today...</p><p>&quot;It&#39;s a more &#39;challenging&#39; smartypants test...&quot;</p><p>&#39;And,&#39; as a bonus --- &quot;one multiline&quot; test!</p>
````````````

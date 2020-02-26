# New - double_link

## Example undefined

This markdown:

````````````markdown
<p>Already linked: <a href="http://example.com/">http://example.com/</a>.</p>

Already linked: [http://example.com/](http://example.com/).

Already linked: <a href="http://example.com/">**http://example.com/**</a>.

````````````

Should give output:

````````````html
<p>Already linked:<a href="http://example.com/">http://example.com/</a>.</p><p>Already linked:<a href="http://example.com/">http://example.com/</a>.</p><p>Already linked:<a href="http://example.com/"><strong>http://example.com/</strong></a>.</p>
````````````

But instead was:

````````````html
<p><p>Already linked:</p><a href="http://example.com/"><p>http://example.com/</p></a><p>.</p></p><p>Already linked:<a href="http://example.com/">http://example.com/</a>.</p><p>Already linked:<a href="http://example.com/"><p><strong>http://example.com/</strong></p></a>.</p>
````````````

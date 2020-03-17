# New - ref_paren

## Example undefined

This markdown:

````````````markdown
[hi]

[hi]: /url (there)

````````````

Should give output:

````````````html
<p><a href="/url" title="there">hi</a></p>
````````````

But instead was:

````````````html
<p><a href="/url">hi</a></p><p>(there)</p>
````````````

# New - uppercase_hex

## Example undefined

This markdown:

````````````markdown
lower[click me](javascript&#x3a;...)lower
upper[click me](javascript&#X3a;...)upper

````````````

Should give output:

````````````html
<p>lowerclick melower upperclick meupper</p>
````````````

But instead was:

````````````html
<p>lower<a href="javascript:...">click me</a>lower upper<a href="javascript:...">click me</a>upper</p>
````````````

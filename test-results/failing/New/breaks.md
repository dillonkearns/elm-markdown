# New - breaks

## Example undefined

This markdown:

````````````markdown
A
B

````````````

Should give output:

````````````html
<p>A<br>B</p>
````````````

But instead was:

````````````html
<p>A B</p>
````````````

# New - pedantic_heading_interrupts_paragraph

## Example undefined

This markdown:

````````````markdown
paragraph before head with hash
#how are you

paragraph before head with equals
how are you again
===========

````````````

Should give output:

````````````html
<p>paragraph before head with hash</p><h1 id="how-are-you">how are you</h1><p>paragraph before head with equals</p><h1 id="how-are-you-again">how are you again</h1>
````````````

But instead was:

````````````html
<p>paragraph before head with hash #how are you</p><h1>paragraph before head with equals how are you again</h1>
````````````

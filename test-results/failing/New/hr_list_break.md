# New - hr_list_break

## Example undefined

This markdown:

````````````markdown
* hello
world
* how
are
* * *
you today?

````````````

Should give output:

````````````html
<ul><li>hello world</li><li>how are</li></ul><hr><p>you today?</p>
````````````

But instead was:

````````````html
<ul><li>hello</li></ul><p>world</p><ul><li>how</li></ul><p>are</p><hr><p>you today?</p>
````````````

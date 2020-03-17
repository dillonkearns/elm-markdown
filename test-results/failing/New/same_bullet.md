# New - same_bullet

## Example undefined

This markdown:

````````````markdown
* test
+ test
- test

````````````

Should give output:

````````````html
<ul><li>test</li><li>test</li><li>test</li></ul>
````````````

But instead was:

````````````html
<ul><li>test</li></ul><ul><li>test</li></ul><ul><li>test</li></ul>
````````````

# New - adjacent_lists

## Example undefined

This markdown:

````````````markdown
* This should be
* An unordered list

1. This should be
2. An unordered list

````````````

Should give output:

````````````html
<ul><li>This should be</li><li>An unordered list</li></ul><ol><li>This should be</li><li>An unordered list</li></ol>
````````````

But instead was:

````````````html
<ul><li>This should be</li><li>An unordered list</li></ul><ol></ol><ol start="2"></ol>
````````````

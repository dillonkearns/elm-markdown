# CommonMark - Thematic breaks

## [Example 31](https://spec.commonmark.org/0.29/#example-31)

This markdown:

````````````markdown
- Foo
- * * *

````````````

Should give output:

````````````html
<ul><li>Foo</li><li><hr></li></ul>
````````````

But instead was:

````````````html
<ul><li><hr></li><li>Foo</li></ul>
````````````

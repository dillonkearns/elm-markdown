# CommonMark - Autolinks

## [Example 604](https://spec.commonmark.org/0.29/#example-604)

This markdown:

````````````markdown
< http://foo.bar >

````````````

Should give output:

````````````html
<p>&lt; http://foo.bar &gt;</p>
````````````

But instead was:

````````````html
<p>&lt;<a href="http://foo.bar">http://foo.bar</a>&gt;</p>
````````````
## [Example 607](https://spec.commonmark.org/0.29/#example-607)

This markdown:

````````````markdown
http://example.com

````````````

Should give output:

````````````html
<p>http://example.com</p>
````````````

But instead was:

````````````html
<p><a href="http://example.com">http://example.com</a></p>
````````````

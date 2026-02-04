# CommonMark - Autolinks

## [Example 610](https://spec.commonmark.org/0.30/#example-610)

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
## [Example 611](https://spec.commonmark.org/0.30/#example-611)

This markdown:

````````````markdown
foo@bar.example.com

````````````

Should give output:

````````````html
<p>foo@bar.example.com</p>
````````````

But instead was:

````````````html
<p><a href="mailto:foo@bar.example.com">foo@bar.example.com</a></p>
````````````

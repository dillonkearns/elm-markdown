# CommonMark - ATX headings

## [Example 34](https://spec.commonmark.org/0.29/#example-34)

This markdown:

````````````markdown
#5 bolt

#hashtag

````````````

Should give output:

````````````html
<p>#5 bolt</p><p>#hashtag</p>
````````````

But instead was:

````````````html
<h1>5 bolt</h1><h1>hashtag</h1>
````````````
## [Example 38](https://spec.commonmark.org/0.29/#example-38)

This markdown:

````````````markdown
 ### foo
  ## foo
   # foo

````````````

Should give output:

````````````html
<h3>foo</h3><h2>foo</h2><h1>foo</h1>
````````````

But instead was:

````````````html
<p>### foo ## foo # foo</p>
````````````
## [Example 41](https://spec.commonmark.org/0.29/#example-41)

This markdown:

````````````markdown
## foo ##
  ###   bar    ###

````````````

Should give output:

````````````html
<h2>foo</h2><h3>bar</h3>
````````````

But instead was:

````````````html
<h2>foo</h2><p>### bar ###</p>
````````````
## [Example 43](https://spec.commonmark.org/0.29/#example-43)

This markdown:

````````````markdown
### foo ###     

````````````

Should give output:

````````````html
<h3>foo</h3>
````````````

But instead was:

````````````html
<h3>foo ###</h3>
````````````
## [Example 45](https://spec.commonmark.org/0.29/#example-45)

This markdown:

````````````markdown
# foo#

````````````

Should give output:

````````````html
<h1>foo#</h1>
````````````

But instead was:

````````````html
<h1>foo</h1>
````````````
## [Example 46](https://spec.commonmark.org/0.29/#example-46)

This markdown:

````````````markdown
### foo \###
## foo #\##
# foo \#

````````````

Should give output:

````````````html
<h3>foo ###</h3><h2>foo ###</h2><h1>foo #</h1>
````````````

But instead was:

````````````html
<h3>foo \</h3><h2>foo #\</h2><h1>foo \</h1>
````````````

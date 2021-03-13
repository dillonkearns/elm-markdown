# New - nogfm_hashtag

## Example undefined

This markdown:

````````````markdown
#header

# header1

#  header2

````````````

Should give output:

````````````html
<h1 id="header">header</h1><h1 id="header1">header1</h1><h1 id="header2">header2</h1>
````````````

But instead was:

````````````html
<p>#header</p><h1>header1</h1><h1>header2</h1>
````````````

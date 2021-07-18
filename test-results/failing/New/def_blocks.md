# New - def_blocks

## Example undefined

This markdown:

````````````markdown
> hello
> [1]: hello

* * *

> hello
[2]: hello


* hello
* [3]: hello


* hello
[4]: hello


> foo
> bar
[5]: foo
> bar

````````````

Should give output:

````````````html
<blockquote><p>hello [1]: hello</p></blockquote><hr><blockquote><p>hello [2]: hello</p></blockquote><ul><li>hello</li><li>[3]: hello</li></ul><ul><li>hello</li></ul><blockquote><p>foo bar [5]: foo bar</p></blockquote>
````````````

But instead was:

````````````html
<blockquote><p>hello</p></blockquote><hr><blockquote><p>hello</p></blockquote><ul><li><p>hello</p></li><li></li><li><p>hello</p></li></ul><blockquote><p>foo bar bar</p></blockquote>
````````````

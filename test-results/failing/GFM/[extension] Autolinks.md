# GFM - [extension] Autolinks

## [Example 623](https://github.github.com/gfm/#example-623)

This markdown:

````````````markdown
Visit www.commonmark.org.

Visit www.commonmark.org/a.b.
````````````

Should give output:

````````````html
<p>Visit<a href="http://www.commonmark.org">www.commonmark.org</a>.</p><p>Visit<a href="http://www.commonmark.org/a.b">www.commonmark.org/a.b</a>.</p>
````````````

But instead was:

````````````html
<p>Visit<a href="http://www.commonmark.org">www.commonmark.org</a>.</p><p>Visit<a href="http://www.commonmark.org/a">www.commonmark.org/a</a>.b.</p>
````````````
## [Example 624](https://github.github.com/gfm/#example-624)

This markdown:

````````````markdown
www.google.com/search?q=Markup+(business)

www.google.com/search?q=Markup+(business)))

(www.google.com/search?q=Markup+(business))

(www.google.com/search?q=Markup+(business)
````````````

Should give output:

````````````html
<p><a href="http://www.google.com/search?q=Markup+(business)">www.google.com/search?q=Markup+(business)</a></p><p><a href="http://www.google.com/search?q=Markup+(business)">www.google.com/search?q=Markup+(business)</a>))</p><p>(<a href="http://www.google.com/search?q=Markup+(business)">www.google.com/search?q=Markup+(business)</a>)</p><p>(<a href="http://www.google.com/search?q=Markup+(business)">www.google.com/search?q=Markup+(business)</a></p>
````````````

But instead was:

````````````html
<p><a href="http://www.google.com/search">www.google.com/search</a>?q=Markup+(business)</p><p><a href="http://www.google.com/search">www.google.com/search</a>?q=Markup+(business)))</p><p>(<a href="http://www.google.com/search">www.google.com/search</a>?q=Markup+(business))</p><p>(<a href="http://www.google.com/search">www.google.com/search</a>?q=Markup+(business)</p>
````````````
## [Example 625](https://github.github.com/gfm/#example-625)

This markdown:

````````````markdown
www.google.com/search?q=(business))+ok
````````````

Should give output:

````````````html
<p><a href="http://www.google.com/search?q=(business))+ok">www.google.com/search?q=(business))+ok</a></p>
````````````

But instead was:

````````````html
<p><a href="http://www.google.com/search">www.google.com/search</a>?q=(business))+ok</p>
````````````
## [Example 626](https://github.github.com/gfm/#example-626)

This markdown:

````````````markdown
www.google.com/search?q=commonmark&hl=en

www.google.com/search?q=commonmark&hl;
````````````

Should give output:

````````````html
<p><a href="http://www.google.com/search?q=commonmark&amp;hl=en">www.google.com/search?q=commonmark&amp;hl=en</a></p><p><a href="http://www.google.com/search?q=commonmark">www.google.com/search?q=commonmark</a>&amp;hl;</p>
````````````

But instead was:

````````````html
<p><a href="http://www.google.com/search">www.google.com/search</a>?q=commonmark&amp;hl=en</p><p><a href="http://www.google.com/search">www.google.com/search</a>?q=commonmark&amp;hl;</p>
````````````
## [Example 628](https://github.github.com/gfm/#example-628)

This markdown:

````````````markdown
http://commonmark.org

(Visit https://encrypted.google.com/search?q=Markup+(business))

Anonymous FTP is available at ftp://foo.bar.baz.
````````````

Should give output:

````````````html
<p><a href="http://commonmark.org">http://commonmark.org</a></p><p>(Visit<a href="https://encrypted.google.com/search?q=Markup+(business)">https://encrypted.google.com/search?q=Markup+(business)</a>)</p><p>Anonymous FTP is available at<a href="ftp://foo.bar.baz">ftp://foo.bar.baz</a>.</p>
````````````

But instead was:

````````````html
<p>http://commonmark.org</p><p>(Visit https://encrypted.google.com/search?q=Markup+(business))</p><p>Anonymous FTP is available at ftp://foo.bar.baz.</p>
````````````
## [Example 629](https://github.github.com/gfm/#example-629)

This markdown:

````````````markdown
foo@bar.baz
````````````

Should give output:

````````````html
<p><a href="mailto:foo@bar.baz">foo@bar.baz</a></p>
````````````

But instead was:

````````````html
<p>foo@bar.baz</p>
````````````
## [Example 630](https://github.github.com/gfm/#example-630)

This markdown:

````````````markdown
hello@mail+xyz.example isn't valid, but hello+xyz@mail.example is.
````````````

Should give output:

````````````html
<p>hello@mail+xyz.example isn&#39;t valid, but<a href="mailto:hello+xyz@mail.example">hello+xyz@mail.example</a>is.</p>
````````````

But instead was:

````````````html
<p>hello@mail+xyz.example isn&#39;t valid, but hello+xyz@mail.example is.</p>
````````````
## [Example 631](https://github.github.com/gfm/#example-631)

This markdown:

````````````markdown
a.b-c_d@a.b

a.b-c_d@a.b.

a.b-c_d@a.b-

a.b-c_d@a.b_
````````````

Should give output:

````````````html
<p><a href="mailto:a.b-c_d@a.b">a.b-c_d@a.b</a></p><p><a href="mailto:a.b-c_d@a.b">a.b-c_d@a.b</a>.</p><p>a.b-c_d@a.b-</p><p>a.b-c_d@a.b_</p>
````````````

But instead was:

````````````html
<p>a.b-c_d@a.b</p><p>a.b-c_d@a.b.</p><p>a.b-c_d@a.b-</p><p>a.b-c_d@a.b_</p>
````````````

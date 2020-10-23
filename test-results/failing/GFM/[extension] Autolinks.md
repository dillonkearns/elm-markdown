# GFM - [extension] Autolinks

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
<p><a href="http://commonmark.org">http://commonmark.org</a></p><p>(Visit<a href="https://encrypted.google.com/search?q=Markup+(business)">https://encrypted.google.com/search?q=Markup+(business)</a>)</p><p>Anonymous FTP is available at ftp://foo.bar.baz.</p>
````````````

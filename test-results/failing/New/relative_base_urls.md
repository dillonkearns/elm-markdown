# New - relative_base_urls

## Example undefined

This markdown:

````````````markdown
# Absolutization of RFC 3986 URIs

## Absolute URI
[![section 4.3](http://example.com/logo)](http://example.com/)

## Network-path reference
[![section 4.2](//example.com/logo)](//example.com/)

## Absolute path
[![section 4.2](/path/to/img)](/path/to/content)

## Relative path
[![section 4.2](img)](content)

## Dot-relative path
[![section 3.3](./img)](./content)

[![section 3.3](../img)](../content)

## Same-document query
[![section 4.4](?type=image)](?)

## Same-document fragment
[![section 4.4](#img)](#)

## Empty
[section 4.2]()

````````````

Should give output:

````````````html
<h1 id="absolutization-of-rfc-3986-uris">Absolutization of RFC 3986 URIs</h1><h2 id="absolute-uri">Absolute URI</h2><p><a href="http://example.com/"><img alt="section 4.3" src="http://example.com/logo"></a></p><h2 id="network-path-reference">Network-path reference</h2><p><a href="//example.com/"><img alt="section 4.2" src="//example.com/logo"></a></p><h2 id="absolute-path">Absolute path</h2><p><a href="/path/to/content"><img alt="section 4.2" src="/path/to/img"></a></p><h2 id="relative-path">Relative path</h2><p><a href="/base/content"><img alt="section 4.2" src="/base/img"></a></p><h2 id="dot-relative-path">Dot-relative path</h2><p><a href="/base/./content"><img alt="section 3.3" src="/base/./img"></a></p><p><a href="/base/../content"><img alt="section 3.3" src="/base/../img"></a></p><h2 id="same-document-query">Same-document query</h2><p><a href="?"><img alt="section 4.4" src="?type=image"></a></p><h2 id="same-document-fragment">Same-document fragment</h2><p><a href="#"><img alt="section 4.4" src="#img"></a></p><h2 id="empty">Empty</h2><p><a href="">section 4.2</a></p>
````````````

But instead was:

````````````html
<h1>Absolutization of RFC 3986 URIs</h1><h2>Absolute URI</h2><p><a href="http://example.com/"><img alt="section 4.3" src="http://example.com/logo"></a></p><h2>Network-path reference</h2><p><a href="//example.com/"><img alt="section 4.2" src="//example.com/logo"></a></p><h2>Absolute path</h2><p><a href="/path/to/content"><img alt="section 4.2" src="/path/to/img"></a></p><h2>Relative path</h2><p><a href="content"><img alt="section 4.2" src="img"></a></p><h2>Dot-relative path</h2><p><a href="./content"><img alt="section 3.3" src="./img"></a></p><p><a href="../content"><img alt="section 3.3" src="../img"></a></p><h2>Same-document query</h2><p><a href="?"><img alt="section 4.4" src="?type=image"></a></p><h2>Same-document fragment</h2><p><a href="#"><img alt="section 4.4" src="#img"></a></p><h2>Empty</h2><p><a href="">section 4.2</a></p>
````````````

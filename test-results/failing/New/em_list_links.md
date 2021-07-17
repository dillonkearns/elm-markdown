# New - em_list_links

## Example undefined

This markdown:

````````````markdown
- italic
  - [*named link*][some-url]
  - *[named link][some-url]*
  - [_named link_][some-url]
  - _[named link][some-url]_
- bold
  - [**named link**][some-url]
  - **[named link][some-url]**
  - [__named link__][some-url]
  - __[named link][some-url]__
- bold italic
  - [***named link***][some-url]
  - ***[named link][some-url]***
  - [___named link___][some-url]
  - ___[named link][some-url]___
  - [*__named link__*][some-url]
  - [__*named link*__][some-url]
  - __*[named link][some-url]*__
- code
  - [`named link`][some-url]
- code italic
  - *[`named link`][some-url]*
  - [*`named link`*][some-url]
  - _[`named link`][some-url]_
  - [_`named link`_][some-url]
- code bold
  - **[`named link`][some-url]**
  - [**`named link`**][some-url]
  - __[`named link`][some-url]__
  - [__`named link`__][some-url]
- code bold italic
  - [***`named link`***][some-url]
  - ***[`named link`][some-url]***
  - [___`named link`___][some-url]
  - ___[`named link`][some-url]___
  - [*__`named link`__*][some-url]
  - [__*`named link`*__][some-url]
  - __*[`named link`][some-url]*__

[some-url]: https://www.google.com
````````````

Should give output:

````````````html
<ul><li>italic<ul><li><a href="https://www.google.com"><em>named link</em></a></li><li><em><a href="https://www.google.com">named link</a></em></li><li><a href="https://www.google.com"><em>named link</em></a></li><li><em><a href="https://www.google.com">named link</a></em></li></ul></li><li>bold<ul><li><a href="https://www.google.com"><strong>named link</strong></a></li><li><strong><a href="https://www.google.com">named link</a></strong></li><li><a href="https://www.google.com"><strong>named link</strong></a></li><li><strong><a href="https://www.google.com">named link</a></strong></li></ul></li><li>bold italic<ul><li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li><li><strong><em><a href="https://www.google.com">named link</a></em></strong></li><li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li><li><strong><em><a href="https://www.google.com">named link</a></em></strong></li><li><a href="https://www.google.com"><em><strong>named link</strong></em></a></li><li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li><li><strong><em><a href="https://www.google.com">named link</a></em></strong></li></ul></li><li>code<ul><li><a href="https://www.google.com"><code>named link</code></a></li></ul></li><li>code italic<ul><li><em><a href="https://www.google.com"><code>named link</code></a></em></li><li><a href="https://www.google.com"><em><code>named link</code></em></a></li><li><em><a href="https://www.google.com"><code>named link</code></a></em></li><li><a href="https://www.google.com"><em><code>named link</code></em></a></li></ul></li><li>code bold<ul><li><strong><a href="https://www.google.com"><code>named link</code></a></strong></li><li><a href="https://www.google.com"><strong><code>named link</code></strong></a></li><li><strong><a href="https://www.google.com"><code>named link</code></a></strong></li><li><a href="https://www.google.com"><strong><code>named link</code></strong></a></li></ul></li><li>code bold italic<ul><li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li><li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li><li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li><li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li><li><a href="https://www.google.com"><em><strong><code>named link</code></strong></em></a></li><li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li><li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li></ul></li></ul>
````````````

But instead was:

````````````html
<ul><li>italic<ul><li><a href="https://www.google.com"><em>named link</em></a></li><li><em><a href="https://www.google.com">named link</a></em></li><li><a href="https://www.google.com"><em>named link</em></a></li><li><em><a href="https://www.google.com">named link</a></em></li></ul></li><li>bold<ul><li><a href="https://www.google.com"><strong>named link</strong></a></li><li><strong><a href="https://www.google.com">named link</a></strong></li><li><a href="https://www.google.com"><strong>named link</strong></a></li><li><strong><a href="https://www.google.com">named link</a></strong></li></ul></li><li>bold italic<ul><li><a href="https://www.google.com"><em><strong>named link</strong></em></a></li><li><em><strong><a href="https://www.google.com">named link</a></strong></em></li><li><a href="https://www.google.com"><em><strong>named link</strong></em></a></li><li><em><strong><a href="https://www.google.com">named link</a></strong></em></li><li><a href="https://www.google.com"><em><strong>named link</strong></em></a></li><li><a href="https://www.google.com"><strong><em>named link</em></strong></a></li><li><strong><em><a href="https://www.google.com">named link</a></em></strong></li></ul></li><li>code<ul><li><a href="https://www.google.com"><code>named link</code></a></li></ul></li><li>code italic<ul><li><em><a href="https://www.google.com"><code>named link</code></a></em></li><li><a href="https://www.google.com"><em><code>named link</code></em></a></li><li><em><a href="https://www.google.com"><code>named link</code></a></em></li><li><a href="https://www.google.com"><em><code>named link</code></em></a></li></ul></li><li>code bold<ul><li><strong><a href="https://www.google.com"><code>named link</code></a></strong></li><li><a href="https://www.google.com"><strong><code>named link</code></strong></a></li><li><strong><a href="https://www.google.com"><code>named link</code></a></strong></li><li><a href="https://www.google.com"><strong><code>named link</code></strong></a></li></ul></li><li>code bold italic<ul><li><a href="https://www.google.com"><em><strong><code>named link</code></strong></em></a></li><li><em><strong><a href="https://www.google.com"><code>named link</code></a></strong></em></li><li><a href="https://www.google.com"><em><strong><code>named link</code></strong></em></a></li><li><em><strong><a href="https://www.google.com"><code>named link</code></a></strong></em></li><li><a href="https://www.google.com"><em><strong><code>named link</code></strong></em></a></li><li><a href="https://www.google.com"><strong><em><code>named link</code></em></strong></a></li><li><strong><em><a href="https://www.google.com"><code>named link</code></a></em></strong></li></ul></li></ul>
````````````

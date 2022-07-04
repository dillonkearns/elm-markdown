# New - main

## Example undefined

This markdown:

````````````markdown
[test]: http://google.com/ "Google"

# A heading

Just a note, I've found that I can't test my markdown parser vs others.
For example, both markdown.js and showdown code blocks in lists wrong. They're
also completely [inconsistent][test] with regards to paragraphs in list items.

A link. Not anymore.

<aside>This will make me fail the test because
markdown.js doesnt acknowledge arbitrary html blocks =/</aside>

* List Item 1

* List Item 2
  * New List Item 1
    Hi, this is a list item.
  * New List Item 2
    Another item
        Code goes here.
        Lots of it...
  * New List Item 3
    The last item

* List Item 3
The final item.

* List Item 4
The real final item.

Paragraph.

> * bq Item 1
> * bq Item 2
>   * New bq Item 1
>   * New bq Item 2
>   Text here

* * *

> Another blockquote!
> I really need to get
> more creative with
> mockup text..
> markdown.js breaks here again

Another Heading
-------------

Hello *world*. Here is a [link](//hello).
And an image ![alt](src).

    Code goes here.
    Lots of it...

````````````

Should give output:

````````````html
<h1 id="a-heading">A heading</h1><p>Just a note, I&#39;ve found that I can&#39;t test my markdown parser vs others. For example, both markdown.js and showdown code blocks in lists wrong. They&#39;re also completely<a href="http://google.com/" title="Google">inconsistent</a>with regards to paragraphs in list items.</p><p>A link. Not anymore.</p><aside>This will make me fail the test because markdown.js doesnt acknowledge arbitrary html blocks =/</aside><ul><li><p>List Item 1</p></li><li><p>List Item 2</p><ul><li>New List Item 1 Hi, this is a list item.</li><li>New List Item 2 Another item<pre><code>Code goes here. Lots of it...</code></pre></li><li>New List Item 3 The last item</li></ul></li><li><p>List Item 3 The final item.</p></li><li><p>List Item 4 The real final item.</p></li></ul><p>Paragraph.</p><blockquote><ul><li>bq Item 1</li><li>bq Item 2<ul><li>New bq Item 1</li><li>New bq Item 2 Text here</li></ul></li></ul></blockquote><hr><blockquote><p>Another blockquote! I really need to get more creative with mockup text.. markdown.js breaks here again</p></blockquote><h2 id="another-heading">Another Heading</h2><p>Hello<em>world</em>. Here is a<a href="//hello">link</a>. And an image<img alt="alt" src="src">.</p><pre><code>Code goes here. Lots of it...</code></pre>
````````````

But instead was:

````````````html
<h1>A heading</h1><p>Just a note, I&#39;ve found that I can&#39;t test my markdown parser vs others. For example, both markdown.js and showdown code blocks in lists wrong. They&#39;re also completely<a href="http://google.com/" title="Google">inconsistent</a>with regards to paragraphs in list items.</p><p>A link. Not anymore.</p><aside><p>This will make me fail the test because markdown.js doesnt acknowledge arbitrary html blocks =/</p></aside><ul><li><p>List Item 1</p></li><li><p>List Item 2</p><ul><li>New List Item 1 Hi, this is a list item.</li><li>New List Item 2 Another item Code goes here. Lots of it...</li><li>New List Item 3 The last item</li></ul></li><li><p>List Item 3 The final item.</p></li><li><p>List Item 4 The real final item.</p></li></ul><p>Paragraph.</p><blockquote><ul><li>bq Item 1</li><li>bq Item 2<ul><li>New bq Item 1</li><li>New bq Item 2 Text here</li></ul></li></ul></blockquote><hr><blockquote><p>Another blockquote! I really need to get more creative with mockup text.. markdown.js breaks here again</p></blockquote><h2>Another Heading</h2><p>Hello<em>world</em>. Here is a<a href="//hello">link</a>. And an image<img alt="alt" src="src">.</p><pre><code>Code goes here. Lots of it...</code></pre>
````````````

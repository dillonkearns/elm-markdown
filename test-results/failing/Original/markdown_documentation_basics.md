# Original - markdown_documentation_basics

## Example undefined

This markdown:

````````````markdown
Markdown: Basics
================

<ul id="ProjectSubmenu">
    <li><a href="/projects/markdown/" title="Markdown Project Page">Main</a></li>
    <li><a class="selected" title="Markdown Basics">Basics</a></li>
    <li><a href="/projects/markdown/syntax" title="Markdown Syntax Documentation">Syntax</a></li>
    <li><a href="/projects/markdown/license" title="Pricing and License Information">License</a></li>
    <li><a href="/projects/markdown/dingus" title="Online Markdown Web Form">Dingus</a></li>
</ul>


Getting the Gist of Markdown's Formatting Syntax
------------------------------------------------

This page offers a brief overview of what it's like to use Markdown.
The [syntax page] [s] provides complete, detailed documentation for
every feature, but Markdown should be very easy to pick up simply by
looking at a few examples of it in action. The examples on this page
are written in a before/after style, showing example syntax and the
HTML output produced by Markdown.

It's also helpful to simply try Markdown out; the [Dingus] [d] is a
web application that allows you type your own Markdown-formatted text
and translate it to XHTML.

**Note:** This document is itself written using Markdown; you
can [see the source for it by adding '.text' to the URL] [src].

  [s]: /projects/markdown/syntax  "Markdown Syntax"
  [d]: /projects/markdown/dingus  "Markdown Dingus"
  [src]: /projects/markdown/basics.text


## Paragraphs, Headers, Blockquotes ##

A paragraph is simply one or more consecutive lines of text, separated
by one or more blank lines. (A blank line is any line that looks like a
blank line -- a line containing nothing spaces or tabs is considered
blank.) Normal paragraphs should not be intended with spaces or tabs.

Markdown offers two styles of headers: *Setext* and *atx*.
Setext-style headers for `<h1>` and `<h2>` are created by
"underlining" with equal signs (`=`) and hyphens (`-`), respectively.
To create an atx-style header, you put 1-6 hash marks (`#`) at the
beginning of the line -- the number of hashes equals the resulting
HTML header level.

Blockquotes are indicated using email-style '`>`' angle brackets.

Markdown:

    A First Level Header
    ====================
    
    A Second Level Header
    ---------------------

    Now is the time for all good men to come to
    the aid of their country. This is just a
    regular paragraph.

    The quick brown fox jumped over the lazy
    dog's back.
    
    ### Header 3

    > This is a blockquote.
    > 
    > This is the second paragraph in the blockquote.
    >
    > ## This is an H2 in a blockquote


Output:

    <h1>A First Level Header</h1>
    
    <h2>A Second Level Header</h2>
    
    <p>Now is the time for all good men to come to
    the aid of their country. This is just a
    regular paragraph.</p>
    
    <p>The quick brown fox jumped over the lazy
    dog's back.</p>
    
    <h3>Header 3</h3>
    
    <blockquote>
        <p>This is a blockquote.</p>
        
        <p>This is the second paragraph in the blockquote.</p>
        
        <h2>This is an H2 in a blockquote</h2>
    </blockquote>



### Phrase Emphasis ###

Markdown uses asterisks and underscores to indicate spans of emphasis.

Markdown:

    Some of these words *are emphasized*.
    Some of these words _are emphasized also_.
    
    Use two asterisks for **strong emphasis**.
    Or, if you prefer, __use two underscores instead__.

Output:

    <p>Some of these words <em>are emphasized</em>.
    Some of these words <em>are emphasized also</em>.</p>
    
    <p>Use two asterisks for <strong>strong emphasis</strong>.
    Or, if you prefer, <strong>use two underscores instead</strong>.</p>
   


## Lists ##

Unordered (bulleted) lists use asterisks, pluses, and hyphens (`*`,
`+`, and `-`) as list markers. These three markers are
interchangable; this:

    *   Candy.
    *   Gum.
    *   Booze.

this:

    +   Candy.
    +   Gum.
    +   Booze.

and this:

    -   Candy.
    -   Gum.
    -   Booze.

all produce the same output:

    <ul>
    <li>Candy.</li>
    <li>Gum.</li>
    <li>Booze.</li>
    </ul>

Ordered (numbered) lists use regular numbers, followed by periods, as
list markers:

    1.  Red
    2.  Green
    3.  Blue

Output:

    <ol>
    <li>Red</li>
    <li>Green</li>
    <li>Blue</li>
    </ol>

If you put blank lines between items, you'll get `<p>` tags for the
list item text. You can create multi-paragraph list items by indenting
the paragraphs by 4 spaces or 1 tab:

    *   A list item.
    
        With multiple paragraphs.

    *   Another item in the list.

Output:

    <ul>
    <li><p>A list item.</p>
    <p>With multiple paragraphs.</p></li>
    <li><p>Another item in the list.</p></li>
    </ul>
    


### Links ###

Markdown supports two styles for creating links: *inline* and
*reference*. With both styles, you use square brackets to delimit the
text you want to turn into a link.

Inline-style links use parentheses immediately after the link text.
For example:

    This is an [example link](http://example.com/).

Output:

    <p>This is an <a href="http://example.com/">
    example link</a>.</p>

Optionally, you may include a title attribute in the parentheses:

    This is an [example link](http://example.com/ "With a Title").

Output:

    <p>This is an <a href="http://example.com/" title="With a Title">
    example link</a>.</p>

Reference-style links allow you to refer to your links by names, which
you define elsewhere in your document:

    I get 10 times more traffic from [Google][1] than from
    [Yahoo][2] or [MSN][3].

    [1]: http://google.com/        "Google"
    [2]: http://search.yahoo.com/  "Yahoo Search"
    [3]: http://search.msn.com/    "MSN Search"

Output:

    <p>I get 10 times more traffic from <a href="http://google.com/"
    title="Google">Google</a> than from <a href="http://search.yahoo.com/"
    title="Yahoo Search">Yahoo</a> or <a href="http://search.msn.com/"
    title="MSN Search">MSN</a>.</p>

The title attribute is optional. Link names may contain letters,
numbers and spaces, but are *not* case sensitive:

    I start my morning with a cup of coffee and
    [The New York Times][NY Times].

    [ny times]: http://www.nytimes.com/

Output:

    <p>I start my morning with a cup of coffee and
    <a href="http://www.nytimes.com/">The New York Times</a>.</p>


### Images ###

Image syntax is very much like link syntax.

Inline (titles are optional):

    ![alt text](/path/to/img.jpg "Title")

Reference-style:

    ![alt text][id]

    [id]: /path/to/img.jpg "Title"

Both of the above examples produce the same output:

    <img src="/path/to/img.jpg" alt="alt text" title="Title" />



### Code ###

In a regular paragraph, you can create code span by wrapping text in
backtick quotes. Any ampersands (`&`) and angle brackets (`<` or
`>`) will automatically be translated into HTML entities. This makes
it easy to use Markdown to write about HTML example code:

    I strongly recommend against using any `<blink>` tags.

    I wish SmartyPants used named entities like `&mdash;`
    instead of decimal-encoded entites like `&#8212;`.

Output:

    <p>I strongly recommend against using any
    <code>&lt;blink&gt;</code> tags.</p>
    
    <p>I wish SmartyPants used named entities like
    <code>&amp;mdash;</code> instead of decimal-encoded
    entites like <code>&amp;#8212;</code>.</p>


To specify an entire block of pre-formatted code, indent every line of
the block by 4 spaces or 1 tab. Just like with code spans, `&`, `<`,
and `>` characters will be escaped automatically.

Markdown:

    If you want your page to validate under XHTML 1.0 Strict,
    you've got to put paragraph tags in your blockquotes:

        <blockquote>
            <p>For example.</p>
        </blockquote>

Output:

    <p>If you want your page to validate under XHTML 1.0 Strict,
    you've got to put paragraph tags in your blockquotes:</p>
    
    <pre><code>&lt;blockquote&gt;
        &lt;p&gt;For example.&lt;/p&gt;
    &lt;/blockquote&gt;
    </code></pre>

````````````

Should give output:

````````````html
<h1 id="markdown-basics">Markdown: Basics</h1><ul id="ProjectSubmenu"><li><a href="/projects/markdown/" title="Markdown Project Page">Main</a></li><li><a class="selected" title="Markdown Basics">Basics</a></li><li><a href="/projects/markdown/syntax" title="Markdown Syntax Documentation">Syntax</a></li><li><a href="/projects/markdown/license" title="Pricing and License Information">License</a></li><li><a href="/projects/markdown/dingus" title="Online Markdown Web Form">Dingus</a></li></ul><h2 id="getting-the-gist-of-markdowns-formatting-syntax">Getting the Gist of Markdown&#39;s Formatting Syntax</h2><p>This page offers a brief overview of what it&#39;s like to use Markdown. The<a href="/projects/markdown/syntax" title="Markdown Syntax">syntax page</a>provides complete, detailed documentation for every feature, but Markdown should be very easy to pick up simply by looking at a few examples of it in action. The examples on this page are written in a before/after style, showing example syntax and the HTML output produced by Markdown.</p><p>It&#39;s also helpful to simply try Markdown out; the<a href="/projects/markdown/dingus" title="Markdown Dingus">Dingus</a>is a web application that allows you type your own Markdown-formatted text and translate it to XHTML.</p><p><strong>Note:</strong>This document is itself written using Markdown; you can<a href="/projects/markdown/basics.text">see the source for it by adding &#39;.text&#39; to the URL</a>.</p><h2 id="paragraphs-headers-blockquotes">Paragraphs, Headers, Blockquotes</h2><p>A paragraph is simply one or more consecutive lines of text, separated by one or more blank lines. (A blank line is any line that looks like a blank line -- a line containing nothing spaces or tabs is considered blank.) Normal paragraphs should not be intended with spaces or tabs.</p><p>Markdown offers two styles of headers:<em>Setext</em>and<em>atx</em>. Setext-style headers for<code>&lt;h1&gt;</code>and<code>&lt;h2&gt;</code>are created by &quot;underlining&quot; with equal signs (<code>=</code>) and hyphens (<code>-</code>), respectively. To create an atx-style header, you put 1-6 hash marks (<code>#</code>) at the beginning of the line -- the number of hashes equals the resulting HTML header level.</p><p>Blockquotes are indicated using email-style &#39;<code>&gt;</code>&#39; angle brackets.</p><p>Markdown:</p><pre><code>A First Level Header ==================== A Second Level Header --------------------- Now is the time for all good men to come to the aid of their country. This is just a regular paragraph. The quick brown fox jumped over the lazy dog&#39;s back. ### Header 3 &gt; This is a blockquote. &gt; &gt; This is the second paragraph in the blockquote. &gt; &gt; ## This is an H2 in a blockquote</code></pre><p>Output:</p><pre><code>&lt;h1&gt;A First Level Header&lt;/h1&gt; &lt;h2&gt;A Second Level Header&lt;/h2&gt; &lt;p&gt;Now is the time for all good men to come to the aid of their country. This is just a regular paragraph.&lt;/p&gt; &lt;p&gt;The quick brown fox jumped over the lazy dog&#39;s back.&lt;/p&gt; &lt;h3&gt;Header 3&lt;/h3&gt; &lt;blockquote&gt; &lt;p&gt;This is a blockquote.&lt;/p&gt; &lt;p&gt;This is the second paragraph in the blockquote.&lt;/p&gt; &lt;h2&gt;This is an H2 in a blockquote&lt;/h2&gt; &lt;/blockquote&gt;</code></pre><h3 id="phrase-emphasis">Phrase Emphasis</h3><p>Markdown uses asterisks and underscores to indicate spans of emphasis.</p><p>Markdown:</p><pre><code>Some of these words *are emphasized*. Some of these words _are emphasized also_. Use two asterisks for **strong emphasis**. Or, if you prefer, __use two underscores instead__.</code></pre><p>Output:</p><pre><code>&lt;p&gt;Some of these words &lt;em&gt;are emphasized&lt;/em&gt;. Some of these words &lt;em&gt;are emphasized also&lt;/em&gt;.&lt;/p&gt; &lt;p&gt;Use two asterisks for &lt;strong&gt;strong emphasis&lt;/strong&gt;. Or, if you prefer, &lt;strong&gt;use two underscores instead&lt;/strong&gt;.&lt;/p&gt;</code></pre><h2 id="lists">Lists</h2><p>Unordered (bulleted) lists use asterisks, pluses, and hyphens (<code>*</code>,<code>+</code>, and<code>-</code>) as list markers. These three markers are interchangable; this:</p><pre><code>* Candy. * Gum. * Booze.</code></pre><p>this:</p><pre><code>+ Candy. + Gum. + Booze.</code></pre><p>and this:</p><pre><code>- Candy. - Gum. - Booze.</code></pre><p>all produce the same output:</p><pre><code>&lt;ul&gt; &lt;li&gt;Candy.&lt;/li&gt; &lt;li&gt;Gum.&lt;/li&gt; &lt;li&gt;Booze.&lt;/li&gt; &lt;/ul&gt;</code></pre><p>Ordered (numbered) lists use regular numbers, followed by periods, as list markers:</p><pre><code>1. Red 2. Green 3. Blue</code></pre><p>Output:</p><pre><code>&lt;ol&gt; &lt;li&gt;Red&lt;/li&gt; &lt;li&gt;Green&lt;/li&gt; &lt;li&gt;Blue&lt;/li&gt; &lt;/ol&gt;</code></pre><p>If you put blank lines between items, you&#39;ll get<code>&lt;p&gt;</code>tags for the list item text. You can create multi-paragraph list items by indenting the paragraphs by 4 spaces or 1 tab:</p><pre><code>* A list item. With multiple paragraphs. * Another item in the list.</code></pre><p>Output:</p><pre><code>&lt;ul&gt; &lt;li&gt;&lt;p&gt;A list item.&lt;/p&gt; &lt;p&gt;With multiple paragraphs.&lt;/p&gt;&lt;/li&gt; &lt;li&gt;&lt;p&gt;Another item in the list.&lt;/p&gt;&lt;/li&gt; &lt;/ul&gt;</code></pre><h3 id="links">Links</h3><p>Markdown supports two styles for creating links:<em>inline</em>and<em>reference</em>. With both styles, you use square brackets to delimit the text you want to turn into a link.</p><p>Inline-style links use parentheses immediately after the link text. For example:</p><pre><code>This is an [example link](http://example.com/).</code></pre><p>Output:</p><pre><code>&lt;p&gt;This is an &lt;a href=&quot;http://example.com/&quot;&gt; example link&lt;/a&gt;.&lt;/p&gt;</code></pre><p>Optionally, you may include a title attribute in the parentheses:</p><pre><code>This is an [example link](http://example.com/ &quot;With a Title&quot;).</code></pre><p>Output:</p><pre><code>&lt;p&gt;This is an &lt;a href=&quot;http://example.com/&quot; title=&quot;With a Title&quot;&gt; example link&lt;/a&gt;.&lt;/p&gt;</code></pre><p>Reference-style links allow you to refer to your links by names, which you define elsewhere in your document:</p><pre><code>I get 10 times more traffic from [Google][1] than from [Yahoo][2] or [MSN][3]. [1]: http://google.com/ &quot;Google&quot; [2]: http://search.yahoo.com/ &quot;Yahoo Search&quot; [3]: http://search.msn.com/ &quot;MSN Search&quot;</code></pre><p>Output:</p><pre><code>&lt;p&gt;I get 10 times more traffic from &lt;a href=&quot;http://google.com/&quot; title=&quot;Google&quot;&gt;Google&lt;/a&gt; than from &lt;a href=&quot;http://search.yahoo.com/&quot; title=&quot;Yahoo Search&quot;&gt;Yahoo&lt;/a&gt; or &lt;a href=&quot;http://search.msn.com/&quot; title=&quot;MSN Search&quot;&gt;MSN&lt;/a&gt;.&lt;/p&gt;</code></pre><p>The title attribute is optional. Link names may contain letters, numbers and spaces, but are<em>not</em>case sensitive:</p><pre><code>I start my morning with a cup of coffee and [The New York Times][NY Times]. [ny times]: http://www.nytimes.com/</code></pre><p>Output:</p><pre><code>&lt;p&gt;I start my morning with a cup of coffee and &lt;a href=&quot;http://www.nytimes.com/&quot;&gt;The New York Times&lt;/a&gt;.&lt;/p&gt;</code></pre><h3 id="images">Images</h3><p>Image syntax is very much like link syntax.</p><p>Inline (titles are optional):</p><pre><code>![alt text](/path/to/img.jpg &quot;Title&quot;)</code></pre><p>Reference-style:</p><pre><code>![alt text][id] [id]: /path/to/img.jpg &quot;Title&quot;</code></pre><p>Both of the above examples produce the same output:</p><pre><code>&lt;img src=&quot;/path/to/img.jpg&quot; alt=&quot;alt text&quot; title=&quot;Title&quot; /&gt;</code></pre><h3 id="code">Code</h3><p>In a regular paragraph, you can create code span by wrapping text in backtick quotes. Any ampersands (<code>&amp;</code>) and angle brackets (<code>&lt;</code>or<code>&gt;</code>) will automatically be translated into HTML entities. This makes it easy to use Markdown to write about HTML example code:</p><pre><code>I strongly recommend against using any `&lt;blink&gt;` tags. I wish SmartyPants used named entities like `&amp;mdash;` instead of decimal-encoded entites like `&amp;#8212;`.</code></pre><p>Output:</p><pre><code>&lt;p&gt;I strongly recommend against using any &lt;code&gt;&amp;lt;blink&amp;gt;&lt;/code&gt; tags.&lt;/p&gt; &lt;p&gt;I wish SmartyPants used named entities like &lt;code&gt;&amp;amp;mdash;&lt;/code&gt; instead of decimal-encoded entites like &lt;code&gt;&amp;amp;#8212;&lt;/code&gt;.&lt;/p&gt;</code></pre><p>To specify an entire block of pre-formatted code, indent every line of the block by 4 spaces or 1 tab. Just like with code spans,<code>&amp;</code>,<code>&lt;</code>, and<code>&gt;</code>characters will be escaped automatically.</p><p>Markdown:</p><pre><code>If you want your page to validate under XHTML 1.0 Strict, you&#39;ve got to put paragraph tags in your blockquotes: &lt;blockquote&gt; &lt;p&gt;For example.&lt;/p&gt; &lt;/blockquote&gt;</code></pre><p>Output:</p><pre><code>&lt;p&gt;If you want your page to validate under XHTML 1.0 Strict, you&#39;ve got to put paragraph tags in your blockquotes:&lt;/p&gt; &lt;pre&gt;&lt;code&gt;&amp;lt;blockquote&amp;gt; &amp;lt;p&amp;gt;For example.&amp;lt;/p&amp;gt; &amp;lt;/blockquote&amp;gt; &lt;/code&gt;&lt;/pre&gt;</code></pre>
````````````

But instead was:

````````````html
<h1>Markdown: Basics</h1><ul id="ProjectSubmenu"><pre><code></code></pre><li><a href="/projects/markdown/" title="Markdown Project Page"><p>Main</p></a></li><pre><code></code></pre><li><a class="selected" title="Markdown Basics"><p>Basics</p></a></li><pre><code></code></pre><li><a href="/projects/markdown/syntax" title="Markdown Syntax Documentation"><p>Syntax</p></a></li><pre><code></code></pre><li><a href="/projects/markdown/license" title="Pricing and License Information"><p>License</p></a></li><pre><code></code></pre><li><a href="/projects/markdown/dingus" title="Online Markdown Web Form"><p>Dingus</p></a></li></ul><h2>Getting the Gist of Markdown&#39;s Formatting Syntax</h2><p>This page offers a brief overview of what it&#39;s like to use Markdown. The [syntax page]<a href="/projects/markdown/syntax" title="Markdown Syntax">s</a>provides complete, detailed documentation for every feature, but Markdown should be very easy to pick up simply by looking at a few examples of it in action. The examples on this page are written in a before/after style, showing example syntax and the HTML output produced by Markdown.</p><p>It&#39;s also helpful to simply try Markdown out; the [Dingus]<a href="/projects/markdown/dingus" title="Markdown Dingus">d</a>is a web application that allows you type your own Markdown-formatted text and translate it to XHTML.</p><p><strong>Note:</strong>This document is itself written using Markdown; you can [see the source for it by adding &#39;.text&#39; to the URL]<a href="/projects/markdown/basics.text">src</a>.</p><h2>Paragraphs, Headers, Blockquotes</h2><p>A paragraph is simply one or more consecutive lines of text, separated by one or more blank lines. (A blank line is any line that looks like a blank line -- a line containing nothing spaces or tabs is considered blank.) Normal paragraphs should not be intended with spaces or tabs.</p><p>Markdown offers two styles of headers:<em>Setext</em>and<em>atx</em>. Setext-style headers for<code>&lt;h1&gt;</code>and<code>&lt;h2&gt;</code>are created by &quot;underlining&quot; with equal signs (<code>=</code>) and hyphens (<code>-</code>), respectively. To create an atx-style header, you put 1-6 hash marks (<code>#</code>) at the beginning of the line -- the number of hashes equals the resulting HTML header level.</p><p>Blockquotes are indicated using email-style &#39;<code>&gt;</code>&#39; angle brackets.</p><p>Markdown:</p><pre><code>A First Level Header ==================== A Second Level Header --------------------- Now is the time for all good men to come to the aid of their country. This is just a regular paragraph. The quick brown fox jumped over the lazy dog&#39;s back. ### Header 3 &gt; This is a blockquote. &gt; &gt; This is the second paragraph in the blockquote. &gt; &gt; ## This is an H2 in a blockquote</code></pre><p>Output:</p><pre><code>&lt;h1&gt;A First Level Header&lt;/h1&gt; &lt;h2&gt;A Second Level Header&lt;/h2&gt; &lt;p&gt;Now is the time for all good men to come to the aid of their country. This is just a regular paragraph.&lt;/p&gt; &lt;p&gt;The quick brown fox jumped over the lazy dog&#39;s back.&lt;/p&gt; &lt;h3&gt;Header 3&lt;/h3&gt; &lt;blockquote&gt; &lt;p&gt;This is a blockquote.&lt;/p&gt; &lt;p&gt;This is the second paragraph in the blockquote.&lt;/p&gt; &lt;h2&gt;This is an H2 in a blockquote&lt;/h2&gt; &lt;/blockquote&gt;</code></pre><h3>Phrase Emphasis</h3><p>Markdown uses asterisks and underscores to indicate spans of emphasis.</p><p>Markdown:</p><pre><code>Some of these words *are emphasized*. Some of these words _are emphasized also_. Use two asterisks for **strong emphasis**. Or, if you prefer, __use two underscores instead__.</code></pre><p>Output:</p><pre><code>&lt;p&gt;Some of these words &lt;em&gt;are emphasized&lt;/em&gt;. Some of these words &lt;em&gt;are emphasized also&lt;/em&gt;.&lt;/p&gt; &lt;p&gt;Use two asterisks for &lt;strong&gt;strong emphasis&lt;/strong&gt;. Or, if you prefer, &lt;strong&gt;use two underscores instead&lt;/strong&gt;.&lt;/p&gt;</code></pre><h2>Lists</h2><p>Unordered (bulleted) lists use asterisks, pluses, and hyphens (<code>*</code>,<code>+</code>, and<code>-</code>) as list markers. These three markers are interchangable; this:</p><pre><code>* Candy. * Gum. * Booze.</code></pre><p>this:</p><pre><code>+ Candy. + Gum. + Booze.</code></pre><p>and this:</p><pre><code>- Candy. - Gum. - Booze.</code></pre><p>all produce the same output:</p><pre><code>&lt;ul&gt; &lt;li&gt;Candy.&lt;/li&gt; &lt;li&gt;Gum.&lt;/li&gt; &lt;li&gt;Booze.&lt;/li&gt; &lt;/ul&gt;</code></pre><p>Ordered (numbered) lists use regular numbers, followed by periods, as list markers:</p><pre><code>1. Red 2. Green 3. Blue</code></pre><p>Output:</p><pre><code>&lt;ol&gt; &lt;li&gt;Red&lt;/li&gt; &lt;li&gt;Green&lt;/li&gt; &lt;li&gt;Blue&lt;/li&gt; &lt;/ol&gt;</code></pre><p>If you put blank lines between items, you&#39;ll get<code>&lt;p&gt;</code>tags for the list item text. You can create multi-paragraph list items by indenting the paragraphs by 4 spaces or 1 tab:</p><pre><code>* A list item. With multiple paragraphs. * Another item in the list.</code></pre><p>Output:</p><pre><code>&lt;ul&gt; &lt;li&gt;&lt;p&gt;A list item.&lt;/p&gt; &lt;p&gt;With multiple paragraphs.&lt;/p&gt;&lt;/li&gt; &lt;li&gt;&lt;p&gt;Another item in the list.&lt;/p&gt;&lt;/li&gt; &lt;/ul&gt;</code></pre><h3>Links</h3><p>Markdown supports two styles for creating links:<em>inline</em>and<em>reference</em>. With both styles, you use square brackets to delimit the text you want to turn into a link.</p><p>Inline-style links use parentheses immediately after the link text. For example:</p><pre><code>This is an [example link](http://example.com/).</code></pre><p>Output:</p><pre><code>&lt;p&gt;This is an &lt;a href=&quot;http://example.com/&quot;&gt; example link&lt;/a&gt;.&lt;/p&gt;</code></pre><p>Optionally, you may include a title attribute in the parentheses:</p><pre><code>This is an [example link](http://example.com/ &quot;With a Title&quot;).</code></pre><p>Output:</p><pre><code>&lt;p&gt;This is an &lt;a href=&quot;http://example.com/&quot; title=&quot;With a Title&quot;&gt; example link&lt;/a&gt;.&lt;/p&gt;</code></pre><p>Reference-style links allow you to refer to your links by names, which you define elsewhere in your document:</p><pre><code>I get 10 times more traffic from [Google][1] than from [Yahoo][2] or [MSN][3]. [1]: http://google.com/ &quot;Google&quot; [2]: http://search.yahoo.com/ &quot;Yahoo Search&quot; [3]: http://search.msn.com/ &quot;MSN Search&quot;</code></pre><p>Output:</p><pre><code>&lt;p&gt;I get 10 times more traffic from &lt;a href=&quot;http://google.com/&quot; title=&quot;Google&quot;&gt;Google&lt;/a&gt; than from &lt;a href=&quot;http://search.yahoo.com/&quot; title=&quot;Yahoo Search&quot;&gt;Yahoo&lt;/a&gt; or &lt;a href=&quot;http://search.msn.com/&quot; title=&quot;MSN Search&quot;&gt;MSN&lt;/a&gt;.&lt;/p&gt;</code></pre><p>The title attribute is optional. Link names may contain letters, numbers and spaces, but are<em>not</em>case sensitive:</p><pre><code>I start my morning with a cup of coffee and [The New York Times][NY Times]. [ny times]: http://www.nytimes.com/</code></pre><p>Output:</p><pre><code>&lt;p&gt;I start my morning with a cup of coffee and &lt;a href=&quot;http://www.nytimes.com/&quot;&gt;The New York Times&lt;/a&gt;.&lt;/p&gt;</code></pre><h3>Images</h3><p>Image syntax is very much like link syntax.</p><p>Inline (titles are optional):</p><pre><code>![alt text](/path/to/img.jpg &quot;Title&quot;)</code></pre><p>Reference-style:</p><pre><code>![alt text][id] [id]: /path/to/img.jpg &quot;Title&quot;</code></pre><p>Both of the above examples produce the same output:</p><pre><code>&lt;img src=&quot;/path/to/img.jpg&quot; alt=&quot;alt text&quot; title=&quot;Title&quot; /&gt;</code></pre><h3>Code</h3><p>In a regular paragraph, you can create code span by wrapping text in backtick quotes. Any ampersands (<code>&amp;</code>) and angle brackets (<code>&lt;</code>or<code>&gt;</code>) will automatically be translated into HTML entities. This makes it easy to use Markdown to write about HTML example code:</p><pre><code>I strongly recommend against using any `&lt;blink&gt;` tags. I wish SmartyPants used named entities like `&amp;mdash;` instead of decimal-encoded entites like `&amp;#8212;`.</code></pre><p>Output:</p><pre><code>&lt;p&gt;I strongly recommend against using any &lt;code&gt;&amp;lt;blink&amp;gt;&lt;/code&gt; tags.&lt;/p&gt; &lt;p&gt;I wish SmartyPants used named entities like &lt;code&gt;&amp;amp;mdash;&lt;/code&gt; instead of decimal-encoded entites like &lt;code&gt;&amp;amp;#8212;&lt;/code&gt;.&lt;/p&gt;</code></pre><p>To specify an entire block of pre-formatted code, indent every line of the block by 4 spaces or 1 tab. Just like with code spans,<code>&amp;</code>,<code>&lt;</code>, and<code>&gt;</code>characters will be escaped automatically.</p><p>Markdown:</p><pre><code>If you want your page to validate under XHTML 1.0 Strict, you&#39;ve got to put paragraph tags in your blockquotes: &lt;blockquote&gt; &lt;p&gt;For example.&lt;/p&gt; &lt;/blockquote&gt;</code></pre><p>Output:</p><pre><code>&lt;p&gt;If you want your page to validate under XHTML 1.0 Strict, you&#39;ve got to put paragraph tags in your blockquotes:&lt;/p&gt; &lt;pre&gt;&lt;code&gt;&amp;lt;blockquote&amp;gt; &amp;lt;p&amp;gt;For example.&amp;lt;/p&amp;gt; &amp;lt;/blockquote&amp;gt; &lt;/code&gt;&lt;/pre&gt;</code></pre>
````````````

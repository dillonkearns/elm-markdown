# Original

## blockquotes_with_code_blocks

### Example undefined

This markdown:

```markdown
> Example:
> 
>     sub status {
>         print "working";
>     }
> 
> Or:
> 
>     sub status {
>         return "working";
>     }

```

Should give output:

```html
<blockquote><p>Example:</p><pre><code>sub status { print &quot;working&quot;; }</code></pre><p>Or:</p><pre><code>sub status { return &quot;working&quot;; }</code></pre></blockquote>
```

But instead was:

```html
<p>&gt; Example: &gt; &gt; sub status { &gt; print &quot;working&quot;; &gt; } &gt; &gt; Or: &gt; &gt; sub status { &gt; return &quot;working&quot;; &gt; }</p>
```
## nested_blockquotes

### Example undefined

This markdown:

```markdown
> foo
>
> > bar
>
> foo

```

Should give output:

```html
<blockquote><p>foo</p><blockquote><p>bar</p></blockquote><p>foo</p></blockquote>
```

But instead was:

```html
<p>&gt; foo &gt; &gt; &gt; bar &gt; &gt; foo</p>
```
## tabs

### Example undefined

This markdown:

```markdown
+	this is a list item
	indented with tabs

+   this is a list item
    indented with spaces

Code:

	this code block is indented by one tab

And:

		this code block is indented by two tabs

And:

	+	this is an example list item
		indented with tabs
	
	+   this is an example list item
	    indented with spaces

```

Should give output:

```html
<ul><li><p>this is a list item indented with tabs</p></li><li><p>this is a list item indented with spaces</p></li></ul><p>Code:</p><pre><code>this code block is indented by one tab</code></pre><p>And:</p><pre><code>this code block is indented by two tabs</code></pre><p>And:</p><pre><code>+ this is an example list item indented with tabs + this is an example list item indented with spaces</code></pre>
```

But instead was:

```html
<p>+ this is a list item</p><pre><code>indented with tabs</code></pre><p>+ this is a list item</p><pre><code>indented with spaces</code></pre><p>Code:</p><pre><code>this code block is indented by one tab</code></pre><p>And:</p><pre><code>this code block is indented by two tabs</code></pre><p>And:</p><pre><code>+ this is an example list item</code></pre><pre><code>indented with tabs</code></pre><pre><code></code></pre><pre><code>+ this is an example list item</code></pre><pre><code>indented with spaces</code></pre>
```
## inline_html_advanced

### Example undefined

This markdown:

```markdown
Simple block on one line:

<div>foo</div>

And nested without indentation:

<div>
<div>
<div>
foo
</div>
<div style=">"/>
</div>
<div>bar</div>
</div>

```

Should give output:

```html
<p>Simple block on one line:</p><div>foo</div><p>And nested without indentation:</p><div><div><div>foo</div><div style="&gt;"></div><div>bar</div></div>
```

But instead was:

```html
ERROR Ran into a oneOf with no possibilities!
```
## markdown_documentation_syntax

### Example undefined

This markdown:

```markdown
Markdown: Syntax
================

<ul id="ProjectSubmenu">
    <li><a href="/projects/markdown/" title="Markdown Project Page">Main</a></li>
    <li><a href="/projects/markdown/basics" title="Markdown Basics">Basics</a></li>
    <li><a class="selected" title="Markdown Syntax Documentation">Syntax</a></li>
    <li><a href="/projects/markdown/license" title="Pricing and License Information">License</a></li>
    <li><a href="/projects/markdown/dingus" title="Online Markdown Web Form">Dingus</a></li>
</ul>


*   [Overview](#overview)
    *   [Philosophy](#philosophy)
    *   [Inline HTML](#html)
    *   [Automatic Escaping for Special Characters](#autoescape)
*   [Block Elements](#block)
    *   [Paragraphs and Line Breaks](#p)
    *   [Headers](#header)
    *   [Blockquotes](#blockquote)
    *   [Lists](#list)
    *   [Code Blocks](#precode)
    *   [Horizontal Rules](#hr)
*   [Span Elements](#span)
    *   [Links](#link)
    *   [Emphasis](#em)
    *   [Code](#code)
    *   [Images](#img)
*   [Miscellaneous](#misc)
    *   [Backslash Escapes](#backslash)
    *   [Automatic Links](#autolink)


**Note:** This document is itself written using Markdown; you
can [see the source for it by adding '.text' to the URL][src].

  [src]: /projects/markdown/syntax.text

* * *

<h2 id="overview">Overview</h2>

<h3 id="philosophy">Philosophy</h3>

Markdown is intended to be as easy-to-read and easy-to-write as is feasible.

Readability, however, is emphasized above all else. A Markdown-formatted
document should be publishable as-is, as plain text, without looking
like it's been marked up with tags or formatting instructions. While
Markdown's syntax has been influenced by several existing text-to-HTML
filters -- including [Setext] [1], [atx] [2], [Textile] [3], [reStructuredText] [4],
[Grutatext] [5], and [EtText] [6] -- the single biggest source of
inspiration for Markdown's syntax is the format of plain text email.

  [1]: http://docutils.sourceforge.net/mirror/setext.html
  [2]: http://www.aaronsw.com/2002/atx/
  [3]: http://textism.com/tools/textile/
  [4]: http://docutils.sourceforge.net/rst.html
  [5]: http://www.triptico.com/software/grutatxt.html
  [6]: http://ettext.taint.org/doc/

To this end, Markdown's syntax is comprised entirely of punctuation
characters, which punctuation characters have been carefully chosen so
as to look like what they mean. E.g., asterisks around a word actually
look like \*emphasis\*. Markdown lists look like, well, lists. Even
blockquotes look like quoted passages of text, assuming you've ever
used email.



<h3 id="html">Inline HTML</h3>

Markdown's syntax is intended for one purpose: to be used as a
format for *writing* for the web.

Markdown is not a replacement for HTML, or even close to it. Its
syntax is very small, corresponding only to a very small subset of
HTML tags. The idea is *not* to create a syntax that makes it easier
to insert HTML tags. In my opinion, HTML tags are already easy to
insert. The idea for Markdown is to make it easy to read, write, and
edit prose. HTML is a *publishing* format; Markdown is a *writing*
format. Thus, Markdown's formatting syntax only addresses issues that
can be conveyed in plain text.

For any markup that is not covered by Markdown's syntax, you simply
use HTML itself. There's no need to preface it or delimit it to
indicate that you're switching from Markdown to HTML; you just use
the tags.

The only restrictions are that block-level HTML elements -- e.g. `<div>`,
`<table>`, `<pre>`, `<p>`, etc. -- must be separated from surrounding
content by blank lines, and the start and end tags of the block should
not be indented with tabs or spaces. Markdown is smart enough not
to add extra (unwanted) `<p>` tags around HTML block-level tags.

For example, to add an HTML table to a Markdown article:

    This is a regular paragraph.

    <table>
        <tr>
            <td>Foo</td>
        </tr>
    </table>

    This is another regular paragraph.

Note that Markdown formatting syntax is not processed within block-level
HTML tags. E.g., you can't use Markdown-style `*emphasis*` inside an
HTML block.

Span-level HTML tags -- e.g. `<span>`, `<cite>`, or `<del>` -- can be
used anywhere in a Markdown paragraph, list item, or header. If you
want, you can even use HTML tags instead of Markdown formatting; e.g. if
you'd prefer to use HTML `<a>` or `<img>` tags instead of Markdown's
link or image syntax, go right ahead.

Unlike block-level HTML tags, Markdown syntax *is* processed within
span-level tags.


<h3 id="autoescape">Automatic Escaping for Special Characters</h3>

In HTML, there are two characters that demand special treatment: `<`
and `&`. Left angle brackets are used to start tags; ampersands are
used to denote HTML entities. If you want to use them as literal
characters, you must escape them as entities, e.g. `&lt;`, and
`&amp;`.

Ampersands in particular are bedeviling for web writers. If you want to
write about 'AT&T', you need to write '`AT&amp;T`'. You even need to
escape ampersands within URLs. Thus, if you want to link to:

    http://images.google.com/images?num=30&q=larry+bird

you need to encode the URL as:

    http://images.google.com/images?num=30&amp;q=larry+bird

in your anchor tag `href` attribute. Needless to say, this is easy to
forget, and is probably the single most common source of HTML validation
errors in otherwise well-marked-up web sites.

Markdown allows you to use these characters naturally, taking care of
all the necessary escaping for you. If you use an ampersand as part of
an HTML entity, it remains unchanged; otherwise it will be translated
into `&amp;`.

So, if you want to include a copyright symbol in your article, you can write:

    &copy;

and Markdown will leave it alone. But if you write:

    AT&T

Markdown will translate it to:

    AT&amp;T

Similarly, because Markdown supports [inline HTML](#html), if you use
angle brackets as delimiters for HTML tags, Markdown will treat them as
such. But if you write:

    4 < 5

Markdown will translate it to:

    4 &lt; 5

However, inside Markdown code spans and blocks, angle brackets and
ampersands are *always* encoded automatically. This makes it easy to use
Markdown to write about HTML code. (As opposed to raw HTML, which is a
terrible format for writing about HTML syntax, because every single `<`
and `&` in your example code needs to be escaped.)


* * *


<h2 id="block">Block Elements</h2>


<h3 id="p">Paragraphs and Line Breaks</h3>

A paragraph is simply one or more consecutive lines of text, separated
by one or more blank lines. (A blank line is any line that looks like a
blank line -- a line containing nothing but spaces or tabs is considered
blank.) Normal paragraphs should not be intended with spaces or tabs.

The implication of the "one or more consecutive lines of text" rule is
that Markdown supports "hard-wrapped" text paragraphs. This differs
significantly from most other text-to-HTML formatters (including Movable
Type's "Convert Line Breaks" option) which translate every line break
character in a paragraph into a `<br />` tag.

When you *do* want to insert a `<br />` break tag using Markdown, you
end a line with two or more spaces, then type return.

Yes, this takes a tad more effort to create a `<br />`, but a simplistic
"every line break is a `<br />`" rule wouldn't work for Markdown.
Markdown's email-style [blockquoting][bq] and multi-paragraph [list items][l]
work best -- and look better -- when you format them with hard breaks.

  [bq]: #blockquote
  [l]:  #list



<h3 id="header">Headers</h3>

Markdown supports two styles of headers, [Setext] [1] and [atx] [2].

Setext-style headers are "underlined" using equal signs (for first-level
headers) and dashes (for second-level headers). For example:

    This is an H1
    =============

    This is an H2
    -------------

Any number of underlining `=`'s or `-`'s will work.

Atx-style headers use 1-6 hash characters at the start of the line,
corresponding to header levels 1-6. For example:

    # This is an H1

    ## This is an H2

    ###### This is an H6

Optionally, you may "close" atx-style headers. This is purely
cosmetic -- you can use this if you think it looks better. The
closing hashes don't even need to match the number of hashes
used to open the header. (The number of opening hashes
determines the header level.) :

    # This is an H1 #

    ## This is an H2 ##

    ### This is an H3 ######


<h3 id="blockquote">Blockquotes</h3>

Markdown uses email-style `>` characters for blockquoting. If you're
familiar with quoting passages of text in an email message, then you
know how to create a blockquote in Markdown. It looks best if you hard
wrap the text and put a `>` before every line:

    > This is a blockquote with two paragraphs. Lorem ipsum dolor sit amet,
    > consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus.
    > Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus.
    >
    > Donec sit amet nisl. Aliquam semper ipsum sit amet velit. Suspendisse
    > id sem consectetuer libero luctus adipiscing.

Markdown allows you to be lazy and only put the `>` before the first
line of a hard-wrapped paragraph:

    > This is a blockquote with two paragraphs. Lorem ipsum dolor sit amet,
    consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus.
    Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus.

    > Donec sit amet nisl. Aliquam semper ipsum sit amet velit. Suspendisse
    id sem consectetuer libero luctus adipiscing.

Blockquotes can be nested (i.e. a blockquote-in-a-blockquote) by
adding additional levels of `>`:

    > This is the first level of quoting.
    >
    > > This is nested blockquote.
    >
    > Back to the first level.

Blockquotes can contain other Markdown elements, including headers, lists,
and code blocks:

	> ## This is a header.
	>
	> 1.   This is the first list item.
	> 2.   This is the second list item.
	>
	> Here's some example code:
	>
	>     return shell_exec("echo $input | $markdown_script");

Any decent text editor should make email-style quoting easy. For
example, with BBEdit, you can make a selection and choose Increase
Quote Level from the Text menu.


<h3 id="list">Lists</h3>

Markdown supports ordered (numbered) and unordered (bulleted) lists.

Unordered lists use asterisks, pluses, and hyphens -- interchangably
-- as list markers:

    *   Red
    *   Green
    *   Blue

is equivalent to:

    +   Red
    +   Green
    +   Blue

and:

    -   Red
    -   Green
    -   Blue

Ordered lists use numbers followed by periods:

    1.  Bird
    2.  McHale
    3.  Parish

It's important to note that the actual numbers you use to mark the
list have no effect on the HTML output Markdown produces. The HTML
Markdown produces from the above list is:

    <ol>
    <li>Bird</li>
    <li>McHale</li>
    <li>Parish</li>
    </ol>

If you instead wrote the list in Markdown like this:

    1.  Bird
    1.  McHale
    1.  Parish

or even:

    3. Bird
    1. McHale
    8. Parish

you'd get the exact same HTML output. The point is, if you want to,
you can use ordinal numbers in your ordered Markdown lists, so that
the numbers in your source match the numbers in your published HTML.
But if you want to be lazy, you don't have to.

If you do use lazy list numbering, however, you should still start the
list with the number 1. At some point in the future, Markdown may support
starting ordered lists at an arbitrary number.

List markers typically start at the left margin, but may be indented by
up to three spaces. List markers must be followed by one or more spaces
or a tab.

To make lists look nice, you can wrap items with hanging indents:

    *   Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
        Aliquam hendrerit mi posuere lectus. Vestibulum enim wisi,
        viverra nec, fringilla in, laoreet vitae, risus.
    *   Donec sit amet nisl. Aliquam semper ipsum sit amet velit.
        Suspendisse id sem consectetuer libero luctus adipiscing.

But if you want to be lazy, you don't have to:

    *   Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
    Aliquam hendrerit mi posuere lectus. Vestibulum enim wisi,
    viverra nec, fringilla in, laoreet vitae, risus.
    *   Donec sit amet nisl. Aliquam semper ipsum sit amet velit.
    Suspendisse id sem consectetuer libero luctus adipiscing.

If list items are separated by blank lines, Markdown will wrap the
items in `<p>` tags in the HTML output. For example, this input:

    *   Bird
    *   Magic

will turn into:

    <ul>
    <li>Bird</li>
    <li>Magic</li>
    </ul>

But this:

    *   Bird

    *   Magic

will turn into:

    <ul>
    <li><p>Bird</p></li>
    <li><p>Magic</p></li>
    </ul>

List items may consist of multiple paragraphs. Each subsequent
paragraph in a list item must be intended by either 4 spaces
or one tab:

    1.  This is a list item with two paragraphs. Lorem ipsum dolor
        sit amet, consectetuer adipiscing elit. Aliquam hendrerit
        mi posuere lectus.

        Vestibulum enim wisi, viverra nec, fringilla in, laoreet
        vitae, risus. Donec sit amet nisl. Aliquam semper ipsum
        sit amet velit.

    2.  Suspendisse id sem consectetuer libero luctus adipiscing.

It looks nice if you indent every line of the subsequent
paragraphs, but here again, Markdown will allow you to be
lazy:

    *   This is a list item with two paragraphs.

        This is the second paragraph in the list item. You're
    only required to indent the first line. Lorem ipsum dolor
    sit amet, consectetuer adipiscing elit.

    *   Another item in the same list.

To put a blockquote within a list item, the blockquote's `>`
delimiters need to be indented:

    *   A list item with a blockquote:

        > This is a blockquote
        > inside a list item.

To put a code block within a list item, the code block needs
to be indented *twice* -- 8 spaces or two tabs:

    *   A list item with a code block:

            <code goes here>


It's worth noting that it's possible to trigger an ordered list by
accident, by writing something like this:

    1986. What a great season.

In other words, a *number-period-space* sequence at the beginning of a
line. To avoid this, you can backslash-escape the period:

    1986\. What a great season.



<h3 id="precode">Code Blocks</h3>

Pre-formatted code blocks are used for writing about programming or
markup source code. Rather than forming normal paragraphs, the lines
of a code block are interpreted literally. Markdown wraps a code block
in both `<pre>` and `<code>` tags.

To produce a code block in Markdown, simply indent every line of the
block by at least 4 spaces or 1 tab. For example, given this input:

    This is a normal paragraph:

        This is a code block.

Markdown will generate:

    <p>This is a normal paragraph:</p>

    <pre><code>This is a code block.
    </code></pre>

One level of indentation -- 4 spaces or 1 tab -- is removed from each
line of the code block. For example, this:

    Here is an example of AppleScript:

        tell application "Foo"
            beep
        end tell

will turn into:

    <p>Here is an example of AppleScript:</p>

    <pre><code>tell application "Foo"
        beep
    end tell
    </code></pre>

A code block continues until it reaches a line that is not indented
(or the end of the article).

Within a code block, ampersands (`&`) and angle brackets (`<` and `>`)
are automatically converted into HTML entities. This makes it very
easy to include example HTML source code using Markdown -- just paste
it and indent it, and Markdown will handle the hassle of encoding the
ampersands and angle brackets. For example, this:

        <div class="footer">
            &copy; 2004 Foo Corporation
        </div>

will turn into:

    <pre><code>&lt;div class="footer"&gt;
        &amp;copy; 2004 Foo Corporation
    &lt;/div&gt;
    </code></pre>

Regular Markdown syntax is not processed within code blocks. E.g.,
asterisks are just literal asterisks within a code block. This means
it's also easy to use Markdown to write about Markdown's own syntax.



<h3 id="hr">Horizontal Rules</h3>

You can produce a horizontal rule tag (`<hr />`) by placing three or
more hyphens, asterisks, or underscores on a line by themselves. If you
wish, you may use spaces between the hyphens or asterisks. Each of the
following lines will produce a horizontal rule:

    * * *

    ***

    *****

    - - -

    ---------------------------------------

	_ _ _


* * *

<h2 id="span">Span Elements</h2>

<h3 id="link">Links</h3>

Markdown supports two style of links: *inline* and *reference*.

In both styles, the link text is delimited by [square brackets].

To create an inline link, use a set of regular parentheses immediately
after the link text's closing square bracket. Inside the parentheses,
put the URL where you want the link to point, along with an *optional*
title for the link, surrounded in quotes. For example:

    This is [an example](http://example.com/ "Title") inline link.

    [This link](http://example.net/) has no title attribute.

Will produce:

    <p>This is <a href="http://example.com/" title="Title">
    an example</a> inline link.</p>

    <p><a href="http://example.net/">This link</a> has no
    title attribute.</p>

If you're referring to a local resource on the same server, you can
use relative paths:

    See my [About](/about/) page for details.

Reference-style links use a second set of square brackets, inside
which you place a label of your choosing to identify the link:

    This is [an example][id] reference-style link.

You can optionally use a space to separate the sets of brackets:

    This is [an example] [id] reference-style link.

Then, anywhere in the document, you define your link label like this,
on a line by itself:

    [id]: http://example.com/  "Optional Title Here"

That is:

*   Square brackets containing the link identifier (optionally
    indented from the left margin using up to three spaces);
*   followed by a colon;
*   followed by one or more spaces (or tabs);
*   followed by the URL for the link;
*   optionally followed by a title attribute for the link, enclosed
    in double or single quotes.

The link URL may, optionally, be surrounded by angle brackets:

    [id]: <http://example.com/>  "Optional Title Here"

You can put the title attribute on the next line and use extra spaces
or tabs for padding, which tends to look better with longer URLs:

    [id]: http://example.com/longish/path/to/resource/here
        "Optional Title Here"

Link definitions are only used for creating links during Markdown
processing, and are stripped from your document in the HTML output.

Link definition names may constist of letters, numbers, spaces, and punctuation -- but they are *not* case sensitive. E.g. these two links:

	[link text][a]
	[link text][A]

are equivalent.

The *implicit link name* shortcut allows you to omit the name of the
link, in which case the link text itself is used as the name.
Just use an empty set of square brackets -- e.g., to link the word
"Google" to the google.com web site, you could simply write:

	[Google][]

And then define the link:

	[Google]: http://google.com/

Because link names may contain spaces, this shortcut even works for
multiple words in the link text:

	Visit [Daring Fireball][] for more information.

And then define the link:

	[Daring Fireball]: http://daringfireball.net/

Link definitions can be placed anywhere in your Markdown document. I
tend to put them immediately after each paragraph in which they're
used, but if you want, you can put them all at the end of your
document, sort of like footnotes.

Here's an example of reference links in action:

    I get 10 times more traffic from [Google] [1] than from
    [Yahoo] [2] or [MSN] [3].

      [1]: http://google.com/        "Google"
      [2]: http://search.yahoo.com/  "Yahoo Search"
      [3]: http://search.msn.com/    "MSN Search"

Using the implicit link name shortcut, you could instead write:

    I get 10 times more traffic from [Google][] than from
    [Yahoo][] or [MSN][].

      [google]: http://google.com/        "Google"
      [yahoo]:  http://search.yahoo.com/  "Yahoo Search"
      [msn]:    http://search.msn.com/    "MSN Search"

Both of the above examples will produce the following HTML output:

    <p>I get 10 times more traffic from <a href="http://google.com/"
    title="Google">Google</a> than from
    <a href="http://search.yahoo.com/" title="Yahoo Search">Yahoo</a>
    or <a href="http://search.msn.com/" title="MSN Search">MSN</a>.</p>

For comparison, here is the same paragraph written using
Markdown's inline link style:

    I get 10 times more traffic from [Google](http://google.com/ "Google")
    than from [Yahoo](http://search.yahoo.com/ "Yahoo Search") or
    [MSN](http://search.msn.com/ "MSN Search").

The point of reference-style links is not that they're easier to
write. The point is that with reference-style links, your document
source is vastly more readable. Compare the above examples: using
reference-style links, the paragraph itself is only 81 characters
long; with inline-style links, it's 176 characters; and as raw HTML,
it's 234 characters. In the raw HTML, there's more markup than there
is text.

With Markdown's reference-style links, a source document much more
closely resembles the final output, as rendered in a browser. By
allowing you to move the markup-related metadata out of the paragraph,
you can add links without interrupting the narrative flow of your
prose.


<h3 id="em">Emphasis</h3>

Markdown treats asterisks (`*`) and underscores (`_`) as indicators of
emphasis. Text wrapped with one `*` or `_` will be wrapped with an
HTML `<em>` tag; double `*`'s or `_`'s will be wrapped with an HTML
`<strong>` tag. E.g., this input:

    *single asterisks*

    _single underscores_

    **double asterisks**

    __double underscores__

will produce:

    <em>single asterisks</em>

    <em>single underscores</em>

    <strong>double asterisks</strong>

    <strong>double underscores</strong>

You can use whichever style you prefer; the lone restriction is that
the same character must be used to open and close an emphasis span.

Emphasis can be used in the middle of a word:

    un*fucking*believable

But if you surround an `*` or `_` with spaces, it'll be treated as a
literal asterisk or underscore.

To produce a literal asterisk or underscore at a position where it
would otherwise be used as an emphasis delimiter, you can backslash
escape it:

    \*this text is surrounded by literal asterisks\*



<h3 id="code">Code</h3>

To indicate a span of code, wrap it with backtick quotes (`` ` ``).
Unlike a pre-formatted code block, a code span indicates code within a
normal paragraph. For example:

    Use the `printf()` function.

will produce:

    <p>Use the <code>printf()</code> function.</p>

To include a literal backtick character within a code span, you can use
multiple backticks as the opening and closing delimiters:

    ``There is a literal backtick (`) here.``

which will produce this:

    <p><code>There is a literal backtick (`) here.</code></p>

The backtick delimiters surrounding a code span may include spaces --
one after the opening, one before the closing. This allows you to place
literal backtick characters at the beginning or end of a code span:

	A single backtick in a code span: `` ` ``

	A backtick-delimited string in a code span: `` `foo` ``

will produce:

	<p>A single backtick in a code span: <code>`</code></p>

	<p>A backtick-delimited string in a code span: <code>`foo`</code></p>

With a code span, ampersands and angle brackets are encoded as HTML
entities automatically, which makes it easy to include example HTML
tags. Markdown will turn this:

    Please don't use any `<blink>` tags.

into:

    <p>Please don't use any <code>&lt;blink&gt;</code> tags.</p>

You can write this:

    `&#8212;` is the decimal-encoded equivalent of `&mdash;`.

to produce:

    <p><code>&amp;#8212;</code> is the decimal-encoded
    equivalent of <code>&amp;mdash;</code>.</p>



<h3 id="img">Images</h3>

Admittedly, it's fairly difficult to devise a "natural" syntax for
placing images into a plain text document format.

Markdown uses an image syntax that is intended to resemble the syntax
for links, allowing for two styles: *inline* and *reference*.

Inline image syntax looks like this:

    ![Alt text](/path/to/img.jpg)

    ![Alt text](/path/to/img.jpg "Optional title")

That is:

*   An exclamation mark: `!`;
*   followed by a set of square brackets, containing the `alt`
    attribute text for the image;
*   followed by a set of parentheses, containing the URL or path to
    the image, and an optional `title` attribute enclosed in double
    or single quotes.

Reference-style image syntax looks like this:

    ![Alt text][id]

Where "id" is the name of a defined image reference. Image references
are defined using syntax identical to link references:

    [id]: url/to/image  "Optional title attribute"

As of this writing, Markdown has no syntax for specifying the
dimensions of an image; if this is important to you, you can simply
use regular HTML `<img>` tags.


* * *


<h2 id="misc">Miscellaneous</h2>

<h3 id="autolink">Automatic Links</h3>

Markdown supports a shortcut style for creating "automatic" links for URLs and email addresses: simply surround the URL or email address with angle brackets. What this means is that if you want to show the actual text of a URL or email address, and also have it be a clickable link, you can do this:

    <http://example.com/>

Markdown will turn this into:

    <a href="http://example.com/">http://example.com/</a>

Automatic links for email addresses work similarly, except that
Markdown will also perform a bit of randomized decimal and hex
entity-encoding to help obscure your address from address-harvesting
spambots. For example, Markdown will turn this:

    <address@example.com>

into something like this:

    <a href="&#x6D;&#x61;i&#x6C;&#x74;&#x6F;:&#x61;&#x64;&#x64;&#x72;&#x65;
    &#115;&#115;&#64;&#101;&#120;&#x61;&#109;&#x70;&#x6C;e&#x2E;&#99;&#111;
    &#109;">&#x61;&#x64;&#x64;&#x72;&#x65;&#115;&#115;&#64;&#101;&#120;&#x61;
    &#109;&#x70;&#x6C;e&#x2E;&#99;&#111;&#109;</a>

which will render in a browser as a clickable link to "address@example.com".

(This sort of entity-encoding trick will indeed fool many, if not
most, address-harvesting bots, but it definitely won't fool all of
them. It's better than nothing, but an address published in this way
will probably eventually start receiving spam.)



<h3 id="backslash">Backslash Escapes</h3>

Markdown allows you to use backslash escapes to generate literal
characters which would otherwise have special meaning in Markdown's
formatting syntax. For example, if you wanted to surround a word with
literal asterisks (instead of an HTML `<em>` tag), you can backslashes
before the asterisks, like this:

    \*literal asterisks\*

Markdown provides backslash escapes for the following characters:

    \   backslash
    `   backtick
    *   asterisk
    _   underscore
    {}  curly braces
    []  square brackets
    ()  parentheses
    #   hash mark
	+	plus sign
	-	minus sign (hyphen)
    .   dot
    !   exclamation mark

```

Should give output:

```html
<h1>Markdown: Syntax</h1><ul id="ProjectSubmenu"><li><a href="/projects/markdown/" title="Markdown Project Page">Main</a></li><li><a href="/projects/markdown/basics" title="Markdown Basics">Basics</a></li><li><a class="selected" title="Markdown Syntax Documentation">Syntax</a></li><li><a href="/projects/markdown/license" title="Pricing and License Information">License</a></li><li><a href="/projects/markdown/dingus" title="Online Markdown Web Form">Dingus</a></li></ul><ul><li><a href="#overview">Overview</a><ul><li><a href="#philosophy">Philosophy</a></li><li><a href="#html">Inline HTML</a></li><li><a href="#autoescape">Automatic Escaping for Special Characters</a></li></ul></li><li><a href="#block">Block Elements</a><ul><li><a href="#p">Paragraphs and Line Breaks</a></li><li><a href="#header">Headers</a></li><li><a href="#blockquote">Blockquotes</a></li><li><a href="#list">Lists</a></li><li><a href="#precode">Code Blocks</a></li><li><a href="#hr">Horizontal Rules</a></li></ul></li><li><a href="#span">Span Elements</a><ul><li><a href="#link">Links</a></li><li><a href="#em">Emphasis</a></li><li><a href="#code">Code</a></li><li><a href="#img">Images</a></li></ul></li><li><a href="#misc">Miscellaneous</a><ul><li><a href="#backslash">Backslash Escapes</a></li><li><a href="#autolink">Automatic Links</a></li></ul></li></ul><p><strong>Note:</strong>This document is itself written using Markdown; you can<a href="/projects/markdown/syntax.text">see the source for it by adding &#39;.text&#39; to the URL</a>.</p><hr><h2 id="overview">Overview</h2><h3 id="philosophy">Philosophy</h3><p>Markdown is intended to be as easy-to-read and easy-to-write as is feasible.</p><p>Readability, however, is emphasized above all else. A Markdown-formatted document should be publishable as-is, as plain text, without looking like it&#39;s been marked up with tags or formatting instructions. While Markdown&#39;s syntax has been influenced by several existing text-to-HTML filters -- including<a href="http://docutils.sourceforge.net/mirror/setext.html">Setext</a>,<a href="http://www.aaronsw.com/2002/atx/">atx</a>,<a href="http://textism.com/tools/textile/">Textile</a>,<a href="http://docutils.sourceforge.net/rst.html">reStructuredText</a>,<a href="http://www.triptico.com/software/grutatxt.html">Grutatext</a>, and<a href="http://ettext.taint.org/doc/">EtText</a>-- the single biggest source of inspiration for Markdown&#39;s syntax is the format of plain text email.</p><p>To this end, Markdown&#39;s syntax is comprised entirely of punctuation characters, which punctuation characters have been carefully chosen so as to look like what they mean. E.g., asterisks around a word actually look like *emphasis*. Markdown lists look like, well, lists. Even blockquotes look like quoted passages of text, assuming you&#39;ve ever used email.</p><h3 id="html">Inline HTML</h3><p>Markdown&#39;s syntax is intended for one purpose: to be used as a format for<em>writing</em>for the web.</p><p>Markdown is not a replacement for HTML, or even close to it. Its syntax is very small, corresponding only to a very small subset of HTML tags. The idea is<em>not</em>to create a syntax that makes it easier to insert HTML tags. In my opinion, HTML tags are already easy to insert. The idea for Markdown is to make it easy to read, write, and edit prose. HTML is a<em>publishing</em>format; Markdown is a<em>writing</em>format. Thus, Markdown&#39;s formatting syntax only addresses issues that can be conveyed in plain text.</p><p>For any markup that is not covered by Markdown&#39;s syntax, you simply use HTML itself. There&#39;s no need to preface it or delimit it to indicate that you&#39;re switching from Markdown to HTML; you just use the tags.</p><p>The only restrictions are that block-level HTML elements -- e.g.<code>&lt;div&gt;</code>,<code>&lt;table&gt;</code>,<code>&lt;pre&gt;</code>,<code>&lt;p&gt;</code>, etc. -- must be separated from surrounding content by blank lines, and the start and end tags of the block should not be indented with tabs or spaces. Markdown is smart enough not to add extra (unwanted)<code>&lt;p&gt;</code>tags around HTML block-level tags.</p><p>For example, to add an HTML table to a Markdown article:</p><pre><code>This is a regular paragraph. &lt;table&gt; &lt;tr&gt; &lt;td&gt;Foo&lt;/td&gt; &lt;/tr&gt; &lt;/table&gt; This is another regular paragraph.</code></pre><p>Note that Markdown formatting syntax is not processed within block-level HTML tags. E.g., you can&#39;t use Markdown-style<code>*emphasis*</code>inside an HTML block.</p><p>Span-level HTML tags -- e.g.<code>&lt;span&gt;</code>,<code>&lt;cite&gt;</code>, or<code>&lt;del&gt;</code>-- can be used anywhere in a Markdown paragraph, list item, or header. If you want, you can even use HTML tags instead of Markdown formatting; e.g. if you&#39;d prefer to use HTML<code>&lt;a&gt;</code>or<code>&lt;img&gt;</code>tags instead of Markdown&#39;s link or image syntax, go right ahead.</p><p>Unlike block-level HTML tags, Markdown syntax<em>is</em>processed within span-level tags.</p><h3 id="autoescape">Automatic Escaping for Special Characters</h3><p>In HTML, there are two characters that demand special treatment:<code>&lt;</code>and<code>&amp;</code>. Left angle brackets are used to start tags; ampersands are used to denote HTML entities. If you want to use them as literal characters, you must escape them as entities, e.g.<code>&amp;lt;</code>, and<code>&amp;amp;</code>.</p><p>Ampersands in particular are bedeviling for web writers. If you want to write about &#39;AT&amp;T&#39;, you need to write &#39;<code>AT&amp;amp;T</code>&#39;. You even need to escape ampersands within URLs. Thus, if you want to link to:</p><pre><code>http://images.google.com/images?num=30&amp;q=larry+bird</code></pre><p>you need to encode the URL as:</p><pre><code>http://images.google.com/images?num=30&amp;amp;q=larry+bird</code></pre><p>in your anchor tag<code>href</code>attribute. Needless to say, this is easy to forget, and is probably the single most common source of HTML validation errors in otherwise well-marked-up web sites.</p><p>Markdown allows you to use these characters naturally, taking care of all the necessary escaping for you. If you use an ampersand as part of an HTML entity, it remains unchanged; otherwise it will be translated into<code>&amp;amp;</code>.</p><p>So, if you want to include a copyright symbol in your article, you can write:</p><pre><code>&amp;copy;</code></pre><p>and Markdown will leave it alone. But if you write:</p><pre><code>AT&amp;T</code></pre><p>Markdown will translate it to:</p><pre><code>AT&amp;amp;T</code></pre><p>Similarly, because Markdown supports<a href="#html">inline HTML</a>, if you use angle brackets as delimiters for HTML tags, Markdown will treat them as such. But if you write:</p><pre><code>4 &lt; 5</code></pre><p>Markdown will translate it to:</p><pre><code>4 &amp;lt; 5</code></pre><p>However, inside Markdown code spans and blocks, angle brackets and ampersands are<em>always</em>encoded automatically. This makes it easy to use Markdown to write about HTML code. (As opposed to raw HTML, which is a terrible format for writing about HTML syntax, because every single<code>&lt;</code>and<code>&amp;</code>in your example code needs to be escaped.)</p><hr><h2 id="block">Block Elements</h2><h3 id="p">Paragraphs and Line Breaks</h3><p>A paragraph is simply one or more consecutive lines of text, separated by one or more blank lines. (A blank line is any line that looks like a blank line -- a line containing nothing but spaces or tabs is considered blank.) Normal paragraphs should not be intended with spaces or tabs.</p><p>The implication of the &quot;one or more consecutive lines of text&quot; rule is that Markdown supports &quot;hard-wrapped&quot; text paragraphs. This differs significantly from most other text-to-HTML formatters (including Movable Type&#39;s &quot;Convert Line Breaks&quot; option) which translate every line break character in a paragraph into a<code>&lt;br /&gt;</code>tag.</p><p>When you<em>do</em>want to insert a<code>&lt;br /&gt;</code>break tag using Markdown, you end a line with two or more spaces, then type return.</p><p>Yes, this takes a tad more effort to create a<code>&lt;br /&gt;</code>, but a simplistic &quot;every line break is a<code>&lt;br /&gt;</code>&quot; rule wouldn&#39;t work for Markdown. Markdown&#39;s email-style<a href="#blockquote">blockquoting</a>and multi-paragraph<a href="#list">list items</a>work best -- and look better -- when you format them with hard breaks.</p><h3 id="header">Headers</h3><p>Markdown supports two styles of headers,<a href="http://docutils.sourceforge.net/mirror/setext.html">Setext</a>and<a href="http://www.aaronsw.com/2002/atx/">atx</a>.</p><p>Setext-style headers are &quot;underlined&quot; using equal signs (for first-level headers) and dashes (for second-level headers). For example:</p><pre><code>This is an H1 ============= This is an H2 -------------</code></pre><p>Any number of underlining<code>=</code>&#39;s or<code>-</code>&#39;s will work.</p><p>Atx-style headers use 1-6 hash characters at the start of the line, corresponding to header levels 1-6. For example:</p><pre><code># This is an H1 ## This is an H2 ###### This is an H6</code></pre><p>Optionally, you may &quot;close&quot; atx-style headers. This is purely cosmetic -- you can use this if you think it looks better. The closing hashes don&#39;t even need to match the number of hashes used to open the header. (The number of opening hashes determines the header level.) :</p><pre><code># This is an H1 # ## This is an H2 ## ### This is an H3 ######</code></pre><h3 id="blockquote">Blockquotes</h3><p>Markdown uses email-style<code>&gt;</code>characters for blockquoting. If you&#39;re familiar with quoting passages of text in an email message, then you know how to create a blockquote in Markdown. It looks best if you hard wrap the text and put a<code>&gt;</code>before every line:</p><pre><code>&gt; This is a blockquote with two paragraphs. Lorem ipsum dolor sit amet, &gt; consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus. &gt; Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus. &gt; &gt; Donec sit amet nisl. Aliquam semper ipsum sit amet velit. Suspendisse &gt; id sem consectetuer libero luctus adipiscing.</code></pre><p>Markdown allows you to be lazy and only put the<code>&gt;</code>before the first line of a hard-wrapped paragraph:</p><pre><code>&gt; This is a blockquote with two paragraphs. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus. Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus. &gt; Donec sit amet nisl. Aliquam semper ipsum sit amet velit. Suspendisse id sem consectetuer libero luctus adipiscing.</code></pre><p>Blockquotes can be nested (i.e. a blockquote-in-a-blockquote) by adding additional levels of<code>&gt;</code>:</p><pre><code>&gt; This is the first level of quoting. &gt; &gt; &gt; This is nested blockquote. &gt; &gt; Back to the first level.</code></pre><p>Blockquotes can contain other Markdown elements, including headers, lists, and code blocks:</p><pre><code>&gt; ## This is a header. &gt; &gt; 1. This is the first list item. &gt; 2. This is the second list item. &gt; &gt; Here&#39;s some example code: &gt; &gt; return shell_exec(&quot;echo $input | $markdown_script&quot;);</code></pre><p>Any decent text editor should make email-style quoting easy. For example, with BBEdit, you can make a selection and choose Increase Quote Level from the Text menu.</p><h3 id="list">Lists</h3><p>Markdown supports ordered (numbered) and unordered (bulleted) lists.</p><p>Unordered lists use asterisks, pluses, and hyphens -- interchangably -- as list markers:</p><pre><code>* Red * Green * Blue</code></pre><p>is equivalent to:</p><pre><code>+ Red + Green + Blue</code></pre><p>and:</p><pre><code>- Red - Green - Blue</code></pre><p>Ordered lists use numbers followed by periods:</p><pre><code>1. Bird 2. McHale 3. Parish</code></pre><p>It&#39;s important to note that the actual numbers you use to mark the list have no effect on the HTML output Markdown produces. The HTML Markdown produces from the above list is:</p><pre><code>&lt;ol&gt; &lt;li&gt;Bird&lt;/li&gt; &lt;li&gt;McHale&lt;/li&gt; &lt;li&gt;Parish&lt;/li&gt; &lt;/ol&gt;</code></pre><p>If you instead wrote the list in Markdown like this:</p><pre><code>1. Bird 1. McHale 1. Parish</code></pre><p>or even:</p><pre><code>3. Bird 1. McHale 8. Parish</code></pre><p>you&#39;d get the exact same HTML output. The point is, if you want to, you can use ordinal numbers in your ordered Markdown lists, so that the numbers in your source match the numbers in your published HTML. But if you want to be lazy, you don&#39;t have to.</p><p>If you do use lazy list numbering, however, you should still start the list with the number 1. At some point in the future, Markdown may support starting ordered lists at an arbitrary number.</p><p>List markers typically start at the left margin, but may be indented by up to three spaces. List markers must be followed by one or more spaces or a tab.</p><p>To make lists look nice, you can wrap items with hanging indents:</p><pre><code>* Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus. Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus. * Donec sit amet nisl. Aliquam semper ipsum sit amet velit. Suspendisse id sem consectetuer libero luctus adipiscing.</code></pre><p>But if you want to be lazy, you don&#39;t have to:</p><pre><code>* Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus. Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus. * Donec sit amet nisl. Aliquam semper ipsum sit amet velit. Suspendisse id sem consectetuer libero luctus adipiscing.</code></pre><p>If list items are separated by blank lines, Markdown will wrap the items in<code>&lt;p&gt;</code>tags in the HTML output. For example, this input:</p><pre><code>* Bird * Magic</code></pre><p>will turn into:</p><pre><code>&lt;ul&gt; &lt;li&gt;Bird&lt;/li&gt; &lt;li&gt;Magic&lt;/li&gt; &lt;/ul&gt;</code></pre><p>But this:</p><pre><code>* Bird * Magic</code></pre><p>will turn into:</p><pre><code>&lt;ul&gt; &lt;li&gt;&lt;p&gt;Bird&lt;/p&gt;&lt;/li&gt; &lt;li&gt;&lt;p&gt;Magic&lt;/p&gt;&lt;/li&gt; &lt;/ul&gt;</code></pre><p>List items may consist of multiple paragraphs. Each subsequent paragraph in a list item must be intended by either 4 spaces or one tab:</p><pre><code>1. This is a list item with two paragraphs. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus. Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus. Donec sit amet nisl. Aliquam semper ipsum sit amet velit. 2. Suspendisse id sem consectetuer libero luctus adipiscing.</code></pre><p>It looks nice if you indent every line of the subsequent paragraphs, but here again, Markdown will allow you to be lazy:</p><pre><code>* This is a list item with two paragraphs. This is the second paragraph in the list item. You&#39;re only required to indent the first line. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. * Another item in the same list.</code></pre><p>To put a blockquote within a list item, the blockquote&#39;s<code>&gt;</code>delimiters need to be indented:</p><pre><code>* A list item with a blockquote: &gt; This is a blockquote &gt; inside a list item.</code></pre><p>To put a code block within a list item, the code block needs to be indented<em>twice</em>-- 8 spaces or two tabs:</p><pre><code>* A list item with a code block: &lt;code goes here&gt;</code></pre><p>It&#39;s worth noting that it&#39;s possible to trigger an ordered list by accident, by writing something like this:</p><pre><code>1986. What a great season.</code></pre><p>In other words, a<em>number-period-space</em>sequence at the beginning of a line. To avoid this, you can backslash-escape the period:</p><pre><code>1986\. What a great season.</code></pre><h3 id="precode">Code Blocks</h3><p>Pre-formatted code blocks are used for writing about programming or markup source code. Rather than forming normal paragraphs, the lines of a code block are interpreted literally. Markdown wraps a code block in both<code>&lt;pre&gt;</code>and<code>&lt;code&gt;</code>tags.</p><p>To produce a code block in Markdown, simply indent every line of the block by at least 4 spaces or 1 tab. For example, given this input:</p><pre><code>This is a normal paragraph: This is a code block.</code></pre><p>Markdown will generate:</p><pre><code>&lt;p&gt;This is a normal paragraph:&lt;/p&gt; &lt;pre&gt;&lt;code&gt;This is a code block. &lt;/code&gt;&lt;/pre&gt;</code></pre><p>One level of indentation -- 4 spaces or 1 tab -- is removed from each line of the code block. For example, this:</p><pre><code>Here is an example of AppleScript: tell application &quot;Foo&quot; beep end tell</code></pre><p>will turn into:</p><pre><code>&lt;p&gt;Here is an example of AppleScript:&lt;/p&gt; &lt;pre&gt;&lt;code&gt;tell application &quot;Foo&quot; beep end tell &lt;/code&gt;&lt;/pre&gt;</code></pre><p>A code block continues until it reaches a line that is not indented (or the end of the article).</p><p>Within a code block, ampersands (<code>&amp;</code>) and angle brackets (<code>&lt;</code>and<code>&gt;</code>) are automatically converted into HTML entities. This makes it very easy to include example HTML source code using Markdown -- just paste it and indent it, and Markdown will handle the hassle of encoding the ampersands and angle brackets. For example, this:</p><pre><code>&lt;div class=&quot;footer&quot;&gt; &amp;copy; 2004 Foo Corporation &lt;/div&gt;</code></pre><p>will turn into:</p><pre><code>&lt;pre&gt;&lt;code&gt;&amp;lt;div class=&quot;footer&quot;&amp;gt; &amp;amp;copy; 2004 Foo Corporation &amp;lt;/div&amp;gt; &lt;/code&gt;&lt;/pre&gt;</code></pre><p>Regular Markdown syntax is not processed within code blocks. E.g., asterisks are just literal asterisks within a code block. This means it&#39;s also easy to use Markdown to write about Markdown&#39;s own syntax.</p><h3 id="hr">Horizontal Rules</h3><p>You can produce a horizontal rule tag (<code>&lt;hr /&gt;</code>) by placing three or more hyphens, asterisks, or underscores on a line by themselves. If you wish, you may use spaces between the hyphens or asterisks. Each of the following lines will produce a horizontal rule:</p><pre><code>* * * *** ***** - - - --------------------------------------- _ _ _</code></pre><hr><h2 id="span">Span Elements</h2><h3 id="link">Links</h3><p>Markdown supports two style of links:<em>inline</em>and<em>reference</em>.</p><p>In both styles, the link text is delimited by [square brackets].</p><p>To create an inline link, use a set of regular parentheses immediately after the link text&#39;s closing square bracket. Inside the parentheses, put the URL where you want the link to point, along with an<em>optional</em>title for the link, surrounded in quotes. For example:</p><pre><code>This is [an example](http://example.com/ &quot;Title&quot;) inline link. [This link](http://example.net/) has no title attribute.</code></pre><p>Will produce:</p><pre><code>&lt;p&gt;This is &lt;a href=&quot;http://example.com/&quot; title=&quot;Title&quot;&gt; an example&lt;/a&gt; inline link.&lt;/p&gt; &lt;p&gt;&lt;a href=&quot;http://example.net/&quot;&gt;This link&lt;/a&gt; has no title attribute.&lt;/p&gt;</code></pre><p>If you&#39;re referring to a local resource on the same server, you can use relative paths:</p><pre><code>See my [About](/about/) page for details.</code></pre><p>Reference-style links use a second set of square brackets, inside which you place a label of your choosing to identify the link:</p><pre><code>This is [an example][id] reference-style link.</code></pre><p>You can optionally use a space to separate the sets of brackets:</p><pre><code>This is [an example] [id] reference-style link.</code></pre><p>Then, anywhere in the document, you define your link label like this, on a line by itself:</p><pre><code>[id]: http://example.com/ &quot;Optional Title Here&quot;</code></pre><p>That is:</p><ul><li>Square brackets containing the link identifier (optionally indented from the left margin using up to three spaces);</li><li>followed by a colon;</li><li>followed by one or more spaces (or tabs);</li><li>followed by the URL for the link;</li><li>optionally followed by a title attribute for the link, enclosed in double or single quotes.</li></ul><p>The link URL may, optionally, be surrounded by angle brackets:</p><pre><code>[id]: &lt;http://example.com/&gt; &quot;Optional Title Here&quot;</code></pre><p>You can put the title attribute on the next line and use extra spaces or tabs for padding, which tends to look better with longer URLs:</p><pre><code>[id]: http://example.com/longish/path/to/resource/here &quot;Optional Title Here&quot;</code></pre><p>Link definitions are only used for creating links during Markdown processing, and are stripped from your document in the HTML output.</p><p>Link definition names may constist of letters, numbers, spaces, and punctuation -- but they are<em>not</em>case sensitive. E.g. these two links:</p><pre><code>[link text][a] [link text][A]</code></pre><p>are equivalent.</p><p>The<em>implicit link name</em>shortcut allows you to omit the name of the link, in which case the link text itself is used as the name. Just use an empty set of square brackets -- e.g., to link the word &quot;Google&quot; to the google.com web site, you could simply write:</p><pre><code>[Google][]</code></pre><p>And then define the link:</p><pre><code>[Google]: http://google.com/</code></pre><p>Because link names may contain spaces, this shortcut even works for multiple words in the link text:</p><pre><code>Visit [Daring Fireball][] for more information.</code></pre><p>And then define the link:</p><pre><code>[Daring Fireball]: http://daringfireball.net/</code></pre><p>Link definitions can be placed anywhere in your Markdown document. I tend to put them immediately after each paragraph in which they&#39;re used, but if you want, you can put them all at the end of your document, sort of like footnotes.</p><p>Here&#39;s an example of reference links in action:</p><pre><code>I get 10 times more traffic from [Google] [1] than from [Yahoo] [2] or [MSN] [3]. [1]: http://google.com/ &quot;Google&quot; [2]: http://search.yahoo.com/ &quot;Yahoo Search&quot; [3]: http://search.msn.com/ &quot;MSN Search&quot;</code></pre><p>Using the implicit link name shortcut, you could instead write:</p><pre><code>I get 10 times more traffic from [Google][] than from [Yahoo][] or [MSN][]. [google]: http://google.com/ &quot;Google&quot; [yahoo]: http://search.yahoo.com/ &quot;Yahoo Search&quot; [msn]: http://search.msn.com/ &quot;MSN Search&quot;</code></pre><p>Both of the above examples will produce the following HTML output:</p><pre><code>&lt;p&gt;I get 10 times more traffic from &lt;a href=&quot;http://google.com/&quot; title=&quot;Google&quot;&gt;Google&lt;/a&gt; than from &lt;a href=&quot;http://search.yahoo.com/&quot; title=&quot;Yahoo Search&quot;&gt;Yahoo&lt;/a&gt; or &lt;a href=&quot;http://search.msn.com/&quot; title=&quot;MSN Search&quot;&gt;MSN&lt;/a&gt;.&lt;/p&gt;</code></pre><p>For comparison, here is the same paragraph written using Markdown&#39;s inline link style:</p><pre><code>I get 10 times more traffic from [Google](http://google.com/ &quot;Google&quot;) than from [Yahoo](http://search.yahoo.com/ &quot;Yahoo Search&quot;) or [MSN](http://search.msn.com/ &quot;MSN Search&quot;).</code></pre><p>The point of reference-style links is not that they&#39;re easier to write. The point is that with reference-style links, your document source is vastly more readable. Compare the above examples: using reference-style links, the paragraph itself is only 81 characters long; with inline-style links, it&#39;s 176 characters; and as raw HTML, it&#39;s 234 characters. In the raw HTML, there&#39;s more markup than there is text.</p><p>With Markdown&#39;s reference-style links, a source document much more closely resembles the final output, as rendered in a browser. By allowing you to move the markup-related metadata out of the paragraph, you can add links without interrupting the narrative flow of your prose.</p><h3 id="em">Emphasis</h3><p>Markdown treats asterisks (<code>*</code>) and underscores (<code>_</code>) as indicators of emphasis. Text wrapped with one<code>*</code>or<code>_</code>will be wrapped with an HTML<code>&lt;em&gt;</code>tag; double<code>*</code>&#39;s or<code>_</code>&#39;s will be wrapped with an HTML<code>&lt;strong&gt;</code>tag. E.g., this input:</p><pre><code>*single asterisks* _single underscores_ **double asterisks** __double underscores__</code></pre><p>will produce:</p><pre><code>&lt;em&gt;single asterisks&lt;/em&gt; &lt;em&gt;single underscores&lt;/em&gt; &lt;strong&gt;double asterisks&lt;/strong&gt; &lt;strong&gt;double underscores&lt;/strong&gt;</code></pre><p>You can use whichever style you prefer; the lone restriction is that the same character must be used to open and close an emphasis span.</p><p>Emphasis can be used in the middle of a word:</p><pre><code>un*fucking*believable</code></pre><p>But if you surround an<code>*</code>or<code>_</code>with spaces, it&#39;ll be treated as a literal asterisk or underscore.</p><p>To produce a literal asterisk or underscore at a position where it would otherwise be used as an emphasis delimiter, you can backslash escape it:</p><pre><code>\*this text is surrounded by literal asterisks\*</code></pre><h3 id="code">Code</h3><p>To indicate a span of code, wrap it with backtick quotes (<code>`</code>). Unlike a pre-formatted code block, a code span indicates code within a normal paragraph. For example:</p><pre><code>Use the `printf()` function.</code></pre><p>will produce:</p><pre><code>&lt;p&gt;Use the &lt;code&gt;printf()&lt;/code&gt; function.&lt;/p&gt;</code></pre><p>To include a literal backtick character within a code span, you can use multiple backticks as the opening and closing delimiters:</p><pre><code>``There is a literal backtick (`) here.``</code></pre><p>which will produce this:</p><pre><code>&lt;p&gt;&lt;code&gt;There is a literal backtick (`) here.&lt;/code&gt;&lt;/p&gt;</code></pre><p>The backtick delimiters surrounding a code span may include spaces -- one after the opening, one before the closing. This allows you to place literal backtick characters at the beginning or end of a code span:</p><pre><code>A single backtick in a code span: `` ` `` A backtick-delimited string in a code span: `` `foo` ``</code></pre><p>will produce:</p><pre><code>&lt;p&gt;A single backtick in a code span: &lt;code&gt;`&lt;/code&gt;&lt;/p&gt; &lt;p&gt;A backtick-delimited string in a code span: &lt;code&gt;`foo`&lt;/code&gt;&lt;/p&gt;</code></pre><p>With a code span, ampersands and angle brackets are encoded as HTML entities automatically, which makes it easy to include example HTML tags. Markdown will turn this:</p><pre><code>Please don&#39;t use any `&lt;blink&gt;` tags.</code></pre><p>into:</p><pre><code>&lt;p&gt;Please don&#39;t use any &lt;code&gt;&amp;lt;blink&amp;gt;&lt;/code&gt; tags.&lt;/p&gt;</code></pre><p>You can write this:</p><pre><code>`&amp;#8212;` is the decimal-encoded equivalent of `&amp;mdash;`.</code></pre><p>to produce:</p><pre><code>&lt;p&gt;&lt;code&gt;&amp;amp;#8212;&lt;/code&gt; is the decimal-encoded equivalent of &lt;code&gt;&amp;amp;mdash;&lt;/code&gt;.&lt;/p&gt;</code></pre><h3 id="img">Images</h3><p>Admittedly, it&#39;s fairly difficult to devise a &quot;natural&quot; syntax for placing images into a plain text document format.</p><p>Markdown uses an image syntax that is intended to resemble the syntax for links, allowing for two styles:<em>inline</em>and<em>reference</em>.</p><p>Inline image syntax looks like this:</p><pre><code>![Alt text](/path/to/img.jpg) ![Alt text](/path/to/img.jpg &quot;Optional title&quot;)</code></pre><p>That is:</p><ul><li>An exclamation mark:<code>!</code>;</li><li>followed by a set of square brackets, containing the<code>alt</code>attribute text for the image;</li><li>followed by a set of parentheses, containing the URL or path to the image, and an optional<code>title</code>attribute enclosed in double or single quotes.</li></ul><p>Reference-style image syntax looks like this:</p><pre><code>![Alt text][id]</code></pre><p>Where &quot;id&quot; is the name of a defined image reference. Image references are defined using syntax identical to link references:</p><pre><code>[id]: url/to/image &quot;Optional title attribute&quot;</code></pre><p>As of this writing, Markdown has no syntax for specifying the dimensions of an image; if this is important to you, you can simply use regular HTML<code>&lt;img&gt;</code>tags.</p><hr><h2 id="misc">Miscellaneous</h2><h3 id="autolink">Automatic Links</h3><p>Markdown supports a shortcut style for creating &quot;automatic&quot; links for URLs and email addresses: simply surround the URL or email address with angle brackets. What this means is that if you want to show the actual text of a URL or email address, and also have it be a clickable link, you can do this:</p><pre><code>&lt;http://example.com/&gt;</code></pre><p>Markdown will turn this into:</p><pre><code>&lt;a href=&quot;http://example.com/&quot;&gt;http://example.com/&lt;/a&gt;</code></pre><p>Automatic links for email addresses work similarly, except that Markdown will also perform a bit of randomized decimal and hex entity-encoding to help obscure your address from address-harvesting spambots. For example, Markdown will turn this:</p><pre><code>&lt;address@example.com&gt;</code></pre><p>into something like this:</p><pre><code>&lt;a href=&quot;&amp;#x6D;&amp;#x61;i&amp;#x6C;&amp;#x74;&amp;#x6F;:&amp;#x61;&amp;#x64;&amp;#x64;&amp;#x72;&amp;#x65; &amp;#115;&amp;#115;&amp;#64;&amp;#101;&amp;#120;&amp;#x61;&amp;#109;&amp;#x70;&amp;#x6C;e&amp;#x2E;&amp;#99;&amp;#111; &amp;#109;&quot;&gt;&amp;#x61;&amp;#x64;&amp;#x64;&amp;#x72;&amp;#x65;&amp;#115;&amp;#115;&amp;#64;&amp;#101;&amp;#120;&amp;#x61; &amp;#109;&amp;#x70;&amp;#x6C;e&amp;#x2E;&amp;#99;&amp;#111;&amp;#109;&lt;/a&gt;</code></pre><p>which will render in a browser as a clickable link to &quot;address@example.com&quot;.</p><p>(This sort of entity-encoding trick will indeed fool many, if not most, address-harvesting bots, but it definitely won&#39;t fool all of them. It&#39;s better than nothing, but an address published in this way will probably eventually start receiving spam.)</p><h3 id="backslash">Backslash Escapes</h3><p>Markdown allows you to use backslash escapes to generate literal characters which would otherwise have special meaning in Markdown&#39;s formatting syntax. For example, if you wanted to surround a word with literal asterisks (instead of an HTML<code>&lt;em&gt;</code>tag), you can backslashes before the asterisks, like this:</p><pre><code>\*literal asterisks\*</code></pre><p>Markdown provides backslash escapes for the following characters:</p><pre><code>\ backslash ` backtick * asterisk _ underscore {} curly braces [] square brackets () parentheses # hash mark + plus sign - minus sign (hyphen) . dot ! exclamation mark</code></pre>
```

But instead was:

```html
ERROR Problem at row 60 Expecting --- Problem at row 60 Expecting *** Problem at row 60 Expecting ___
```
## amps_and_angles_encoding

### Example undefined

This markdown:

```markdown
AT&T has an ampersand in their name.

AT&amp;T is another way to write it.

This & that.

4 < 5.

6 > 5.

Here's a [link] [1] with an ampersand in the URL.

Here's a link with an amersand in the link text: [AT&T] [2].

Here's an inline [link](/script?foo=1&bar=2).

Here's an inline [link](</script?foo=1&bar=2>).


[1]: http://example.com/?foo=1&bar=2
[2]: http://att.com/  "AT&T"

```

Should give output:

```html
<p>AT&amp;T has an ampersand in their name.</p><p>AT&amp;T is another way to write it.</p><p>This &amp; that.</p><p>4 &lt; 5.</p><p>6 &gt; 5.</p><p>Here&#39;s a<a href="http://example.com/?foo=1&amp;bar=2">link</a>with an ampersand in the URL.</p><p>Here&#39;s a link with an amersand in the link text:<a href="http://att.com/" title="AT&amp;T">AT&amp;T</a>.</p><p>Here&#39;s an inline<a href="/script?foo=1&amp;bar=2">link</a>.</p><p>Here&#39;s an inline<a href="/script?foo=1&amp;bar=2">link</a>.</p>
```

But instead was:

```html
ERROR Problem at row 33 Expecting Problem at row 1 Expecting symbol (
```
## hard_wrapped_paragraphs_with_list_like_lines

### Example undefined

This markdown:

```markdown
In Markdown 1.0.0 and earlier. Version
8. This line turns into a list item.
Because a hard-wrapped line in the
middle of a paragraph looked like a
list item.

Here's one with a bullet.
* criminey.

```

Should give output:

```html
<p>In Markdown 1.0.0 and earlier. Version 8. This line turns into a list item. Because a hard-wrapped line in the middle of a paragraph looked like a list item.</p><p>Here&#39;s one with a bullet. * criminey.</p>
```

But instead was:

```html
<p>In Markdown 1.0.0 and earlier. Version 8. This line turns into a list item. Because a hard-wrapped line in the middle of a paragraph looked like a list item.</p><p>Here&#39;s one with a bullet.<em>criminey.</em></p>
```
## code_spans

### Example undefined

This markdown:

```markdown
`<test a="` content of attribute `">`

Fix for backticks within HTML tag: <span attr='`ticks`'>like this</span>

Here's how you put `` `backticks` `` in a code span.


```

Should give output:

```html
<p><code>&lt;test a=&quot;</code>content of attribute<code>&quot;&gt;</code></p><p>Fix for backticks within HTML tag:<span attr="`ticks`">like this</span></p><p>Here&#39;s how you put<code>`backticks`</code>in a code span.</p>
```

But instead was:

```html
<p><code>&lt;test a=&quot;</code>content of attribute<code>&quot;&gt;</code></p><p>Fix for backticks within HTML tag: &lt;span attr=&#39;<code>ticks</code>&#39;&gt;like this&lt;/span&gt;</p><p>Here&#39;s how you put<code>backticks</code>in a code span.</p>
```
## ordered_and_unordered_lists

### Example undefined

This markdown:

```markdown
## Unordered

Asterisks tight:

*	asterisk 1
*	asterisk 2
*	asterisk 3


Asterisks loose:

*	asterisk 1

*	asterisk 2

*	asterisk 3

* * *

Pluses tight:

+	Plus 1
+	Plus 2
+	Plus 3


Pluses loose:

+	Plus 1

+	Plus 2

+	Plus 3

* * *


Minuses tight:

-	Minus 1
-	Minus 2
-	Minus 3


Minuses loose:

-	Minus 1

-	Minus 2

-	Minus 3


## Ordered

Tight:

1.	First
2.	Second
3.	Third

and:

1. One
2. Two
3. Three


Loose using tabs:

1.	First

2.	Second

3.	Third

and using spaces:

1. One

2. Two

3. Three

Multiple paragraphs:

1.	Item 1, graf one.

	Item 2. graf two. The quick brown fox jumped over the lazy dog's
	back.

2.	Item 2.

3.	Item 3.



## Nested

*	Tab
	*	Tab
		*	Tab

Here's another:

1. First
2. Second:
	* Fee
	* Fie
	* Foe
3. Third

Same thing but with paragraphs:

1. First

2. Second:
	* Fee
	* Fie
	* Foe

3. Third


This was an error in Markdown 1.0.1:

*	this

	*	sub

	that

Ordered lists start from initial number:

3. Three
1. Four

Ordered lists start from initial zero:

0. Zero
1. One

```

Should give output:

```html
<h2>Unordered</h2><p>Asterisks tight:</p><ul><li>asterisk 1</li><li>asterisk 2</li><li>asterisk 3</li></ul><p>Asterisks loose:</p><ul><li><p>asterisk 1</p></li><li><p>asterisk 2</p></li><li><p>asterisk 3</p></li></ul><hr><p>Pluses tight:</p><ul><li>Plus 1</li><li>Plus 2</li><li>Plus 3</li></ul><p>Pluses loose:</p><ul><li><p>Plus 1</p></li><li><p>Plus 2</p></li><li><p>Plus 3</p></li></ul><hr><p>Minuses tight:</p><ul><li>Minus 1</li><li>Minus 2</li><li>Minus 3</li></ul><p>Minuses loose:</p><ul><li><p>Minus 1</p></li><li><p>Minus 2</p></li><li><p>Minus 3</p></li></ul><h2>Ordered</h2><p>Tight:</p><ol><li>First</li><li>Second</li><li>Third</li></ol><p>and:</p><ol><li>One</li><li>Two</li><li>Three</li></ol><p>Loose using tabs:</p><ol><li><p>First</p></li><li><p>Second</p></li><li><p>Third</p></li></ol><p>and using spaces:</p><ol><li><p>One</p></li><li><p>Two</p></li><li><p>Three</p></li></ol><p>Multiple paragraphs:</p><ol><li><p>Item 1, graf one.</p><p>Item 2. graf two. The quick brown fox jumped over the lazy dog&#39;s back.</p></li><li><p>Item 2.</p></li><li><p>Item 3.</p></li></ol><h2>Nested</h2><ul><li>Tab<ul><li>Tab<ul><li>Tab</li></ul></li></ul></li></ul><p>Here&#39;s another:</p><ol><li>First</li><li>Second:<ul><li>Fee</li><li>Fie</li><li>Foe</li></ul></li><li>Third</li></ol><p>Same thing but with paragraphs:</p><ol><li><p>First</p></li><li><p>Second:</p><ul><li>Fee</li><li>Fie</li><li>Foe</li></ul></li><li><p>Third</p></li></ol><p>This was an error in Markdown 1.0.1:</p><ul><li><p>this</p><ul><li>sub</li></ul><p>that</p></li></ul><p>Ordered lists start from initial number:</p><ol start="3"><li>Three</li><li>Four</li></ol><p>Ordered lists start from initial zero:</p><ol start="0"><li>Zero</li><li>One</li></ol>
```

But instead was:

```html
<h2>Unordered</h2><p>Asterisks tight:</p><p><em>asterisk 1</em>asterisk 2<em>asterisk 3</em></p><p>Asterisks loose:</p><p><em>asterisk 1</em></p><p><em>asterisk 2</em></p><p><em>asterisk 3</em></p><p><em></em></p><p>Pluses tight:</p><p>+ Plus 1 + Plus 2 + Plus 3</p><p>Pluses loose:</p><p>+ Plus 1</p><p>+ Plus 2</p><p>+ Plus 3</p><p><em></em></p><p>Minuses tight:</p><ul><li><p>Minus 1</p></li><li><p>Minus 2</p></li><li><p>Minus 3</p></li></ul><p>Minuses loose:</p><ul><li><p>Minus 1</p></li></ul><ul><li><p>Minus 2</p></li></ul><ul><li><p>Minus 3</p></li></ul><h2>Ordered</h2><p>Tight:</p><p>1. First 2. Second 3. Third</p><p>and:</p><p>1. One 2. Two 3. Three</p><p>Loose using tabs:</p><p>1. First</p><p>2. Second</p><p>3. Third</p><p>and using spaces:</p><p>1. One</p><p>2. Two</p><p>3. Three</p><p>Multiple paragraphs:</p><p>1. Item 1, graf one.</p><pre><code>Item 2. graf two. The quick brown fox jumped over the lazy dog&#39;s</code></pre><pre><code>back.</code></pre><p>2. Item 2.</p><p>3. Item 3.</p><h2>Nested</h2><p><em>Tab</em></p><pre><code>* Tab</code></pre><pre><code>* Tab</code></pre><p>Here&#39;s another:</p><p>1. First 2. Second:</p><pre><code>* Fee</code></pre><pre><code>* Fie</code></pre><pre><code>* Foe</code></pre><p>3. Third</p><p>Same thing but with paragraphs:</p><p>1. First</p><p>2. Second:</p><pre><code>* Fee</code></pre><pre><code>* Fie</code></pre><pre><code>* Foe</code></pre><p>3. Third</p><p>This was an error in Markdown 1.0.1:</p><p><em>this</em></p><pre><code>* sub</code></pre><pre><code>that</code></pre><p>Ordered lists start from initial number:</p><p>3. Three 1. Four</p><p>Ordered lists start from initial zero:</p><p>0. Zero 1. One</p>
```
## inline_html_simple

### Example undefined

This markdown:

```markdown
Here's a simple block:

<div>
	foo
</div>

This should be a code block, though:

	<div>
		foo
	</div>

As should this:

	<div>foo</div>

Now, nested:

<div>
	<div>
		<div>
			foo
		</div>
	</div>
</div>

This should just be an HTML comment:

<!-- Comment -->

Multiline:

<!--
Blah
Blah
-->

Code block:

	<!-- Comment -->

Just plain comment, with trailing spaces on the line:

<!-- foo -->   

Code:

	<hr />
	
Hr's:

<hr>

<hr/>

<hr />

<hr>   

<hr/>  

<hr /> 

<hr class="foo" id="bar" />

<hr class="foo" id="bar"/>

<hr class="foo" id="bar" >


```

Should give output:

```html
<p>Here&#39;s a simple block:</p><div>foo</div><p>This should be a code block, though:</p><pre><code>&lt;div&gt; foo &lt;/div&gt;</code></pre><p>As should this:</p><pre><code>&lt;div&gt;foo&lt;/div&gt;</code></pre><p>Now, nested:</p><div><div><div>foo</div></div></div><p>This should just be an HTML comment:</p><p>Multiline:</p><p>Code block:</p><pre><code>&lt;!-- Comment --&gt;</code></pre><p>Just plain comment, with trailing spaces on the line:</p><p>Code:</p><pre><code>&lt;hr /&gt;</code></pre><p>Hr&#39;s:</p><hr><hr><hr><hr><hr><hr><hr class="foo" id="bar"><hr class="foo" id="bar"><hr class="foo" id="bar">
```

But instead was:

```html
ERROR Problem at row 38 Expecting symbol =
```
## code_blocks

### Example undefined

This markdown:

```markdown
	code block on the first line
	
Regular text.

    code block indented by spaces

Regular text.

	the lines in this block  
	all contain trailing spaces  

Regular Text.

	code block on the last line

```

Should give output:

```html
<pre><code>code block on the first line</code></pre><p>Regular text.</p><pre><code>code block indented by spaces</code></pre><p>Regular text.</p><pre><code>the lines in this block all contain trailing spaces</code></pre><p>Regular Text.</p><pre><code>code block on the last line</code></pre>
```

But instead was:

```html
<pre><code>code block on the first line</code></pre><pre><code></code></pre><p>Regular text.</p><pre><code>code block indented by spaces</code></pre><p>Regular text.</p><pre><code>the lines in this block</code></pre><pre><code>all contain trailing spaces</code></pre><p>Regular Text.</p><pre><code>code block on the last line</code></pre>
```
## inline_html_comments

### Example undefined

This markdown:

```markdown
Paragraph one.

<!-- This is a simple comment -->

<!--
	This is another comment.
-->

Paragraph two.

<!-- one comment block -- -- with two comments -->

The end.

```

Should give output:

```html
<p>Paragraph one.</p><p>Paragraph two.</p><p>The end.</p>
```

But instead was:

```html
ERROR Problem at row 4 Expecting symbol =
```
## markdown_documentation_basics

### Example undefined

This markdown:

```markdown
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

```

Should give output:

```html
<h1 id="markdown-basics">Markdown: Basics</h1><ul id="ProjectSubmenu"><li><a href="/projects/markdown/" title="Markdown Project Page">Main</a></li><li><a class="selected" title="Markdown Basics">Basics</a></li><li><a href="/projects/markdown/syntax" title="Markdown Syntax Documentation">Syntax</a></li><li><a href="/projects/markdown/license" title="Pricing and License Information">License</a></li><li><a href="/projects/markdown/dingus" title="Online Markdown Web Form">Dingus</a></li></ul><h2 id="getting-the-gist-of-markdowns-formatting-syntax">Getting the Gist of Markdown&#39;s Formatting Syntax</h2><p>This page offers a brief overview of what it&#39;s like to use Markdown. The<a href="/projects/markdown/syntax" title="Markdown Syntax">syntax page</a>provides complete, detailed documentation for every feature, but Markdown should be very easy to pick up simply by looking at a few examples of it in action. The examples on this page are written in a before/after style, showing example syntax and the HTML output produced by Markdown.</p><p>It&#39;s also helpful to simply try Markdown out; the<a href="/projects/markdown/dingus" title="Markdown Dingus">Dingus</a>is a web application that allows you type your own Markdown-formatted text and translate it to XHTML.</p><p><strong>Note:</strong>This document is itself written using Markdown; you can<a href="/projects/markdown/basics.text">see the source for it by adding &#39;.text&#39; to the URL</a>.</p><h2 id="paragraphs-headers-blockquotes">Paragraphs, Headers, Blockquotes</h2><p>A paragraph is simply one or more consecutive lines of text, separated by one or more blank lines. (A blank line is any line that looks like a blank line -- a line containing nothing spaces or tabs is considered blank.) Normal paragraphs should not be intended with spaces or tabs.</p><p>Markdown offers two styles of headers:<em>Setext</em>and<em>atx</em>. Setext-style headers for<code>&lt;h1&gt;</code>and<code>&lt;h2&gt;</code>are created by &quot;underlining&quot; with equal signs (<code>=</code>) and hyphens (<code>-</code>), respectively. To create an atx-style header, you put 1-6 hash marks (<code>#</code>) at the beginning of the line -- the number of hashes equals the resulting HTML header level.</p><p>Blockquotes are indicated using email-style &#39;<code>&gt;</code>&#39; angle brackets.</p><p>Markdown:</p><pre><code>A First Level Header ==================== A Second Level Header --------------------- Now is the time for all good men to come to the aid of their country. This is just a regular paragraph. The quick brown fox jumped over the lazy dog&#39;s back. ### Header 3 &gt; This is a blockquote. &gt; &gt; This is the second paragraph in the blockquote. &gt; &gt; ## This is an H2 in a blockquote</code></pre><p>Output:</p><pre><code>&lt;h1&gt;A First Level Header&lt;/h1&gt; &lt;h2&gt;A Second Level Header&lt;/h2&gt; &lt;p&gt;Now is the time for all good men to come to the aid of their country. This is just a regular paragraph.&lt;/p&gt; &lt;p&gt;The quick brown fox jumped over the lazy dog&#39;s back.&lt;/p&gt; &lt;h3&gt;Header 3&lt;/h3&gt; &lt;blockquote&gt; &lt;p&gt;This is a blockquote.&lt;/p&gt; &lt;p&gt;This is the second paragraph in the blockquote.&lt;/p&gt; &lt;h2&gt;This is an H2 in a blockquote&lt;/h2&gt; &lt;/blockquote&gt;</code></pre><h3 id="phrase-emphasis">Phrase Emphasis</h3><p>Markdown uses asterisks and underscores to indicate spans of emphasis.</p><p>Markdown:</p><pre><code>Some of these words *are emphasized*. Some of these words _are emphasized also_. Use two asterisks for **strong emphasis**. Or, if you prefer, __use two underscores instead__.</code></pre><p>Output:</p><pre><code>&lt;p&gt;Some of these words &lt;em&gt;are emphasized&lt;/em&gt;. Some of these words &lt;em&gt;are emphasized also&lt;/em&gt;.&lt;/p&gt; &lt;p&gt;Use two asterisks for &lt;strong&gt;strong emphasis&lt;/strong&gt;. Or, if you prefer, &lt;strong&gt;use two underscores instead&lt;/strong&gt;.&lt;/p&gt;</code></pre><h2 id="lists">Lists</h2><p>Unordered (bulleted) lists use asterisks, pluses, and hyphens (<code>*</code>,<code>+</code>, and<code>-</code>) as list markers. These three markers are interchangable; this:</p><pre><code>* Candy. * Gum. * Booze.</code></pre><p>this:</p><pre><code>+ Candy. + Gum. + Booze.</code></pre><p>and this:</p><pre><code>- Candy. - Gum. - Booze.</code></pre><p>all produce the same output:</p><pre><code>&lt;ul&gt; &lt;li&gt;Candy.&lt;/li&gt; &lt;li&gt;Gum.&lt;/li&gt; &lt;li&gt;Booze.&lt;/li&gt; &lt;/ul&gt;</code></pre><p>Ordered (numbered) lists use regular numbers, followed by periods, as list markers:</p><pre><code>1. Red 2. Green 3. Blue</code></pre><p>Output:</p><pre><code>&lt;ol&gt; &lt;li&gt;Red&lt;/li&gt; &lt;li&gt;Green&lt;/li&gt; &lt;li&gt;Blue&lt;/li&gt; &lt;/ol&gt;</code></pre><p>If you put blank lines between items, you&#39;ll get<code>&lt;p&gt;</code>tags for the list item text. You can create multi-paragraph list items by indenting the paragraphs by 4 spaces or 1 tab:</p><pre><code>* A list item. With multiple paragraphs. * Another item in the list.</code></pre><p>Output:</p><pre><code>&lt;ul&gt; &lt;li&gt;&lt;p&gt;A list item.&lt;/p&gt; &lt;p&gt;With multiple paragraphs.&lt;/p&gt;&lt;/li&gt; &lt;li&gt;&lt;p&gt;Another item in the list.&lt;/p&gt;&lt;/li&gt; &lt;/ul&gt;</code></pre><h3 id="links">Links</h3><p>Markdown supports two styles for creating links:<em>inline</em>and<em>reference</em>. With both styles, you use square brackets to delimit the text you want to turn into a link.</p><p>Inline-style links use parentheses immediately after the link text. For example:</p><pre><code>This is an [example link](http://example.com/).</code></pre><p>Output:</p><pre><code>&lt;p&gt;This is an &lt;a href=&quot;http://example.com/&quot;&gt; example link&lt;/a&gt;.&lt;/p&gt;</code></pre><p>Optionally, you may include a title attribute in the parentheses:</p><pre><code>This is an [example link](http://example.com/ &quot;With a Title&quot;).</code></pre><p>Output:</p><pre><code>&lt;p&gt;This is an &lt;a href=&quot;http://example.com/&quot; title=&quot;With a Title&quot;&gt; example link&lt;/a&gt;.&lt;/p&gt;</code></pre><p>Reference-style links allow you to refer to your links by names, which you define elsewhere in your document:</p><pre><code>I get 10 times more traffic from [Google][1] than from [Yahoo][2] or [MSN][3]. [1]: http://google.com/ &quot;Google&quot; [2]: http://search.yahoo.com/ &quot;Yahoo Search&quot; [3]: http://search.msn.com/ &quot;MSN Search&quot;</code></pre><p>Output:</p><pre><code>&lt;p&gt;I get 10 times more traffic from &lt;a href=&quot;http://google.com/&quot; title=&quot;Google&quot;&gt;Google&lt;/a&gt; than from &lt;a href=&quot;http://search.yahoo.com/&quot; title=&quot;Yahoo Search&quot;&gt;Yahoo&lt;/a&gt; or &lt;a href=&quot;http://search.msn.com/&quot; title=&quot;MSN Search&quot;&gt;MSN&lt;/a&gt;.&lt;/p&gt;</code></pre><p>The title attribute is optional. Link names may contain letters, numbers and spaces, but are<em>not</em>case sensitive:</p><pre><code>I start my morning with a cup of coffee and [The New York Times][NY Times]. [ny times]: http://www.nytimes.com/</code></pre><p>Output:</p><pre><code>&lt;p&gt;I start my morning with a cup of coffee and &lt;a href=&quot;http://www.nytimes.com/&quot;&gt;The New York Times&lt;/a&gt;.&lt;/p&gt;</code></pre><h3 id="images">Images</h3><p>Image syntax is very much like link syntax.</p><p>Inline (titles are optional):</p><pre><code>![alt text](/path/to/img.jpg &quot;Title&quot;)</code></pre><p>Reference-style:</p><pre><code>![alt text][id] [id]: /path/to/img.jpg &quot;Title&quot;</code></pre><p>Both of the above examples produce the same output:</p><pre><code>&lt;img src=&quot;/path/to/img.jpg&quot; alt=&quot;alt text&quot; title=&quot;Title&quot; /&gt;</code></pre><h3 id="code">Code</h3><p>In a regular paragraph, you can create code span by wrapping text in backtick quotes. Any ampersands (<code>&amp;</code>) and angle brackets (<code>&lt;</code>or<code>&gt;</code>) will automatically be translated into HTML entities. This makes it easy to use Markdown to write about HTML example code:</p><pre><code>I strongly recommend against using any `&lt;blink&gt;` tags. I wish SmartyPants used named entities like `&amp;mdash;` instead of decimal-encoded entites like `&amp;#8212;`.</code></pre><p>Output:</p><pre><code>&lt;p&gt;I strongly recommend against using any &lt;code&gt;&amp;lt;blink&amp;gt;&lt;/code&gt; tags.&lt;/p&gt; &lt;p&gt;I wish SmartyPants used named entities like &lt;code&gt;&amp;amp;mdash;&lt;/code&gt; instead of decimal-encoded entites like &lt;code&gt;&amp;amp;#8212;&lt;/code&gt;.&lt;/p&gt;</code></pre><p>To specify an entire block of pre-formatted code, indent every line of the block by 4 spaces or 1 tab. Just like with code spans,<code>&amp;</code>,<code>&lt;</code>, and<code>&gt;</code>characters will be escaped automatically.</p><p>Markdown:</p><pre><code>If you want your page to validate under XHTML 1.0 Strict, you&#39;ve got to put paragraph tags in your blockquotes: &lt;blockquote&gt; &lt;p&gt;For example.&lt;/p&gt; &lt;/blockquote&gt;</code></pre><p>Output:</p><pre><code>&lt;p&gt;If you want your page to validate under XHTML 1.0 Strict, you&#39;ve got to put paragraph tags in your blockquotes:&lt;/p&gt; &lt;pre&gt;&lt;code&gt;&amp;lt;blockquote&amp;gt; &amp;lt;p&amp;gt;For example.&amp;lt;/p&amp;gt; &amp;lt;/blockquote&amp;gt; &lt;/code&gt;&lt;/pre&gt;</code></pre>
```

But instead was:

```html
ERROR Problem at row 44 Expecting --- Problem at row 44 Expecting *** Problem at row 44 Expecting ___
```
## links_shortcut_references

### Example undefined

This markdown:

```markdown
This is the [simple case].

[simple case]: /simple



This one has a [line
break].

This one has a [line 
break] with a line-ending space.

[line break]: /foo


[this] [that] and the [other]

[this]: /this
[that]: /that
[other]: /other

```

Should give output:

```html
<p>This is the<a href="/simple">simple case</a>.</p><p>This one has a<a href="/foo">line break</a>.</p><p>This one has a<a href="/foo">line break</a>with a line-ending space.</p><p><a href="/that">this</a>and the<a href="/other">other</a></p>
```

But instead was:

```html
ERROR Problem at row 32 Expecting Problem at row 1 Expecting symbol (
```
## links_reference_style

### Example undefined

This markdown:

```markdown
Foo [bar] [1].

Foo [bar][1].

Foo [bar]
[1].

[1]: /url/  "Title"


With [embedded [brackets]] [b].


Indented [once][].

Indented [twice][].

Indented [thrice][].

Indented [four][] times.

 [once]: /url

  [twice]: /url

   [thrice]: /url

    [four]: /url


[b]: /url/

* * *

[this] [this] should work

So should [this][this].

And [this] [].

And [this][].

And [this].

But not [that] [].

Nor [that][].

Nor [that].

[Something in brackets like [this][] should work]

[Same with [this].]

In this case, [this](/somethingelse/) points to something else.

Backslashing should suppress \[this] and [this\].

[this]: foo


* * *

Here's one where the [link
breaks] across lines.

Here's another where the [link 
breaks] across lines, but with a line-ending space.


[link breaks]: /url/

```

Should give output:

```html
<p>Foo<a href="/url/" title="Title">bar</a>.</p><p>Foo<a href="/url/" title="Title">bar</a>.</p><p>Foo<a href="/url/" title="Title">bar</a>.</p><p>With<a href="/url/">embedded [brackets]</a>.</p><p>Indented<a href="/url">once</a>.</p><p>Indented<a href="/url">twice</a>.</p><p>Indented<a href="/url">thrice</a>.</p><p>Indented [four][] times.</p><pre><code>[four]: /url</code></pre><hr><p><a href="foo">this</a>should work</p><p>So should<a href="foo">this</a>.</p><p>And<a href="foo">this</a>.</p><p>And<a href="foo">this</a>.</p><p>And<a href="foo">this</a>.</p><p>But not [that] [].</p><p>Nor [that][].</p><p>Nor [that].</p><p>[Something in brackets like<a href="foo">this</a>should work]</p><p>[Same with<a href="foo">this</a>.]</p><p>In this case,<a href="/somethingelse/">this</a>points to something else.</p><p>Backslashing should suppress [this] and [this].</p><hr><p>Here&#39;s one where the<a href="/url/">link breaks</a>across lines.</p><p>Here&#39;s another where the<a href="/url/">link breaks</a>across lines, but with a line-ending space.</p>
```

But instead was:

```html
ERROR Problem at row 32 Expecting --- Problem at row 32 Expecting *** Problem at row 32 Expecting ___
```
## backslash_escapes

### Example undefined

This markdown:

```markdown
These should all get escaped:

Backslash: \\

Backtick: \`

Asterisk: \*

Underscore: \_

Left brace: \{

Right brace: \}

Left bracket: \[

Right bracket: \]

Left paren: \(

Right paren: \)

Greater-than: \>

Hash: \#

Period: \.

Bang: \!

Plus: \+

Minus: \-



These should not, because they occur within a code block:

	Backslash: \\

	Backtick: \`

	Asterisk: \*

	Underscore: \_

	Left brace: \{

	Right brace: \}

	Left bracket: \[

	Right bracket: \]

	Left paren: \(

	Right paren: \)

	Greater-than: \>

	Hash: \#

	Period: \.

	Bang: \!

	Plus: \+

	Minus: \-


Nor should these, which occur in code spans:

Backslash: `\\`

Backtick: `` \` ``

Asterisk: `\*`

Underscore: `\_`

Left brace: `\{`

Right brace: `\}`

Left bracket: `\[`

Right bracket: `\]`

Left paren: `\(`

Right paren: `\)`

Greater-than: `\>`

Hash: `\#`

Period: `\.`

Bang: `\!`

Plus: `\+`

Minus: `\-`


These should get escaped, even though they're matching pairs for
other Markdown constructs:

\*asterisks\*

\_underscores\_

\`backticks\`

This is a code span with a literal backslash-backtick sequence: `` \` ``

This is a tag with unescaped backticks <span attr='`ticks`'>bar</span>.

This is a tag with backslashes <span attr='\\backslashes\\'>bar</span>.

```

Should give output:

```html
<p>These should all get escaped:</p><p>Backslash: \</p><p>Backtick: `</p><p>Asterisk: *</p><p>Underscore: _</p><p>Left brace: {</p><p>Right brace: }</p><p>Left bracket: [</p><p>Right bracket: ]</p><p>Left paren: (</p><p>Right paren: )</p><p>Greater-than: &gt;</p><p>Hash: #</p><p>Period: .</p><p>Bang: !</p><p>Plus: +</p><p>Minus: -</p><p>These should not, because they occur within a code block:</p><pre><code>Backslash: \\ Backtick: \` Asterisk: \* Underscore: \_ Left brace: \{ Right brace: \} Left bracket: \[ Right bracket: \] Left paren: \( Right paren: \) Greater-than: \&gt; Hash: \# Period: \. Bang: \! Plus: \+ Minus: \-</code></pre><p>Nor should these, which occur in code spans:</p><p>Backslash:<code>\\</code></p><p>Backtick:<code>\`</code></p><p>Asterisk:<code>\*</code></p><p>Underscore:<code>\_</code></p><p>Left brace:<code>\{</code></p><p>Right brace:<code>\}</code></p><p>Left bracket:<code>\[</code></p><p>Right bracket:<code>\]</code></p><p>Left paren:<code>\(</code></p><p>Right paren:<code>\)</code></p><p>Greater-than:<code>\&gt;</code></p><p>Hash:<code>\#</code></p><p>Period:<code>\.</code></p><p>Bang:<code>\!</code></p><p>Plus:<code>\+</code></p><p>Minus:<code>\-</code></p><p>These should get escaped, even though they&#39;re matching pairs for other Markdown constructs:</p><p>*asterisks*</p><p>_underscores_</p><p>`backticks`</p><p>This is a code span with a literal backslash-backtick sequence:<code>\`</code></p><p>This is a tag with unescaped backticks<span attr="`ticks`">bar</span>.</p><p>This is a tag with backslashes<span attr="\\backslashes\\">bar</span>.</p>
```

But instead was:

```html
ERROR Problem at row 180 Expecting Problem at row 1 Expecting symbol ]
```
## literal_quotes_in_titles

### Example undefined

This markdown:

```markdown
Foo [bar][].

Foo [bar](/url/ "Title with "quotes" inside").


  [bar]: /url/ "Title with "quotes" inside"


```

Should give output:

```html
<p>Foo<a href="/url/" title="Title with &quot;quotes&quot; inside">bar</a>.</p><p>Foo<a href="/url/" title="Title with &quot;quotes&quot; inside">bar</a>.</p>
```

But instead was:

```html
ERROR Problem at row 8 Expecting --- Problem at row 8 Expecting *** Problem at row 8 Expecting ___
```
## auto_links

### Example undefined

This markdown:

```markdown
Link: <http://example.com/>.

With an ampersand: <http://example.com/?foo=1&bar=2>

* In a list?
* <http://example.com/>
* It should.

> Blockquoted: <http://example.com/>

Auto-links should not occur here: `<http://example.com/>`

	or here: <http://example.com/>

```

Should give output:

```html
<p>Link:<a href="http://example.com/">http://example.com/</a>.</p><p>With an ampersand:<a href="http://example.com/?foo=1&amp;bar=2">http://example.com/?foo=1&amp;bar=2</a></p><ul><li>In a list?</li><li><a href="http://example.com/">http://example.com/</a></li><li>It should.</li></ul><blockquote><p>Blockquoted:<a href="http://example.com/">http://example.com/</a></p></blockquote><p>Auto-links should not occur here:<code>&lt;http://example.com/&gt;</code></p><pre><code>or here: &lt;http://example.com/&gt;</code></pre>
```

But instead was:

```html
<p>Link: &lt;http://example.com/&gt;.</p><p>With an ampersand: &lt;http://example.com/?foo=1&amp;bar=2&gt;</p><p><em>In a list?</em>&lt;http://example.com/&gt;<em>It should.</em></p><p>&gt; Blockquoted: &lt;http://example.com/&gt;</p><p>Auto-links should not occur here:<code>&lt;http://example.com/&gt;</code></p><pre><code>or here: &lt;http://example.com/&gt;</code></pre>
```
## horizontal_rules

### Example undefined

This markdown:

```markdown
Dashes:

---

 ---
 
  ---

   ---

	---

- - -

 - - -
 
  - - -

   - - -

	- - -


Asterisks:

***

 ***
 
  ***

   ***

	***

* * *

 * * *
 
  * * *

   * * *

	* * *


Underscores:

___

 ___
 
  ___

   ___

    ___

_ _ _

 _ _ _
 
  _ _ _

   _ _ _

    _ _ _



Not horizontal rules:

--*

-*-

*--

 -_-

  __-

   -__

    _-_


Long rules:

-----------

___________

***********

```

Should give output:

```html
<p>Dashes:</p><hr><hr><hr><hr><pre><code>---</code></pre><hr><hr><hr><hr><pre><code>- - -</code></pre><p>Asterisks:</p><hr><hr><hr><hr><pre><code>***</code></pre><hr><hr><hr><hr><pre><code>* * *</code></pre><p>Underscores:</p><hr><hr><hr><hr><pre><code>___</code></pre><hr><hr><hr><hr><pre><code>_ _ _</code></pre><p>Not horizontal rules:</p><p>--*</p><p>-*-</p><p>*--</p><p>-_-</p><p>__-</p><p>-__</p><pre><code>_-_</code></pre><p>Long rules:</p><hr><hr><hr>
```

But instead was:

```html
ERROR Problem at row 7 Expecting --- Problem at row 7 Expecting *** Problem at row 7 Expecting ___
```
## strong_and_em_together

### Example undefined

This markdown:

```markdown
***This is strong and em.***

So is ***this*** word.

___This is strong and em.___

So is ___this___ word.

```

Should give output:

```html
<p><strong><em>This is strong and em.</em></strong></p><p>So is<strong><em>this</em></strong>word.</p><p><strong><em>This is strong and em.</em></strong></p><p>So is<strong><em>this</em></strong>word.</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## tidyness

### Example undefined

This markdown:

```markdown
> A list within a blockquote:
> 
> *	asterisk 1
> *	asterisk 2
> *	asterisk 3

```

Should give output:

```html
<blockquote><p>A list within a blockquote:</p><ul><li>asterisk 1</li><li>asterisk 2</li><li>asterisk 3</li></ul></blockquote>
```

But instead was:

```html
<p>&gt; A list within a blockquote: &gt; &gt;<em>asterisk 1 &gt;</em>asterisk 2 &gt;<em>asterisk 3</em></p>
```
## links_inline_style

### Example undefined

This markdown:

```markdown
Just a [URL](/url/).

[URL and title](/url/ "title").

[URL and title](/url/  "title preceded by two spaces").

[URL and title](/url/	"title preceded by a tab").

[URL and title](/url/ "title has spaces afterward"  ).

[URL and title]( /url/has space ).

[URL and title]( /url/has space/ "url has space and title").

[Empty]().

```

Should give output:

```html
<p>Just a<a href="/url/">URL</a>.</p><p><a href="/url/" title="title">URL and title</a>.</p><p><a href="/url/" title="title preceded by two spaces">URL and title</a>.</p><p><a href="/url/" title="title preceded by a tab">URL and title</a>.</p><p><a href="/url/" title="title has spaces afterward">URL and title</a>.</p><p><a href="/url/has%20space">URL and title</a>.</p><p><a href="/url/has%20space/" title="url has space and title">URL and title</a>.</p><p><a href="">Empty</a>.</p>
```

But instead was:

```html
<p>Just a<a href="/url/">URL</a>.</p><p><a href="/url/ \" title\""="">URL and title</a>.</p><p><a by="" href="/url/  \" preceded="" spaces\""="" title="" two="">URL and title</a>.</p><p><a a="" by="" href="/url/	\" preceded="" tab\""="" title="">URL and title</a>.</p><p><a "="" afterward\"="" has="" href="/url/ \" spaces="" title="">URL and title</a>.</p><p><a href=" /url/has space ">URL and title</a>.</p><p><a and="" has="" href=" /url/has space/ \" space="" title\""="" url="">URL and title</a>.</p><p><a href="">Empty</a>.</p>
```

## CommonMark

### Block quotes

Example 198

This markdown:

```markdown
> # Foo
> bar
> baz

```

Gives this incorrect output:

```html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

```

Example 199

This markdown:

```markdown
># Foo
>bar
> baz

```

Gives this incorrect output:

```html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

```

Example 200

This markdown:

```markdown
   > # Foo
   > bar
 > baz

```

Gives this incorrect output:

```html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

```

Example 201

This markdown:

```markdown
    > # Foo
    > bar
    > baz

```

Gives this incorrect output:

```html
<pre><code>&gt; # Foo
&gt; bar
&gt; baz
</code></pre>

```

Example 202

This markdown:

```markdown
> # Foo
> bar
baz

```

Gives this incorrect output:

```html
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>

```

Example 203

This markdown:

```markdown
> bar
baz
> foo

```

Gives this incorrect output:

```html
<blockquote>
<p>bar
baz
foo</p>
</blockquote>

```

Example 204

This markdown:

```markdown
> foo
---

```

Gives this incorrect output:

```html
<blockquote>
<p>foo</p>
</blockquote>
<hr />

```

Example 205

This markdown:

```markdown
> - foo
- bar

```

Gives this incorrect output:

```html
<blockquote>
<ul>
<li>foo</li>
</ul>
</blockquote>
<ul>
<li>bar</li>
</ul>

```

Example 206

This markdown:

```markdown
>     foo
    bar

```

Gives this incorrect output:

```html
<blockquote>
<pre><code>foo
</code></pre>
</blockquote>
<pre><code>bar
</code></pre>

```

Example 207

This markdown:

```markdown
> ```
foo
```

```

Gives this incorrect output:

```html
<blockquote>
<pre><code></code></pre>
</blockquote>
<p>foo</p>
<pre><code></code></pre>

```

Example 208

This markdown:

```markdown
> foo
    - bar

```

Gives this incorrect output:

```html
<blockquote>
<p>foo
- bar</p>
</blockquote>

```

Example 209

This markdown:

```markdown
>

```

Gives this incorrect output:

```html
<blockquote>
</blockquote>

```

Example 210

This markdown:

```markdown
>
>  
> 

```

Gives this incorrect output:

```html
<blockquote>
</blockquote>

```

Example 211

This markdown:

```markdown
>
> foo
>  

```

Gives this incorrect output:

```html
<blockquote>
<p>foo</p>
</blockquote>

```

Example 212

This markdown:

```markdown
> foo

> bar

```

Gives this incorrect output:

```html
<blockquote>
<p>foo</p>
</blockquote>
<blockquote>
<p>bar</p>
</blockquote>

```

Example 213

This markdown:

```markdown
> foo
> bar

```

Gives this incorrect output:

```html
<blockquote>
<p>foo
bar</p>
</blockquote>

```

Example 214

This markdown:

```markdown
> foo
>
> bar

```

Gives this incorrect output:

```html
<blockquote>
<p>foo</p>
<p>bar</p>
</blockquote>

```

Example 215

This markdown:

```markdown
foo
> bar

```

Gives this incorrect output:

```html
<p>foo</p>
<blockquote>
<p>bar</p>
</blockquote>

```

Example 216

This markdown:

```markdown
> aaa
***
> bbb

```

Gives this incorrect output:

```html
<blockquote>
<p>aaa</p>
</blockquote>
<hr />
<blockquote>
<p>bbb</p>
</blockquote>

```

Example 217

This markdown:

```markdown
> bar
baz

```

Gives this incorrect output:

```html
<blockquote>
<p>bar
baz</p>
</blockquote>

```

Example 218

This markdown:

```markdown
> bar

baz

```

Gives this incorrect output:

```html
<blockquote>
<p>bar</p>
</blockquote>
<p>baz</p>

```

Example 219

This markdown:

```markdown
> bar
>
baz

```

Gives this incorrect output:

```html
<blockquote>
<p>bar</p>
</blockquote>
<p>baz</p>

```

Example 220

This markdown:

```markdown
> > > foo
bar

```

Gives this incorrect output:

```html
<blockquote>
<blockquote>
<blockquote>
<p>foo
bar</p>
</blockquote>
</blockquote>
</blockquote>

```

Example 221

This markdown:

```markdown
>>> foo
> bar
>>baz

```

Gives this incorrect output:

```html
<blockquote>
<blockquote>
<blockquote>
<p>foo
bar
baz</p>
</blockquote>
</blockquote>
</blockquote>

```

Example 222

This markdown:

```markdown
>     code

>    not code

```

Gives this incorrect output:

```html
<blockquote>
<pre><code>code
</code></pre>
</blockquote>
<blockquote>
<p>not code</p>
</blockquote>

```

### Thematic breaks

Example 16

This markdown:

```markdown
--
**
__

```

Gives this incorrect output:

```html
<p>--
**
__</p>

```

Example 19

This markdown:

```markdown
Foo
    ***

```

Gives this incorrect output:

```html
<p>Foo
***</p>

```

Example 21

This markdown:

```markdown
 - - -

```

Gives this incorrect output:

```html
<hr />

```

Example 22

This markdown:

```markdown
 **  * ** * ** * **

```

Gives this incorrect output:

```html
<hr />

```

Example 23

This markdown:

```markdown
-     -      -      -

```

Gives this incorrect output:

```html
<hr />

```

Example 24

This markdown:

```markdown
- - - -    

```

Gives this incorrect output:

```html
<hr />

```

Example 25

This markdown:

```markdown
_ _ _ _ a

a------

---a---

```

Gives this incorrect output:

```html
<p>_ _ _ _ a</p>
<p>a------</p>
<p>---a---</p>

```

Example 26

This markdown:

```markdown
 *-*

```

Gives this incorrect output:

```html
<p><em>-</em></p>

```

Example 27

This markdown:

```markdown
- foo
***
- bar

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
</ul>
<hr />
<ul>
<li>bar</li>
</ul>

```

Example 29

This markdown:

```markdown
Foo
---
bar

```

Gives this incorrect output:

```html
<h2>Foo</h2>
<p>bar</p>

```

Example 30

This markdown:

```markdown
* Foo
* * *
* Bar

```

Gives this incorrect output:

```html
<ul>
<li>Foo</li>
</ul>
<hr />
<ul>
<li>Bar</li>
</ul>

```

Example 31

This markdown:

```markdown
- Foo
- * * *

```

Gives this incorrect output:

```html
<ul>
<li>Foo</li>
<li>
<hr />
</li>
</ul>

```

### List items

Example 223

This markdown:

```markdown
A paragraph
with two lines.

    indented code

> A block quote.

```

Gives this incorrect output:

```html
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>

```

Example 224

This markdown:

```markdown
1.  A paragraph
    with two lines.

        indented code

    > A block quote.

```

Gives this incorrect output:

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 225

This markdown:

```markdown
- one

 two

```

Gives this incorrect output:

```html
<ul>
<li>one</li>
</ul>
<p>two</p>

```

Example 226

This markdown:

```markdown
- one

  two

```

Gives this incorrect output:

```html
<ul>
<li>
<p>one</p>
<p>two</p>
</li>
</ul>

```

Example 227

This markdown:

```markdown
 -    one

     two

```

Gives this incorrect output:

```html
<ul>
<li>one</li>
</ul>
<pre><code> two
</code></pre>

```

Example 228

This markdown:

```markdown
 -    one

      two

```

Gives this incorrect output:

```html
<ul>
<li>
<p>one</p>
<p>two</p>
</li>
</ul>

```

Example 229

This markdown:

```markdown
   > > 1.  one
>>
>>     two

```

Gives this incorrect output:

```html
<blockquote>
<blockquote>
<ol>
<li>
<p>one</p>
<p>two</p>
</li>
</ol>
</blockquote>
</blockquote>

```

Example 230

This markdown:

```markdown
>>- one
>>
  >  > two

```

Gives this incorrect output:

```html
<blockquote>
<blockquote>
<ul>
<li>one</li>
</ul>
<p>two</p>
</blockquote>
</blockquote>

```

Example 231

This markdown:

```markdown
-one

2.two

```

Gives this incorrect output:

```html
<p>-one</p>
<p>2.two</p>

```

Example 232

This markdown:

```markdown
- foo


  bar

```

Gives this incorrect output:

```html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

```

Example 233

This markdown:

```markdown
1.  foo

    ```
    bar
    ```

    baz

    > bam

```

Gives this incorrect output:

```html
<ol>
<li>
<p>foo</p>
<pre><code>bar
</code></pre>
<p>baz</p>
<blockquote>
<p>bam</p>
</blockquote>
</li>
</ol>

```

Example 234

This markdown:

```markdown
- Foo

      bar


      baz

```

Gives this incorrect output:

```html
<ul>
<li>
<p>Foo</p>
<pre><code>bar


baz
</code></pre>
</li>
</ul>

```

Example 235

This markdown:

```markdown
123456789. ok

```

Gives this incorrect output:

```html
<ol start="123456789">
<li>ok</li>
</ol>

```

Example 237

This markdown:

```markdown
0. ok

```

Gives this incorrect output:

```html
<ol start="0">
<li>ok</li>
</ol>

```

Example 238

This markdown:

```markdown
003. ok

```

Gives this incorrect output:

```html
<ol start="3">
<li>ok</li>
</ol>

```

Example 239

This markdown:

```markdown
-1. not ok

```

Gives this incorrect output:

```html
<p>-1. not ok</p>

```

Example 240

This markdown:

```markdown
- foo

      bar

```

Gives this incorrect output:

```html
<ul>
<li>
<p>foo</p>
<pre><code>bar
</code></pre>
</li>
</ul>

```

Example 241

This markdown:

```markdown
  10.  foo

           bar

```

Gives this incorrect output:

```html
<ol start="10">
<li>
<p>foo</p>
<pre><code>bar
</code></pre>
</li>
</ol>

```

Example 243

This markdown:

```markdown
1.     indented code

   paragraph

       more code

```

Gives this incorrect output:

```html
<ol>
<li>
<pre><code>indented code
</code></pre>
<p>paragraph</p>
<pre><code>more code
</code></pre>
</li>
</ol>

```

Example 244

This markdown:

```markdown
1.      indented code

   paragraph

       more code

```

Gives this incorrect output:

```html
<ol>
<li>
<pre><code> indented code
</code></pre>
<p>paragraph</p>
<pre><code>more code
</code></pre>
</li>
</ol>

```

Example 245

This markdown:

```markdown
   foo

bar

```

Gives this incorrect output:

```html
<p>foo</p>
<p>bar</p>

```

Example 246

This markdown:

```markdown
-    foo

  bar

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
</ul>
<p>bar</p>

```

Example 247

This markdown:

```markdown
-  foo

   bar

```

Gives this incorrect output:

```html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

```

Example 248

This markdown:

```markdown
-
  foo
-
  ```
  bar
  ```
-
      baz

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
<li>
<pre><code>bar
</code></pre>
</li>
<li>
<pre><code>baz
</code></pre>
</li>
</ul>

```

Example 249

This markdown:

```markdown
-   
  foo

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
</ul>

```

Example 250

This markdown:

```markdown
-

  foo

```

Gives this incorrect output:

```html
<ul>
<li></li>
</ul>
<p>foo</p>

```

Example 251

This markdown:

```markdown
- foo
-
- bar

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
<li></li>
<li>bar</li>
</ul>

```

Example 252

This markdown:

```markdown
- foo
-   
- bar

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
<li></li>
<li>bar</li>
</ul>

```

Example 253

This markdown:

```markdown
1. foo
2.
3. bar

```

Gives this incorrect output:

```html
<ol>
<li>foo</li>
<li></li>
<li>bar</li>
</ol>

```

Example 254

This markdown:

```markdown
*

```

Gives this incorrect output:

```html
<ul>
<li></li>
</ul>

```

Example 255

This markdown:

```markdown
foo
*

foo
1.

```

Gives this incorrect output:

```html
<p>foo
*</p>
<p>foo
1.</p>

```

Example 256

This markdown:

```markdown
 1.  A paragraph
     with two lines.

         indented code

     > A block quote.

```

Gives this incorrect output:

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 257

This markdown:

```markdown
  1.  A paragraph
      with two lines.

          indented code

      > A block quote.

```

Gives this incorrect output:

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 258

This markdown:

```markdown
   1.  A paragraph
       with two lines.

           indented code

       > A block quote.

```

Gives this incorrect output:

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 259

This markdown:

```markdown
    1.  A paragraph
        with two lines.

            indented code

        > A block quote.

```

Gives this incorrect output:

```html
<pre><code>1.  A paragraph
    with two lines.

        indented code

    &gt; A block quote.
</code></pre>

```

Example 260

This markdown:

```markdown
  1.  A paragraph
with two lines.

          indented code

      > A block quote.

```

Gives this incorrect output:

```html
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>

```

Example 261

This markdown:

```markdown
  1.  A paragraph
    with two lines.

```

Gives this incorrect output:

```html
<ol>
<li>A paragraph
with two lines.</li>
</ol>

```

Example 262

This markdown:

```markdown
> 1. > Blockquote
continued here.

```

Gives this incorrect output:

```html
<blockquote>
<ol>
<li>
<blockquote>
<p>Blockquote
continued here.</p>
</blockquote>
</li>
</ol>
</blockquote>

```

Example 263

This markdown:

```markdown
> 1. > Blockquote
> continued here.

```

Gives this incorrect output:

```html
<blockquote>
<ol>
<li>
<blockquote>
<p>Blockquote
continued here.</p>
</blockquote>
</li>
</ol>
</blockquote>

```

Example 264

This markdown:

```markdown
- foo
  - bar
    - baz
      - boo

```

Gives this incorrect output:

```html
<ul>
<li>foo
<ul>
<li>bar
<ul>
<li>baz
<ul>
<li>boo</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>

```

Example 265

This markdown:

```markdown
- foo
 - bar
  - baz
   - boo

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
<li>bar</li>
<li>baz</li>
<li>boo</li>
</ul>

```

Example 266

This markdown:

```markdown
10) foo
    - bar

```

Gives this incorrect output:

```html
<ol start="10">
<li>foo
<ul>
<li>bar</li>
</ul>
</li>
</ol>

```

Example 267

This markdown:

```markdown
10) foo
   - bar

```

Gives this incorrect output:

```html
<ol start="10">
<li>foo</li>
</ol>
<ul>
<li>bar</li>
</ul>

```

Example 268

This markdown:

```markdown
- - foo

```

Gives this incorrect output:

```html
<ul>
<li>
<ul>
<li>foo</li>
</ul>
</li>
</ul>

```

Example 269

This markdown:

```markdown
1. - 2. foo

```

Gives this incorrect output:

```html
<ol>
<li>
<ul>
<li>
<ol start="2">
<li>foo</li>
</ol>
</li>
</ul>
</li>
</ol>

```

Example 270

This markdown:

```markdown
- # Foo
- Bar
  ---
  baz

```

Gives this incorrect output:

```html
<ul>
<li>
<h1>Foo</h1>
</li>
<li>
<h2>Bar</h2>
baz</li>
</ul>

```

### Paragraphs

Example 190

This markdown:

```markdown
aaa
bbb

ccc
ddd

```

Gives this incorrect output:

```html
<p>aaa
bbb</p>
<p>ccc
ddd</p>

```

Example 192

This markdown:

```markdown
  aaa
 bbb

```

Gives this incorrect output:

```html
<p>aaa
bbb</p>

```

Example 193

This markdown:

```markdown
aaa
             bbb
                                       ccc

```

Gives this incorrect output:

```html
<p>aaa
bbb
ccc</p>

```

Example 194

This markdown:

```markdown
   aaa
bbb

```

Gives this incorrect output:

```html
<p>aaa
bbb</p>

```

Example 196

This markdown:

```markdown
aaa     
bbb     

```

Gives this incorrect output:

```html
<p>aaa<br />
bbb</p>

```

### Precedence

Example 12

This markdown:

```markdown
- `one
- two`

```

Gives this incorrect output:

```html
<ul>
<li>`one</li>
<li>two`</li>
</ul>

```

### Blank lines

Example 197

This markdown:

```markdown
  

aaa
  

# aaa

  

```

Gives this incorrect output:

```html
<p>aaa</p>
<h1>aaa</h1>

```

### Emphasis and strong emphasis

Example 351

This markdown:

```markdown
a * foo bar*

```

Gives this incorrect output:

```html
<p>a * foo bar*</p>

```

Example 352

This markdown:

```markdown
a*"foo"*

```

Gives this incorrect output:

```html
<p>a*&quot;foo&quot;*</p>

```

Example 353

This markdown:

```markdown
* a *

```

Gives this incorrect output:

```html
<p>* a *</p>

```

Example 356

This markdown:

```markdown
_foo bar_

```

Gives this incorrect output:

```html
<p><em>foo bar</em></p>

```

Example 363

This markdown:

```markdown
foo-_(bar)_

```

Gives this incorrect output:

```html
<p>foo-<em>(bar)</em></p>

```

Example 364

This markdown:

```markdown
_foo*

```

Gives this incorrect output:

```html
<p>_foo*</p>

```

Example 365

This markdown:

```markdown
*foo bar *

```

Gives this incorrect output:

```html
<p>*foo bar *</p>

```

Example 366

This markdown:

```markdown
*foo bar
*

```

Gives this incorrect output:

```html
<p>*foo bar
*</p>

```

Example 367

This markdown:

```markdown
*(*foo)

```

Gives this incorrect output:

```html
<p>*(*foo)</p>

```

Example 368

This markdown:

```markdown
*(*foo*)*

```

Gives this incorrect output:

```html
<p><em>(<em>foo</em>)</em></p>

```

Example 372

This markdown:

```markdown
_(_foo_)_

```

Gives this incorrect output:

```html
<p><em>(<em>foo</em>)</em></p>

```

Example 375

This markdown:

```markdown
_foo_bar_baz_

```

Gives this incorrect output:

```html
<p><em>foo_bar_baz</em></p>

```

Example 376

This markdown:

```markdown
_(bar)_.

```

Gives this incorrect output:

```html
<p><em>(bar)</em>.</p>

```

Example 378

This markdown:

```markdown
** foo bar**

```

Gives this incorrect output:

```html
<p>** foo bar**</p>

```

Example 379

This markdown:

```markdown
a**"foo"**

```

Gives this incorrect output:

```html
<p>a**&quot;foo&quot;**</p>

```

Example 381

This markdown:

```markdown
__foo bar__

```

Gives this incorrect output:

```html
<p><strong>foo bar</strong></p>

```

Example 383

This markdown:

```markdown
__
foo bar__

```

Gives this incorrect output:

```html
<p>__
foo bar__</p>

```

Example 388

This markdown:

```markdown
__foo, __bar__, baz__

```

Gives this incorrect output:

```html
<p><strong>foo, <strong>bar</strong>, baz</strong></p>

```

Example 389

This markdown:

```markdown
foo-__(bar)__

```

Gives this incorrect output:

```html
<p>foo-<strong>(bar)</strong></p>

```

Example 390

This markdown:

```markdown
**foo bar **

```

Gives this incorrect output:

```html
<p>**foo bar **</p>

```

Example 391

This markdown:

```markdown
**(**foo)

```

Gives this incorrect output:

```html
<p>**(**foo)</p>

```

Example 392

This markdown:

```markdown
*(**foo**)*

```

Gives this incorrect output:

```html
<p><em>(<strong>foo</strong>)</em></p>

```

Example 393

This markdown:

```markdown
**Gomphocarpus (*Gomphocarpus physocarpus*, syn.
*Asclepias physocarpa*)**

```

Gives this incorrect output:

```html
<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.
<em>Asclepias physocarpa</em>)</strong></p>

```

Example 394

This markdown:

```markdown
**foo "*bar*" foo**

```

Gives this incorrect output:

```html
<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>

```

Example 398

This markdown:

```markdown
_(__foo__)_

```

Gives this incorrect output:

```html
<p><em>(<strong>foo</strong>)</em></p>

```

Example 401

This markdown:

```markdown
__foo__bar__baz__

```

Gives this incorrect output:

```html
<p><strong>foo__bar__baz</strong></p>

```

Example 402

This markdown:

```markdown
__(bar)__.

```

Gives this incorrect output:

```html
<p><strong>(bar)</strong>.</p>

```

Example 403

This markdown:

```markdown
*foo [bar](/url)*

```

Gives this incorrect output:

```html
<p><em>foo <a href="/url">bar</a></em></p>

```

Example 404

This markdown:

```markdown
*foo
bar*

```

Gives this incorrect output:

```html
<p><em>foo
bar</em></p>

```

Example 405

This markdown:

```markdown
_foo __bar__ baz_

```

Gives this incorrect output:

```html
<p><em>foo <strong>bar</strong> baz</em></p>

```

Example 406

This markdown:

```markdown
_foo _bar_ baz_

```

Gives this incorrect output:

```html
<p><em>foo <em>bar</em> baz</em></p>

```

Example 407

This markdown:

```markdown
__foo_ bar_

```

Gives this incorrect output:

```html
<p><em><em>foo</em> bar</em></p>

```

Example 408

This markdown:

```markdown
*foo *bar**

```

Gives this incorrect output:

```html
<p><em>foo <em>bar</em></em></p>

```

Example 409

This markdown:

```markdown
*foo **bar** baz*

```

Gives this incorrect output:

```html
<p><em>foo <strong>bar</strong> baz</em></p>

```

Example 410

This markdown:

```markdown
*foo**bar**baz*

```

Gives this incorrect output:

```html
<p><em>foo<strong>bar</strong>baz</em></p>

```

Example 411

This markdown:

```markdown
*foo**bar*

```

Gives this incorrect output:

```html
<p><em>foo**bar</em></p>

```

Example 412

This markdown:

```markdown
***foo** bar*

```

Gives this incorrect output:

```html
<p><em><strong>foo</strong> bar</em></p>

```

Example 413

This markdown:

```markdown
*foo **bar***

```

Gives this incorrect output:

```html
<p><em>foo <strong>bar</strong></em></p>

```

Example 414

This markdown:

```markdown
*foo**bar***

```

Gives this incorrect output:

```html
<p><em>foo<strong>bar</strong></em></p>

```

Example 415

This markdown:

```markdown
foo***bar***baz

```

Gives this incorrect output:

```html
<p>foo<em><strong>bar</strong></em>baz</p>

```

Example 416

This markdown:

```markdown
foo******bar*********baz

```

Gives this incorrect output:

```html
<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>

```

Example 417

This markdown:

```markdown
*foo **bar *baz* bim** bop*

```

Gives this incorrect output:

```html
<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>

```

Example 418

This markdown:

```markdown
*foo [*bar*](/url)*

```

Gives this incorrect output:

```html
<p><em>foo <a href="/url"><em>bar</em></a></em></p>

```

Example 419

This markdown:

```markdown
** is not an empty emphasis

```

Gives this incorrect output:

```html
<p>** is not an empty emphasis</p>

```

Example 420

This markdown:

```markdown
**** is not an empty strong emphasis

```

Gives this incorrect output:

```html
<p>**** is not an empty strong emphasis</p>

```

Example 421

This markdown:

```markdown
**foo [bar](/url)**

```

Gives this incorrect output:

```html
<p><strong>foo <a href="/url">bar</a></strong></p>

```

Example 422

This markdown:

```markdown
**foo
bar**

```

Gives this incorrect output:

```html
<p><strong>foo
bar</strong></p>

```

Example 423

This markdown:

```markdown
__foo _bar_ baz__

```

Gives this incorrect output:

```html
<p><strong>foo <em>bar</em> baz</strong></p>

```

Example 424

This markdown:

```markdown
__foo __bar__ baz__

```

Gives this incorrect output:

```html
<p><strong>foo <strong>bar</strong> baz</strong></p>

```

Example 425

This markdown:

```markdown
____foo__ bar__

```

Gives this incorrect output:

```html
<p><strong><strong>foo</strong> bar</strong></p>

```

Example 426

This markdown:

```markdown
**foo **bar****

```

Gives this incorrect output:

```html
<p><strong>foo <strong>bar</strong></strong></p>

```

Example 427

This markdown:

```markdown
**foo *bar* baz**

```

Gives this incorrect output:

```html
<p><strong>foo <em>bar</em> baz</strong></p>

```

Example 428

This markdown:

```markdown
**foo*bar*baz**

```

Gives this incorrect output:

```html
<p><strong>foo<em>bar</em>baz</strong></p>

```

Example 429

This markdown:

```markdown
***foo* bar**

```

Gives this incorrect output:

```html
<p><strong><em>foo</em> bar</strong></p>

```

Example 430

This markdown:

```markdown
**foo *bar***

```

Gives this incorrect output:

```html
<p><strong>foo <em>bar</em></strong></p>

```

Example 431

This markdown:

```markdown
**foo *bar **baz**
bim* bop**

```

Gives this incorrect output:

```html
<p><strong>foo <em>bar <strong>baz</strong>
bim</em> bop</strong></p>

```

Example 432

This markdown:

```markdown
**foo [*bar*](/url)**

```

Gives this incorrect output:

```html
<p><strong>foo <a href="/url"><em>bar</em></a></strong></p>

```

Example 434

This markdown:

```markdown
____ is not an empty strong emphasis

```

Gives this incorrect output:

```html
<p>____ is not an empty strong emphasis</p>

```

Example 435

This markdown:

```markdown
foo ***

```

Gives this incorrect output:

```html
<p>foo ***</p>

```

Example 436

This markdown:

```markdown
foo *\**

```

Gives this incorrect output:

```html
<p>foo <em>*</em></p>

```

Example 438

This markdown:

```markdown
foo *****

```

Gives this incorrect output:

```html
<p>foo *****</p>

```

Example 439

This markdown:

```markdown
foo **\***

```

Gives this incorrect output:

```html
<p>foo <strong>*</strong></p>

```

Example 441

This markdown:

```markdown
**foo*

```

Gives this incorrect output:

```html
<p>*<em>foo</em></p>

```

Example 442

This markdown:

```markdown
*foo**

```

Gives this incorrect output:

```html
<p><em>foo</em>*</p>

```

Example 443

This markdown:

```markdown
***foo**

```

Gives this incorrect output:

```html
<p>*<strong>foo</strong></p>

```

Example 444

This markdown:

```markdown
****foo*

```

Gives this incorrect output:

```html
<p>***<em>foo</em></p>

```

Example 445

This markdown:

```markdown
**foo***

```

Gives this incorrect output:

```html
<p><strong>foo</strong>*</p>

```

Example 446

This markdown:

```markdown
*foo****

```

Gives this incorrect output:

```html
<p><em>foo</em>***</p>

```

Example 448

This markdown:

```markdown
foo _\__

```

Gives this incorrect output:

```html
<p>foo <em>_</em></p>

```

Example 449

This markdown:

```markdown
foo _*_

```

Gives this incorrect output:

```html
<p>foo <em>*</em></p>

```

Example 451

This markdown:

```markdown
foo __\___

```

Gives this incorrect output:

```html
<p>foo <strong>_</strong></p>

```

Example 452

This markdown:

```markdown
foo __*__

```

Gives this incorrect output:

```html
<p>foo <strong>*</strong></p>

```

Example 453

This markdown:

```markdown
__foo_

```

Gives this incorrect output:

```html
<p>_<em>foo</em></p>

```

Example 454

This markdown:

```markdown
_foo__

```

Gives this incorrect output:

```html
<p><em>foo</em>_</p>

```

Example 455

This markdown:

```markdown
___foo__

```

Gives this incorrect output:

```html
<p>_<strong>foo</strong></p>

```

Example 456

This markdown:

```markdown
____foo_

```

Gives this incorrect output:

```html
<p>___<em>foo</em></p>

```

Example 457

This markdown:

```markdown
__foo___

```

Gives this incorrect output:

```html
<p><strong>foo</strong>_</p>

```

Example 458

This markdown:

```markdown
_foo____

```

Gives this incorrect output:

```html
<p><em>foo</em>___</p>

```

Example 460

This markdown:

```markdown
*_foo_*

```

Gives this incorrect output:

```html
<p><em><em>foo</em></em></p>

```

Example 461

This markdown:

```markdown
__foo__

```

Gives this incorrect output:

```html
<p><strong>foo</strong></p>

```

Example 462

This markdown:

```markdown
_*foo*_

```

Gives this incorrect output:

```html
<p><em><em>foo</em></em></p>

```

Example 463

This markdown:

```markdown
****foo****

```

Gives this incorrect output:

```html
<p><strong><strong>foo</strong></strong></p>

```

Example 464

This markdown:

```markdown
____foo____

```

Gives this incorrect output:

```html
<p><strong><strong>foo</strong></strong></p>

```

Example 465

This markdown:

```markdown
******foo******

```

Gives this incorrect output:

```html
<p><strong><strong><strong>foo</strong></strong></strong></p>

```

Example 466

This markdown:

```markdown
***foo***

```

Gives this incorrect output:

```html
<p><em><strong>foo</strong></em></p>

```

Example 467

This markdown:

```markdown
_____foo_____

```

Gives this incorrect output:

```html
<p><em><strong><strong>foo</strong></strong></em></p>

```

Example 469

This markdown:

```markdown
*foo __bar *baz bim__ bam*

```

Gives this incorrect output:

```html
<p><em>foo <strong>bar *baz bim</strong> bam</em></p>

```

Example 470

This markdown:

```markdown
**foo **bar baz**

```

Gives this incorrect output:

```html
<p>**foo <strong>bar baz</strong></p>

```

Example 471

This markdown:

```markdown
*foo *bar baz*

```

Gives this incorrect output:

```html
<p>*foo <em>bar baz</em></p>

```

Example 472

This markdown:

```markdown
*[bar*](/url)

```

Gives this incorrect output:

```html
<p>*<a href="/url">bar*</a></p>

```

Example 474

This markdown:

```markdown
*<img src="foo" title="*"/>

```

Gives this incorrect output:

```html
<p>*<img src="foo" title="*"/></p>

```

Example 475

This markdown:

```markdown
**<a href="**">

```

Gives this incorrect output:

```html
<p>**<a href="**"></p>

```

Example 476

This markdown:

```markdown
__<a href="__">

```

Gives this incorrect output:

```html
<p>__<a href="__"></p>

```

Example 477

This markdown:

```markdown
*a `*`*

```

Gives this incorrect output:

```html
<p><em>a <code>*</code></em></p>

```

Example 478

This markdown:

```markdown
_a `_`_

```

Gives this incorrect output:

```html
<p><em>a <code>_</code></em></p>

```

Example 479

This markdown:

```markdown
**a<http://foo.bar/?q=**>

```

Gives this incorrect output:

```html
<p>**a<a href="http://foo.bar/?q=**">http://foo.bar/?q=**</a></p>

```

Example 480

This markdown:

```markdown
__a<http://foo.bar/?q=__>

```

Gives this incorrect output:

```html
<p>__a<a href="http://foo.bar/?q=__">http://foo.bar/?q=__</a></p>

```

### HTML blocks

Example 118

This markdown:

```markdown
<table><tr><td>
<pre>
**Hello**,

_world_.
</pre>
</td></tr></table>

```

Gives this incorrect output:

```html
<table><tr><td>
<pre>
**Hello**,
<p><em>world</em>.
</pre></p>
</td></tr></table>

```

Example 119

This markdown:

```markdown
<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>

okay.

```

Gives this incorrect output:

```html
<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>
<p>okay.</p>

```

Example 120

This markdown:

```markdown
 <div>
  *hello*
         <foo><a>

```

Gives this incorrect output:

```html
 <div>
  *hello*
         <foo><a>

```

Example 121

This markdown:

```markdown
</div>
*foo*

```

Gives this incorrect output:

```html
</div>
*foo*

```

Example 122

This markdown:

```markdown
<DIV CLASS="foo">

*Markdown*

</DIV>

```

Gives this incorrect output:

```html
<DIV CLASS="foo">
<p><em>Markdown</em></p>
</DIV>

```

Example 123

This markdown:

```markdown
<div id="foo"
  class="bar">
</div>

```

Gives this incorrect output:

```html
<div id="foo"
  class="bar">
</div>

```

Example 124

This markdown:

```markdown
<div id="foo" class="bar
  baz">
</div>

```

Gives this incorrect output:

```html
<div id="foo" class="bar
  baz">
</div>

```

Example 125

This markdown:

```markdown
<div>
*foo*

*bar*

```

Gives this incorrect output:

```html
<div>
*foo*
<p><em>bar</em></p>

```

Example 126

This markdown:

```markdown
<div id="foo"
*hi*

```

Gives this incorrect output:

```html
<div id="foo"
*hi*

```

Example 127

This markdown:

```markdown
<div class
foo

```

Gives this incorrect output:

```html
<div class
foo

```

Example 128

This markdown:

```markdown
<div *???-&&&-<---
*foo*

```

Gives this incorrect output:

```html
<div *???-&&&-<---
*foo*

```

Example 129

This markdown:

```markdown
<div><a href="bar">*foo*</a></div>

```

Gives this incorrect output:

```html
<div><a href="bar">*foo*</a></div>

```

Example 130

This markdown:

```markdown
<table><tr><td>
foo
</td></tr></table>

```

Gives this incorrect output:

```html
<table><tr><td>
foo
</td></tr></table>

```

Example 131

This markdown:

```markdown
<div></div>
``` c
int x = 33;
```

```

Gives this incorrect output:

```html
<div></div>
``` c
int x = 33;
```

```

Example 132

This markdown:

```markdown
<a href="foo">
*bar*
</a>

```

Gives this incorrect output:

```html
<a href="foo">
*bar*
</a>

```

Example 133

This markdown:

```markdown
<Warning>
*bar*
</Warning>

```

Gives this incorrect output:

```html
<Warning>
*bar*
</Warning>

```

Example 134

This markdown:

```markdown
<i class="foo">
*bar*
</i>

```

Gives this incorrect output:

```html
<i class="foo">
*bar*
</i>

```

Example 135

This markdown:

```markdown
</ins>
*bar*

```

Gives this incorrect output:

```html
</ins>
*bar*

```

Example 136

This markdown:

```markdown
<del>
*foo*
</del>

```

Gives this incorrect output:

```html
<del>
*foo*
</del>

```

Example 137

This markdown:

```markdown
<del>

*foo*

</del>

```

Gives this incorrect output:

```html
<del>
<p><em>foo</em></p>
</del>

```

Example 138

This markdown:

```markdown
<del>*foo*</del>

```

Gives this incorrect output:

```html
<p><del><em>foo</em></del></p>

```

Example 139

This markdown:

```markdown
<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
okay

```

Gives this incorrect output:

```html
<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
<p>okay</p>

```

Example 140

This markdown:

```markdown
<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
</script>
okay

```

Gives this incorrect output:

```html
<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
</script>
<p>okay</p>

```

Example 141

This markdown:

```markdown
<style
  type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
okay

```

Gives this incorrect output:

```html
<style
  type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
<p>okay</p>

```

Example 142

This markdown:

```markdown
<style
  type="text/css">

foo

```

Gives this incorrect output:

```html
<style
  type="text/css">

foo

```

Example 143

This markdown:

```markdown
> <div>
> foo

bar

```

Gives this incorrect output:

```html
<blockquote>
<div>
foo
</blockquote>
<p>bar</p>

```

Example 144

This markdown:

```markdown
- <div>
- foo

```

Gives this incorrect output:

```html
<ul>
<li>
<div>
</li>
<li>foo</li>
</ul>

```

Example 145

This markdown:

```markdown
<style>p{color:red;}</style>
*foo*

```

Gives this incorrect output:

```html
<style>p{color:red;}</style>
<p><em>foo</em></p>

```

Example 146

This markdown:

```markdown
<!-- foo -->*bar*
*baz*

```

Gives this incorrect output:

```html
<!-- foo -->*bar*
<p><em>baz</em></p>

```

Example 147

This markdown:

```markdown
<script>
foo
</script>1. *bar*

```

Gives this incorrect output:

```html
<script>
foo
</script>1. *bar*

```

Example 148

This markdown:

```markdown
<!-- Foo

bar
   baz -->
okay

```

Gives this incorrect output:

```html
<!-- Foo

bar
   baz -->
<p>okay</p>

```

Example 149

This markdown:

```markdown
<?php

  echo '>';

?>
okay

```

Gives this incorrect output:

```html
<?php

  echo '>';

?>
<p>okay</p>

```

Example 150

This markdown:

```markdown
<!DOCTYPE html>

```

Gives this incorrect output:

```html
<!DOCTYPE html>

```

Example 151

This markdown:

```markdown
<![CDATA[
function matchwo(a,b)
{
  if (a < b && a < 0) then {
    return 1;

  } else {

    return 0;
  }
}
]]>
okay

```

Gives this incorrect output:

```html
<![CDATA[
function matchwo(a,b)
{
  if (a < b && a < 0) then {
    return 1;

  } else {

    return 0;
  }
}
]]>
<p>okay</p>

```

Example 152

This markdown:

```markdown
  <!-- foo -->

    <!-- foo -->

```

Gives this incorrect output:

```html
  <!-- foo -->
<pre><code>&lt;!-- foo --&gt;
</code></pre>

```

Example 153

This markdown:

```markdown
  <div>

    <div>

```

Gives this incorrect output:

```html
  <div>
<pre><code>&lt;div&gt;
</code></pre>

```

Example 154

This markdown:

```markdown
Foo
<div>
bar
</div>

```

Gives this incorrect output:

```html
<p>Foo</p>
<div>
bar
</div>

```

Example 155

This markdown:

```markdown
<div>
bar
</div>
*foo*

```

Gives this incorrect output:

```html
<div>
bar
</div>
*foo*

```

Example 156

This markdown:

```markdown
Foo
<a href="bar">
baz

```

Gives this incorrect output:

```html
<p>Foo
<a href="bar">
baz</p>

```

Example 157

This markdown:

```markdown
<div>

*Emphasized* text.

</div>

```

Gives this incorrect output:

```html
<div>
<p><em>Emphasized</em> text.</p>
</div>

```

Example 158

This markdown:

```markdown
<div>
*Emphasized* text.
</div>

```

Gives this incorrect output:

```html
<div>
*Emphasized* text.
</div>

```

Example 159

This markdown:

```markdown
<table>

<tr>

<td>
Hi
</td>

</tr>

</table>

```

Gives this incorrect output:

```html
<table>
<tr>
<td>
Hi
</td>
</tr>
</table>

```

Example 160

This markdown:

```markdown
<table>

  <tr>

    <td>
      Hi
    </td>

  </tr>

</table>

```

Gives this incorrect output:

```html
<table>
  <tr>
<pre><code>&lt;td&gt;
  Hi
&lt;/td&gt;
</code></pre>
  </tr>
</table>

```

### Links

Example 481

This markdown:

```markdown
[link](/uri "title")

```

Gives this incorrect output:

```html
<p><a href="/uri" title="title">link</a></p>

```

Example 484

This markdown:

```markdown
[link](<>)

```

Gives this incorrect output:

```html
<p><a href="">link</a></p>

```

Example 485

This markdown:

```markdown
[link](/my uri)

```

Gives this incorrect output:

```html
<p>[link](/my uri)</p>

```

Example 486

This markdown:

```markdown
[link](</my uri>)

```

Gives this incorrect output:

```html
<p><a href="/my%20uri">link</a></p>

```

Example 487

This markdown:

```markdown
[link](foo
bar)

```

Gives this incorrect output:

```html
<p>[link](foo
bar)</p>

```

Example 488

This markdown:

```markdown
[link](<foo
bar>)

```

Gives this incorrect output:

```html
<p>[link](<foo
bar>)</p>

```

Example 489

This markdown:

```markdown
[a](<b)c>)

```

Gives this incorrect output:

```html
<p><a href="b)c">a</a></p>

```

Example 490

This markdown:

```markdown
[link](<foo\>)

```

Gives this incorrect output:

```html
<p>[link](&lt;foo&gt;)</p>

```

Example 491

This markdown:

```markdown
[a](<b)c
[a](<b)c>
[a](<b>c)

```

Gives this incorrect output:

```html
<p>[a](&lt;b)c
[a](&lt;b)c&gt;
[a](<b>c)</p>

```

Example 492

This markdown:

```markdown
[link](\(foo\))

```

Gives this incorrect output:

```html
<p><a href="(foo)">link</a></p>

```

Example 493

This markdown:

```markdown
[link](foo(and(bar)))

```

Gives this incorrect output:

```html
<p><a href="foo(and(bar))">link</a></p>

```

Example 494

This markdown:

```markdown
[link](foo\(and\(bar\))

```

Gives this incorrect output:

```html
<p><a href="foo(and(bar)">link</a></p>

```

Example 495

This markdown:

```markdown
[link](<foo(and(bar)>)

```

Gives this incorrect output:

```html
<p><a href="foo(and(bar)">link</a></p>

```

Example 496

This markdown:

```markdown
[link](foo\)\:)

```

Gives this incorrect output:

```html
<p><a href="foo):">link</a></p>

```

Example 498

This markdown:

```markdown
[link](foo\bar)

```

Gives this incorrect output:

```html
<p><a href="foo%5Cbar">link</a></p>

```

Example 499

This markdown:

```markdown
[link](foo%20b&auml;)

```

Gives this incorrect output:

```html
<p><a href="foo%20b%C3%A4">link</a></p>

```

Example 500

This markdown:

```markdown
[link]("title")

```

Gives this incorrect output:

```html
<p><a href="%22title%22">link</a></p>

```

Example 501

This markdown:

```markdown
[link](/url "title")
[link](/url 'title')
[link](/url (title))

```

Gives this incorrect output:

```html
<p><a href="/url" title="title">link</a>
<a href="/url" title="title">link</a>
<a href="/url" title="title">link</a></p>

```

Example 502

This markdown:

```markdown
[link](/url "title \"&quot;")

```

Gives this incorrect output:

```html
<p><a href="/url" title="title &quot;&quot;">link</a></p>

```

Example 503

This markdown:

```markdown
[link](/url "title")

```

Gives this incorrect output:

```html
<p><a href="/url%C2%A0%22title%22">link</a></p>

```

Example 504

This markdown:

```markdown
[link](/url "title "and" title")

```

Gives this incorrect output:

```html
<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>

```

Example 505

This markdown:

```markdown
[link](/url 'title "and" title')

```

Gives this incorrect output:

```html
<p><a href="/url" title="title &quot;and&quot; title">link</a></p>

```

Example 506

This markdown:

```markdown
[link](   /uri
  "title"  )

```

Gives this incorrect output:

```html
<p><a href="/uri" title="title">link</a></p>

```

Example 507

This markdown:

```markdown
[link] (/uri)

```

Gives this incorrect output:

```html
<p>[link] (/uri)</p>

```

Example 508

This markdown:

```markdown
[link [foo [bar]]](/uri)

```

Gives this incorrect output:

```html
<p><a href="/uri">link [foo [bar]]</a></p>

```

Example 509

This markdown:

```markdown
[link] bar](/uri)

```

Gives this incorrect output:

```html
<p>[link] bar](/uri)</p>

```

Example 510

This markdown:

```markdown
[link [bar](/uri)

```

Gives this incorrect output:

```html
<p>[link <a href="/uri">bar</a></p>

```

Example 511

This markdown:

```markdown
[link \[bar](/uri)

```

Gives this incorrect output:

```html
<p><a href="/uri">link [bar</a></p>

```

Example 512

This markdown:

```markdown
[link *foo **bar** `#`*](/uri)

```

Gives this incorrect output:

```html
<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>

```

Example 513

This markdown:

```markdown
[![moon](moon.jpg)](/uri)

```

Gives this incorrect output:

```html
<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>

```

Example 514

This markdown:

```markdown
[foo [bar](/uri)](/uri)

```

Gives this incorrect output:

```html
<p>[foo <a href="/uri">bar</a>](/uri)</p>

```

Example 515

This markdown:

```markdown
[foo *[bar [baz](/uri)](/uri)*](/uri)

```

Gives this incorrect output:

```html
<p>[foo <em>[bar <a href="/uri">baz</a>](/uri)</em>](/uri)</p>

```

Example 516

This markdown:

```markdown
![[[foo](uri1)](uri2)](uri3)

```

Gives this incorrect output:

```html
<p><img src="uri3" alt="[foo](uri2)" /></p>

```

Example 517

This markdown:

```markdown
*[foo*](/uri)

```

Gives this incorrect output:

```html
<p>*<a href="/uri">foo*</a></p>

```

Example 518

This markdown:

```markdown
[foo *bar](baz*)

```

Gives this incorrect output:

```html
<p><a href="baz*">foo *bar</a></p>

```

Example 519

This markdown:

```markdown
*foo [bar* baz]

```

Gives this incorrect output:

```html
<p><em>foo [bar</em> baz]</p>

```

Example 520

This markdown:

```markdown
[foo <bar attr="](baz)">

```

Gives this incorrect output:

```html
<p>[foo <bar attr="](baz)"></p>

```

Example 521

This markdown:

```markdown
[foo`](/uri)`

```

Gives this incorrect output:

```html
<p>[foo<code>](/uri)</code></p>

```

Example 522

This markdown:

```markdown
[foo<http://example.com/?search=](uri)>

```

Gives this incorrect output:

```html
<p>[foo<a href="http://example.com/?search=%5D(uri)">http://example.com/?search=](uri)</a></p>

```

Example 523

This markdown:

```markdown
[foo][bar]

[bar]: /url "title"

```

Gives this incorrect output:

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 524

This markdown:

```markdown
[link [foo [bar]]][ref]

[ref]: /uri

```

Gives this incorrect output:

```html
<p><a href="/uri">link [foo [bar]]</a></p>

```

Example 525

This markdown:

```markdown
[link \[bar][ref]

[ref]: /uri

```

Gives this incorrect output:

```html
<p><a href="/uri">link [bar</a></p>

```

Example 526

This markdown:

```markdown
[link *foo **bar** `#`*][ref]

[ref]: /uri

```

Gives this incorrect output:

```html
<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>

```

Example 527

This markdown:

```markdown
[![moon](moon.jpg)][ref]

[ref]: /uri

```

Gives this incorrect output:

```html
<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>

```

Example 528

This markdown:

```markdown
[foo [bar](/uri)][ref]

[ref]: /uri

```

Gives this incorrect output:

```html
<p>[foo <a href="/uri">bar</a>]<a href="/uri">ref</a></p>

```

Example 529

This markdown:

```markdown
[foo *bar [baz][ref]*][ref]

[ref]: /uri

```

Gives this incorrect output:

```html
<p>[foo <em>bar <a href="/uri">baz</a></em>]<a href="/uri">ref</a></p>

```

Example 530

This markdown:

```markdown
*[foo*][ref]

[ref]: /uri

```

Gives this incorrect output:

```html
<p>*<a href="/uri">foo*</a></p>

```

Example 531

This markdown:

```markdown
[foo *bar][ref]

[ref]: /uri

```

Gives this incorrect output:

```html
<p><a href="/uri">foo *bar</a></p>

```

Example 532

This markdown:

```markdown
[foo <bar attr="][ref]">

[ref]: /uri

```

Gives this incorrect output:

```html
<p>[foo <bar attr="][ref]"></p>

```

Example 533

This markdown:

```markdown
[foo`][ref]`

[ref]: /uri

```

Gives this incorrect output:

```html
<p>[foo<code>][ref]</code></p>

```

Example 534

This markdown:

```markdown
[foo<http://example.com/?search=][ref]>

[ref]: /uri

```

Gives this incorrect output:

```html
<p>[foo<a href="http://example.com/?search=%5D%5Bref%5D">http://example.com/?search=][ref]</a></p>

```

Example 535

This markdown:

```markdown
[foo][BaR]

[bar]: /url "title"

```

Gives this incorrect output:

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 536

This markdown:

```markdown
[Толпой][Толпой] is a Russian word.

[ТОЛПОЙ]: /url

```

Gives this incorrect output:

```html
<p><a href="/url">Толпой</a> is a Russian word.</p>

```

Example 537

This markdown:

```markdown
[Foo
  bar]: /url

[Baz][Foo bar]

```

Gives this incorrect output:

```html
<p><a href="/url">Baz</a></p>

```

Example 538

This markdown:

```markdown
[foo] [bar]

[bar]: /url "title"

```

Gives this incorrect output:

```html
<p>[foo] <a href="/url" title="title">bar</a></p>

```

Example 539

This markdown:

```markdown
[foo]
[bar]

[bar]: /url "title"

```

Gives this incorrect output:

```html
<p>[foo]
<a href="/url" title="title">bar</a></p>

```

Example 540

This markdown:

```markdown
[foo]: /url1

[foo]: /url2

[bar][foo]

```

Gives this incorrect output:

```html
<p><a href="/url1">bar</a></p>

```

Example 541

This markdown:

```markdown
[bar][foo\!]

[foo!]: /url

```

Gives this incorrect output:

```html
<p>[bar][foo!]</p>

```

Example 542

This markdown:

```markdown
[foo][ref[]

[ref[]: /uri

```

Gives this incorrect output:

```html
<p>[foo][ref[]</p>
<p>[ref[]: /uri</p>

```

Example 543

This markdown:

```markdown
[foo][ref[bar]]

[ref[bar]]: /uri

```

Gives this incorrect output:

```html
<p>[foo][ref[bar]]</p>
<p>[ref[bar]]: /uri</p>

```

Example 544

This markdown:

```markdown
[[[foo]]]

[[[foo]]]: /url

```

Gives this incorrect output:

```html
<p>[[[foo]]]</p>
<p>[[[foo]]]: /url</p>

```

Example 545

This markdown:

```markdown
[foo][ref\[]

[ref\[]: /uri

```

Gives this incorrect output:

```html
<p><a href="/uri">foo</a></p>

```

Example 546

This markdown:

```markdown
[bar\\]: /uri

[bar\\]

```

Gives this incorrect output:

```html
<p><a href="/uri">bar\</a></p>

```

Example 547

This markdown:

```markdown
[]

[]: /uri

```

Gives this incorrect output:

```html
<p>[]</p>
<p>[]: /uri</p>

```

Example 548

This markdown:

```markdown
[
 ]

[
 ]: /uri

```

Gives this incorrect output:

```html
<p>[
]</p>
<p>[
]: /uri</p>

```

Example 549

This markdown:

```markdown
[foo][]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 550

This markdown:

```markdown
[*foo* bar][]

[*foo* bar]: /url "title"

```

Gives this incorrect output:

```html
<p><a href="/url" title="title"><em>foo</em> bar</a></p>

```

Example 551

This markdown:

```markdown
[Foo][]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><a href="/url" title="title">Foo</a></p>

```

Example 552

This markdown:

```markdown
[foo] 
[]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><a href="/url" title="title">foo</a>
[]</p>

```

Example 553

This markdown:

```markdown
[foo]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 554

This markdown:

```markdown
[*foo* bar]

[*foo* bar]: /url "title"

```

Gives this incorrect output:

```html
<p><a href="/url" title="title"><em>foo</em> bar</a></p>

```

Example 555

This markdown:

```markdown
[[*foo* bar]]

[*foo* bar]: /url "title"

```

Gives this incorrect output:

```html
<p>[<a href="/url" title="title"><em>foo</em> bar</a>]</p>

```

Example 556

This markdown:

```markdown
[[bar [foo]

[foo]: /url

```

Gives this incorrect output:

```html
<p>[[bar <a href="/url">foo</a></p>

```

Example 557

This markdown:

```markdown
[Foo]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><a href="/url" title="title">Foo</a></p>

```

Example 558

This markdown:

```markdown
[foo] bar

[foo]: /url

```

Gives this incorrect output:

```html
<p><a href="/url">foo</a> bar</p>

```

Example 559

This markdown:

```markdown
\[foo]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p>[foo]</p>

```

Example 560

This markdown:

```markdown
[foo*]: /url

*[foo*]

```

Gives this incorrect output:

```html
<p>*<a href="/url">foo*</a></p>

```

Example 561

This markdown:

```markdown
[foo][bar]

[foo]: /url1
[bar]: /url2

```

Gives this incorrect output:

```html
<p><a href="/url2">foo</a></p>

```

Example 562

This markdown:

```markdown
[foo][]

[foo]: /url1

```

Gives this incorrect output:

```html
<p><a href="/url1">foo</a></p>

```

Example 563

This markdown:

```markdown
[foo]()

[foo]: /url1

```

Gives this incorrect output:

```html
<p><a href="">foo</a></p>

```

Example 564

This markdown:

```markdown
[foo](not a link)

[foo]: /url1

```

Gives this incorrect output:

```html
<p><a href="/url1">foo</a>(not a link)</p>

```

Example 565

This markdown:

```markdown
[foo][bar][baz]

[baz]: /url

```

Gives this incorrect output:

```html
<p>[foo]<a href="/url">bar</a></p>

```

Example 566

This markdown:

```markdown
[foo][bar][baz]

[baz]: /url1
[bar]: /url2

```

Gives this incorrect output:

```html
<p><a href="/url2">foo</a><a href="/url1">baz</a></p>

```

Example 567

This markdown:

```markdown
[foo][bar][baz]

[baz]: /url1
[foo]: /url2

```

Gives this incorrect output:

```html
<p>[foo]<a href="/url1">bar</a></p>

```

### Code spans

Example 329

This markdown:

```markdown
`` foo ` bar ``

```

Gives this incorrect output:

```html
<p><code>foo ` bar</code></p>

```

Example 330

This markdown:

```markdown
` `` `

```

Gives this incorrect output:

```html
<p><code>``</code></p>

```

Example 331

This markdown:

```markdown
`  ``  `

```

Gives this incorrect output:

```html
<p><code> `` </code></p>

```

Example 334

This markdown:

```markdown
` `
`  `

```

Gives this incorrect output:

```html
<p><code> </code>
<code>  </code></p>

```

Example 335

This markdown:

```markdown
``
foo
bar  
baz
``

```

Gives this incorrect output:

```html
<p><code>foo bar   baz</code></p>

```

Example 336

This markdown:

```markdown
``
foo 
``

```

Gives this incorrect output:

```html
<p><code>foo </code></p>

```

Example 337

This markdown:

```markdown
`foo   bar 
baz`

```

Gives this incorrect output:

```html
<p><code>foo   bar  baz</code></p>

```

Example 338

This markdown:

```markdown
`foo\`bar`

```

Gives this incorrect output:

```html
<p><code>foo\</code>bar`</p>

```

Example 339

This markdown:

```markdown
``foo`bar``

```

Gives this incorrect output:

```html
<p><code>foo`bar</code></p>

```

Example 340

This markdown:

```markdown
` foo `` bar `

```

Gives this incorrect output:

```html
<p><code>foo `` bar</code></p>

```

Example 341

This markdown:

```markdown
*foo`*`

```

Gives this incorrect output:

```html
<p>*foo<code>*</code></p>

```

Example 342

This markdown:

```markdown
[not a `link](/foo`)

```

Gives this incorrect output:

```html
<p>[not a <code>link](/foo</code>)</p>

```

Example 343

This markdown:

```markdown
`<a href="`">`

```

Gives this incorrect output:

```html
<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>

```

Example 344

This markdown:

```markdown
<a href="`">`

```

Gives this incorrect output:

```html
<p><a href="`">`</p>

```

Example 345

This markdown:

```markdown
`<http://foo.bar.`baz>`

```

Gives this incorrect output:

```html
<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>

```

Example 346

This markdown:

```markdown
<http://foo.bar.`baz>`

```

Gives this incorrect output:

```html
<p><a href="http://foo.bar.%60baz">http://foo.bar.`baz</a>`</p>

```

Example 347

This markdown:

```markdown
```foo``

```

Gives this incorrect output:

```html
<p>```foo``</p>

```

Example 348

This markdown:

```markdown
`foo

```

Gives this incorrect output:

```html
<p>`foo</p>

```

Example 349

This markdown:

```markdown
`foo``bar``

```

Gives this incorrect output:

```html
<p>`foo<code>bar</code></p>

```

### Link reference definitions

Example 161

This markdown:

```markdown
[foo]: /url "title"

[foo]

```

Gives this incorrect output:

```html
<p><a href="/url" title="title">foo</a></p>

```

Example 162

This markdown:

```markdown
   [foo]: 
      /url  
           'the title'  

[foo]

```

Gives this incorrect output:

```html
<p><a href="/url" title="the title">foo</a></p>

```

Example 163

This markdown:

```markdown
[Foo*bar\]]:my_(url) 'title (with parens)'

[Foo*bar\]]

```

Gives this incorrect output:

```html
<p><a href="my_(url)" title="title (with parens)">Foo*bar]</a></p>

```

Example 164

This markdown:

```markdown
[Foo bar]:
<my url>
'title'

[Foo bar]

```

Gives this incorrect output:

```html
<p><a href="my%20url" title="title">Foo bar</a></p>

```

Example 165

This markdown:

```markdown
[foo]: /url '
title
line1
line2
'

[foo]

```

Gives this incorrect output:

```html
<p><a href="/url" title="
title
line1
line2
">foo</a></p>

```

Example 166

This markdown:

```markdown
[foo]: /url 'title

with blank line'

[foo]

```

Gives this incorrect output:

```html
<p>[foo]: /url 'title</p>
<p>with blank line'</p>
<p>[foo]</p>

```

Example 167

This markdown:

```markdown
[foo]:
/url

[foo]

```

Gives this incorrect output:

```html
<p><a href="/url">foo</a></p>

```

Example 168

This markdown:

```markdown
[foo]:

[foo]

```

Gives this incorrect output:

```html
<p>[foo]:</p>
<p>[foo]</p>

```

Example 169

This markdown:

```markdown
[foo]: <>

[foo]

```

Gives this incorrect output:

```html
<p><a href="">foo</a></p>

```

Example 170

This markdown:

```markdown
[foo]: <bar>(baz)

[foo]

```

Gives this incorrect output:

```html
<p>[foo]: <bar>(baz)</p>
<p>[foo]</p>

```

Example 171

This markdown:

```markdown
[foo]: /url\bar\*baz "foo\"bar\baz"

[foo]

```

Gives this incorrect output:

```html
<p><a href="/url%5Cbar*baz" title="foo&quot;bar\baz">foo</a></p>

```

Example 172

This markdown:

```markdown
[foo]

[foo]: url

```

Gives this incorrect output:

```html
<p><a href="url">foo</a></p>

```

Example 173

This markdown:

```markdown
[foo]

[foo]: first
[foo]: second

```

Gives this incorrect output:

```html
<p><a href="first">foo</a></p>

```

Example 174

This markdown:

```markdown
[FOO]: /url

[Foo]

```

Gives this incorrect output:

```html
<p><a href="/url">Foo</a></p>

```

Example 175

This markdown:

```markdown
[ΑΓΩ]: /φου

[αγω]

```

Gives this incorrect output:

```html
<p><a href="/%CF%86%CE%BF%CF%85">αγω</a></p>

```

Example 176

This markdown:

```markdown
[foo]: /url

```

Gives this incorrect output:

```html

```

Example 177

This markdown:

```markdown
[
foo
]: /url
bar

```

Gives this incorrect output:

```html
<p>bar</p>

```

Example 178

This markdown:

```markdown
[foo]: /url "title" ok

```

Gives this incorrect output:

```html
<p>[foo]: /url &quot;title&quot; ok</p>

```

Example 179

This markdown:

```markdown
[foo]: /url
"title" ok

```

Gives this incorrect output:

```html
<p>&quot;title&quot; ok</p>

```

Example 180

This markdown:

```markdown
    [foo]: /url "title"

[foo]

```

Gives this incorrect output:

```html
<pre><code>[foo]: /url &quot;title&quot;
</code></pre>
<p>[foo]</p>

```

Example 181

This markdown:

```markdown
```
[foo]: /url
```

[foo]

```

Gives this incorrect output:

```html
<pre><code>[foo]: /url
</code></pre>
<p>[foo]</p>

```

Example 182

This markdown:

```markdown
Foo
[bar]: /baz

[bar]

```

Gives this incorrect output:

```html
<p>Foo
[bar]: /baz</p>
<p>[bar]</p>

```

Example 183

This markdown:

```markdown
# [Foo]
[foo]: /url
> bar

```

Gives this incorrect output:

```html
<h1><a href="/url">Foo</a></h1>
<blockquote>
<p>bar</p>
</blockquote>

```

Example 184

This markdown:

```markdown
[foo]: /url
bar
===
[foo]

```

Gives this incorrect output:

```html
<h1>bar</h1>
<p><a href="/url">foo</a></p>

```

Example 185

This markdown:

```markdown
[foo]: /url
===
[foo]

```

Gives this incorrect output:

```html
<p>===
<a href="/url">foo</a></p>

```

Example 186

This markdown:

```markdown
[foo]: /foo-url "foo"
[bar]: /bar-url
  "bar"
[baz]: /baz-url

[foo],
[bar],
[baz]

```

Gives this incorrect output:

```html
<p><a href="/foo-url" title="foo">foo</a>,
<a href="/bar-url" title="bar">bar</a>,
<a href="/baz-url">baz</a></p>

```

Example 187

This markdown:

```markdown
[foo]

> [foo]: /url

```

Gives this incorrect output:

```html
<p><a href="/url">foo</a></p>
<blockquote>
</blockquote>

```

Example 188

This markdown:

```markdown
[foo]: /url

```

Gives this incorrect output:

```html

```

### Raw HTML

Example 609

This markdown:

```markdown
<a><bab><c2c>

```

Gives this incorrect output:

```html
<p><a><bab><c2c></p>

```

Example 610

This markdown:

```markdown
<a/><b2/>

```

Gives this incorrect output:

```html
<p><a/><b2/></p>

```

Example 611

This markdown:

```markdown
<a  /><b2
data="foo" >

```

Gives this incorrect output:

```html
<p><a  /><b2
data="foo" ></p>

```

Example 612

This markdown:

```markdown
<a foo="bar" bam = 'baz <em>"</em>'
_boolean zoop:33=zoop:33 />

```

Gives this incorrect output:

```html
<p><a foo="bar" bam = 'baz <em>"</em>'
_boolean zoop:33=zoop:33 /></p>

```

Example 613

This markdown:

```markdown
Foo <responsive-image src="foo.jpg" />

```

Gives this incorrect output:

```html
<p>Foo <responsive-image src="foo.jpg" /></p>

```

Example 614

This markdown:

```markdown
<33> <__>

```

Gives this incorrect output:

```html
<p>&lt;33&gt; &lt;__&gt;</p>

```

Example 615

This markdown:

```markdown
<a h*#ref="hi">

```

Gives this incorrect output:

```html
<p>&lt;a h*#ref=&quot;hi&quot;&gt;</p>

```

Example 616

This markdown:

```markdown
<a href="hi'> <a href=hi'>

```

Gives this incorrect output:

```html
<p>&lt;a href=&quot;hi'&gt; &lt;a href=hi'&gt;</p>

```

Example 617

This markdown:

```markdown
< a><
foo><bar/ >
<foo bar=baz
bim!bop />

```

Gives this incorrect output:

```html
<p>&lt; a&gt;&lt;
foo&gt;&lt;bar/ &gt;
&lt;foo bar=baz
bim!bop /&gt;</p>

```

Example 618

This markdown:

```markdown
<a href='bar'title=title>

```

Gives this incorrect output:

```html
<p>&lt;a href='bar'title=title&gt;</p>

```

Example 619

This markdown:

```markdown
</a></foo >

```

Gives this incorrect output:

```html
<p></a></foo ></p>

```

Example 620

This markdown:

```markdown
</a href="foo">

```

Gives this incorrect output:

```html
<p>&lt;/a href=&quot;foo&quot;&gt;</p>

```

Example 621

This markdown:

```markdown
foo <!-- this is a
comment - with hyphen -->

```

Gives this incorrect output:

```html
<p>foo <!-- this is a
comment - with hyphen --></p>

```

Example 624

This markdown:

```markdown
foo <?php echo $a; ?>

```

Gives this incorrect output:

```html
<p>foo <?php echo $a; ?></p>

```

Example 625

This markdown:

```markdown
foo <!ELEMENT br EMPTY>

```

Gives this incorrect output:

```html
<p>foo <!ELEMENT br EMPTY></p>

```

Example 626

This markdown:

```markdown
foo <![CDATA[>&<]]>

```

Gives this incorrect output:

```html
<p>foo <![CDATA[>&<]]></p>

```

Example 627

This markdown:

```markdown
foo <a href="&ouml;">

```

Gives this incorrect output:

```html
<p>foo <a href="&ouml;"></p>

```

Example 628

This markdown:

```markdown
foo <a href="\*">

```

Gives this incorrect output:

```html
<p>foo <a href="\*"></p>

```

Example 629

This markdown:

```markdown
<a href="\"">

```

Gives this incorrect output:

```html
<p>&lt;a href=&quot;&quot;&quot;&gt;</p>

```

### Images

Example 568

This markdown:

```markdown
![foo](/url "title")

```

Gives this incorrect output:

```html
<p><img src="/url" alt="foo" title="title" /></p>

```

Example 569

This markdown:

```markdown
![foo *bar*]

[foo *bar*]: train.jpg "train & tracks"

```

Gives this incorrect output:

```html
<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>

```

Example 570

This markdown:

```markdown
![foo ![bar](/url)](/url2)

```

Gives this incorrect output:

```html
<p><img src="/url2" alt="foo bar" /></p>

```

Example 571

This markdown:

```markdown
![foo [bar](/url)](/url2)

```

Gives this incorrect output:

```html
<p><img src="/url2" alt="foo bar" /></p>

```

Example 572

This markdown:

```markdown
![foo *bar*][]

[foo *bar*]: train.jpg "train & tracks"

```

Gives this incorrect output:

```html
<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>

```

Example 573

This markdown:

```markdown
![foo *bar*][foobar]

[FOOBAR]: train.jpg "train & tracks"

```

Gives this incorrect output:

```html
<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>

```

Example 574

This markdown:

```markdown
![foo](train.jpg)

```

Gives this incorrect output:

```html
<p><img src="train.jpg" alt="foo" /></p>

```

Example 575

This markdown:

```markdown
My ![foo bar](/path/to/train.jpg  "title"   )

```

Gives this incorrect output:

```html
<p>My <img src="/path/to/train.jpg" alt="foo bar" title="title" /></p>

```

Example 576

This markdown:

```markdown
![foo](<url>)

```

Gives this incorrect output:

```html
<p><img src="url" alt="foo" /></p>

```

Example 577

This markdown:

```markdown
![](/url)

```

Gives this incorrect output:

```html
<p><img src="/url" alt="" /></p>

```

Example 578

This markdown:

```markdown
![foo][bar]

[bar]: /url

```

Gives this incorrect output:

```html
<p><img src="/url" alt="foo" /></p>

```

Example 579

This markdown:

```markdown
![foo][bar]

[BAR]: /url

```

Gives this incorrect output:

```html
<p><img src="/url" alt="foo" /></p>

```

Example 580

This markdown:

```markdown
![foo][]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><img src="/url" alt="foo" title="title" /></p>

```

Example 581

This markdown:

```markdown
![*foo* bar][]

[*foo* bar]: /url "title"

```

Gives this incorrect output:

```html
<p><img src="/url" alt="foo bar" title="title" /></p>

```

Example 582

This markdown:

```markdown
![Foo][]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><img src="/url" alt="Foo" title="title" /></p>

```

Example 583

This markdown:

```markdown
![foo] 
[]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><img src="/url" alt="foo" title="title" />
[]</p>

```

Example 584

This markdown:

```markdown
![foo]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><img src="/url" alt="foo" title="title" /></p>

```

Example 585

This markdown:

```markdown
![*foo* bar]

[*foo* bar]: /url "title"

```

Gives this incorrect output:

```html
<p><img src="/url" alt="foo bar" title="title" /></p>

```

Example 586

This markdown:

```markdown
![[foo]]

[[foo]]: /url "title"

```

Gives this incorrect output:

```html
<p>![[foo]]</p>
<p>[[foo]]: /url &quot;title&quot;</p>

```

Example 587

This markdown:

```markdown
![Foo]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p><img src="/url" alt="Foo" title="title" /></p>

```

Example 588

This markdown:

```markdown
!\[foo]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p>![foo]</p>

```

Example 589

This markdown:

```markdown
\![foo]

[foo]: /url "title"

```

Gives this incorrect output:

```html
<p>!<a href="/url" title="title">foo</a></p>

```

### Entity and numeric character references

Example 311

This markdown:

```markdown
&nbsp; &amp; &copy; &AElig; &Dcaron;
&frac34; &HilbertSpace; &DifferentialD;
&ClockwiseContourIntegral; &ngE;

```

Gives this incorrect output:

```html
<p>  &amp; © Æ Ď
¾ ℋ ⅆ
∲ ≧̸</p>

```

Example 312

This markdown:

```markdown
&#35; &#1234; &#992; &#0;

```

Gives this incorrect output:

```html
<p># Ӓ Ϡ �</p>

```

Example 313

This markdown:

```markdown
&#X22; &#XD06; &#xcab;

```

Gives this incorrect output:

```html
<p>&quot; ആ ಫ</p>

```

Example 314

This markdown:

```markdown
&nbsp &x; &#; &#x;
&#987654321;
&#abcdef0;
&ThisIsNotDefined; &hi?;

```

Gives this incorrect output:

```html
<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;
&amp;#987654321;
&amp;#abcdef0;
&amp;ThisIsNotDefined; &amp;hi?;</p>

```

Example 317

This markdown:

```markdown
<a href="&ouml;&ouml;.html">

```

Gives this incorrect output:

```html
<a href="&ouml;&ouml;.html">

```

Example 318

This markdown:

```markdown
[foo](/f&ouml;&ouml; "f&ouml;&ouml;")

```

Gives this incorrect output:

```html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>

```

Example 319

This markdown:

```markdown
[foo]

[foo]: /f&ouml;&ouml; "f&ouml;&ouml;"

```

Gives this incorrect output:

```html
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>

```

Example 320

This markdown:

```markdown
``` f&ouml;&ouml;
foo
```

```

Gives this incorrect output:

```html
<pre><code class="language-föö">foo
</code></pre>

```

Example 323

This markdown:

```markdown
&#42;foo&#42;
*foo*

```

Gives this incorrect output:

```html
<p>*foo*
<em>foo</em></p>

```

Example 324

This markdown:

```markdown
&#42; foo

* foo

```

Gives this incorrect output:

```html
<p>* foo</p>
<ul>
<li>foo</li>
</ul>

```

Example 325

This markdown:

```markdown
foo&#10;&#10;bar

```

Gives this incorrect output:

```html
<p>foo

bar</p>

```

Example 326

This markdown:

```markdown
&#9;foo

```

Gives this incorrect output:

```html
<p>	foo</p>

```

Example 327

This markdown:

```markdown
[a](url &quot;tit&quot;)

```

Gives this incorrect output:

```html
<p>[a](url &quot;tit&quot;)</p>

```

### Fenced code blocks

Example 91

This markdown:

```markdown
``
foo
``

```

Gives this incorrect output:

```html
<p><code>foo</code></p>

```

Example 94

This markdown:

```markdown
````
aaa
```
``````

```

Gives this incorrect output:

```html
<pre><code>aaa
```
</code></pre>

```

Example 95

This markdown:

```markdown
~~~~
aaa
~~~
~~~~

```

Gives this incorrect output:

```html
<pre><code>aaa
~~~
</code></pre>

```

Example 96

This markdown:

```markdown
```

```

Gives this incorrect output:

```html
<pre><code></code></pre>

```

Example 97

This markdown:

```markdown
`````

```
aaa

```

Gives this incorrect output:

```html
<pre><code>
```
aaa
</code></pre>

```

Example 98

This markdown:

```markdown
> ```
> aaa

bbb

```

Gives this incorrect output:

```html
<blockquote>
<pre><code>aaa
</code></pre>
</blockquote>
<p>bbb</p>

```

Example 100

This markdown:

```markdown
```
```

```

Gives this incorrect output:

```html
<pre><code></code></pre>

```

Example 101

This markdown:

```markdown
 ```
 aaa
aaa
```

```

Gives this incorrect output:

```html
<pre><code>aaa
aaa
</code></pre>

```

Example 102

This markdown:

```markdown
  ```
aaa
  aaa
aaa
  ```

```

Gives this incorrect output:

```html
<pre><code>aaa
aaa
aaa
</code></pre>

```

Example 103

This markdown:

```markdown
   ```
   aaa
    aaa
  aaa
   ```

```

Gives this incorrect output:

```html
<pre><code>aaa
 aaa
aaa
</code></pre>

```

Example 104

This markdown:

```markdown
    ```
    aaa
    ```

```

Gives this incorrect output:

```html
<pre><code>```
aaa
```
</code></pre>

```

Example 105

This markdown:

```markdown
```
aaa
  ```

```

Gives this incorrect output:

```html
<pre><code>aaa
</code></pre>

```

Example 106

This markdown:

```markdown
   ```
aaa
  ```

```

Gives this incorrect output:

```html
<pre><code>aaa
</code></pre>

```

Example 107

This markdown:

```markdown
```
aaa
    ```

```

Gives this incorrect output:

```html
<pre><code>aaa
    ```
</code></pre>

```

Example 108

This markdown:

```markdown
``` ```
aaa

```

Gives this incorrect output:

```html
<p><code> </code>
aaa</p>

```

Example 109

This markdown:

```markdown
~~~~~~
aaa
~~~ ~~

```

Gives this incorrect output:

```html
<pre><code>aaa
~~~ ~~
</code></pre>

```

Example 111

This markdown:

```markdown
foo
---
~~~
bar
~~~
# baz

```

Gives this incorrect output:

```html
<h2>foo</h2>
<pre><code>bar
</code></pre>
<h1>baz</h1>

```

Example 112

This markdown:

```markdown
```ruby
def foo(x)
  return 3
end
```

```

Gives this incorrect output:

```html
<pre><code class="language-ruby">def foo(x)
  return 3
end
</code></pre>

```

Example 113

This markdown:

```markdown
~~~~    ruby startline=3 $%@#$
def foo(x)
  return 3
end
~~~~~~~

```

Gives this incorrect output:

```html
<pre><code class="language-ruby">def foo(x)
  return 3
end
</code></pre>

```

Example 114

This markdown:

```markdown
````;
````

```

Gives this incorrect output:

```html
<pre><code class="language-;"></code></pre>

```

Example 115

This markdown:

```markdown
``` aa ```
foo

```

Gives this incorrect output:

```html
<p><code>aa</code>
foo</p>

```

Example 116

This markdown:

```markdown
~~~ aa ``` ~~~
foo
~~~

```

Gives this incorrect output:

```html
<pre><code class="language-aa">foo
</code></pre>

```

### Hard line breaks

Example 630

This markdown:

```markdown
foo  
baz

```

Gives this incorrect output:

```html
<p>foo<br />
baz</p>

```

Example 631

This markdown:

```markdown
foo\
baz

```

Gives this incorrect output:

```html
<p>foo<br />
baz</p>

```

Example 632

This markdown:

```markdown
foo       
baz

```

Gives this incorrect output:

```html
<p>foo<br />
baz</p>

```

Example 633

This markdown:

```markdown
foo  
     bar

```

Gives this incorrect output:

```html
<p>foo<br />
bar</p>

```

Example 634

This markdown:

```markdown
foo\
     bar

```

Gives this incorrect output:

```html
<p>foo<br />
bar</p>

```

Example 635

This markdown:

```markdown
*foo  
bar*

```

Gives this incorrect output:

```html
<p><em>foo<br />
bar</em></p>

```

Example 636

This markdown:

```markdown
*foo\
bar*

```

Gives this incorrect output:

```html
<p><em>foo<br />
bar</em></p>

```

Example 637

This markdown:

```markdown
`code 
span`

```

Gives this incorrect output:

```html
<p><code>code  span</code></p>

```

Example 638

This markdown:

```markdown
`code\
span`

```

Gives this incorrect output:

```html
<p><code>code\ span</code></p>

```

Example 639

This markdown:

```markdown
<a href="foo  
bar">

```

Gives this incorrect output:

```html
<p><a href="foo  
bar"></p>

```

Example 640

This markdown:

```markdown
<a href="foo\
bar">

```

Gives this incorrect output:

```html
<p><a href="foo\
bar"></p>

```

### Setext headings

Example 50

This markdown:

```markdown
Foo *bar*
=========

Foo *bar*
---------

```

Gives this incorrect output:

```html
<h1>Foo <em>bar</em></h1>
<h2>Foo <em>bar</em></h2>

```

Example 51

This markdown:

```markdown
Foo *bar
baz*
====

```

Gives this incorrect output:

```html
<h1>Foo <em>bar
baz</em></h1>

```

Example 52

This markdown:

```markdown
  Foo *bar
baz*	
====

```

Gives this incorrect output:

```html
<h1>Foo <em>bar
baz</em></h1>

```

Example 53

This markdown:

```markdown
Foo
-------------------------

Foo
=

```

Gives this incorrect output:

```html
<h2>Foo</h2>
<h1>Foo</h1>

```

Example 54

This markdown:

```markdown
   Foo
---

  Foo
-----

  Foo
  ===

```

Gives this incorrect output:

```html
<h2>Foo</h2>
<h2>Foo</h2>
<h1>Foo</h1>

```

Example 55

This markdown:

```markdown
    Foo
    ---

    Foo
---

```

Gives this incorrect output:

```html
<pre><code>Foo
---

Foo
</code></pre>
<hr />

```

Example 56

This markdown:

```markdown
Foo
   ----      

```

Gives this incorrect output:

```html
<h2>Foo</h2>

```

Example 57

This markdown:

```markdown
Foo
    ---

```

Gives this incorrect output:

```html
<p>Foo
---</p>

```

Example 58

This markdown:

```markdown
Foo
= =

Foo
--- -

```

Gives this incorrect output:

```html
<p>Foo
= =</p>
<p>Foo</p>
<hr />

```

Example 59

This markdown:

```markdown
Foo  
-----

```

Gives this incorrect output:

```html
<h2>Foo</h2>

```

Example 60

This markdown:

```markdown
Foo\
----

```

Gives this incorrect output:

```html
<h2>Foo\</h2>

```

Example 61

This markdown:

```markdown
`Foo
----
`

<a title="a lot
---
of dashes"/>

```

Gives this incorrect output:

```html
<h2>`Foo</h2>
<p>`</p>
<h2>&lt;a title=&quot;a lot</h2>
<p>of dashes&quot;/&gt;</p>

```

Example 62

This markdown:

```markdown
> Foo
---

```

Gives this incorrect output:

```html
<blockquote>
<p>Foo</p>
</blockquote>
<hr />

```

Example 63

This markdown:

```markdown
> foo
bar
===

```

Gives this incorrect output:

```html
<blockquote>
<p>foo
bar
===</p>
</blockquote>

```

Example 64

This markdown:

```markdown
- Foo
---

```

Gives this incorrect output:

```html
<ul>
<li>Foo</li>
</ul>
<hr />

```

Example 65

This markdown:

```markdown
Foo
Bar
---

```

Gives this incorrect output:

```html
<h2>Foo
Bar</h2>

```

Example 66

This markdown:

```markdown
---
Foo
---
Bar
---
Baz

```

Gives this incorrect output:

```html
<hr />
<h2>Foo</h2>
<h2>Bar</h2>
<p>Baz</p>

```

Example 69

This markdown:

```markdown
- foo
-----

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
</ul>
<hr />

```

Example 71

This markdown:

```markdown
> foo
-----

```

Gives this incorrect output:

```html
<blockquote>
<p>foo</p>
</blockquote>
<hr />

```

Example 72

This markdown:

```markdown
\> foo
------

```

Gives this incorrect output:

```html
<h2>&gt; foo</h2>

```

Example 73

This markdown:

```markdown
Foo

bar
---
baz

```

Gives this incorrect output:

```html
<p>Foo</p>
<h2>bar</h2>
<p>baz</p>

```

Example 74

This markdown:

```markdown
Foo
bar

---

baz

```

Gives this incorrect output:

```html
<p>Foo
bar</p>
<hr />
<p>baz</p>

```

Example 75

This markdown:

```markdown
Foo
bar
* * *
baz

```

Gives this incorrect output:

```html
<p>Foo
bar</p>
<hr />
<p>baz</p>

```

Example 76

This markdown:

```markdown
Foo
bar
\---
baz

```

Gives this incorrect output:

```html
<p>Foo
bar
---
baz</p>

```

### Backslash escapes

Example 298

This markdown:

```markdown
\!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~

```

Gives this incorrect output:

```html
<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>

```

Example 300

This markdown:

```markdown
\*not emphasized*
\<br/> not a tag
\[not a link](/foo)
\`not code`
1\. not a list
\* not a list
\# not a heading
\[foo]: /url "not a reference"
\&ouml; not a character entity

```

Gives this incorrect output:

```html
<p>*not emphasized*
&lt;br/&gt; not a tag
[not a link](/foo)
`not code`
1. not a list
* not a list
# not a heading
[foo]: /url &quot;not a reference&quot;
&amp;ouml; not a character entity</p>

```

Example 301

This markdown:

```markdown
\\*emphasis*

```

Gives this incorrect output:

```html
<p>\<em>emphasis</em></p>

```

Example 302

This markdown:

```markdown
foo\
bar

```

Gives this incorrect output:

```html
<p>foo<br />
bar</p>

```

Example 303

This markdown:

```markdown
`` \[\` ``

```

Gives this incorrect output:

```html
<p><code>\[\`</code></p>

```

Example 306

This markdown:

```markdown
<http://example.com?find=\*>

```

Gives this incorrect output:

```html
<p><a href="http://example.com?find=%5C*">http://example.com?find=\*</a></p>

```

Example 307

This markdown:

```markdown
<a href="/bar\/)">

```

Gives this incorrect output:

```html
<a href="/bar\/)">

```

Example 308

This markdown:

```markdown
[foo](/bar\* "ti\*tle")

```

Gives this incorrect output:

```html
<p><a href="/bar*" title="ti*tle">foo</a></p>

```

Example 309

This markdown:

```markdown
[foo]

[foo]: /bar\* "ti\*tle"

```

Gives this incorrect output:

```html
<p><a href="/bar*" title="ti*tle">foo</a></p>

```

Example 310

This markdown:

```markdown
``` foo\+bar
foo
```

```

Gives this incorrect output:

```html
<pre><code class="language-foo+bar">foo
</code></pre>

```

### Autolinks

Example 590

This markdown:

```markdown
<http://foo.bar.baz>

```

Gives this incorrect output:

```html
<p><a href="http://foo.bar.baz">http://foo.bar.baz</a></p>

```

Example 591

This markdown:

```markdown
<http://foo.bar.baz/test?q=hello&id=22&boolean>

```

Gives this incorrect output:

```html
<p><a href="http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>

```

Example 592

This markdown:

```markdown
<irc://foo.bar:2233/baz>

```

Gives this incorrect output:

```html
<p><a href="irc://foo.bar:2233/baz">irc://foo.bar:2233/baz</a></p>

```

Example 593

This markdown:

```markdown
<MAILTO:FOO@BAR.BAZ>

```

Gives this incorrect output:

```html
<p><a href="MAILTO:FOO@BAR.BAZ">MAILTO:FOO@BAR.BAZ</a></p>

```

Example 594

This markdown:

```markdown
<a+b+c:d>

```

Gives this incorrect output:

```html
<p><a href="a+b+c:d">a+b+c:d</a></p>

```

Example 595

This markdown:

```markdown
<made-up-scheme://foo,bar>

```

Gives this incorrect output:

```html
<p><a href="made-up-scheme://foo,bar">made-up-scheme://foo,bar</a></p>

```

Example 596

This markdown:

```markdown
<http://../>

```

Gives this incorrect output:

```html
<p><a href="http://../">http://../</a></p>

```

Example 597

This markdown:

```markdown
<localhost:5001/foo>

```

Gives this incorrect output:

```html
<p><a href="localhost:5001/foo">localhost:5001/foo</a></p>

```

Example 598

This markdown:

```markdown
<http://foo.bar/baz bim>

```

Gives this incorrect output:

```html
<p>&lt;http://foo.bar/baz bim&gt;</p>

```

Example 599

This markdown:

```markdown
<http://example.com/\[\>

```

Gives this incorrect output:

```html
<p><a href="http://example.com/%5C%5B%5C">http://example.com/\[\</a></p>

```

Example 600

This markdown:

```markdown
<foo@bar.example.com>

```

Gives this incorrect output:

```html
<p><a href="mailto:foo@bar.example.com">foo@bar.example.com</a></p>

```

Example 601

This markdown:

```markdown
<foo+special@Bar.baz-bar0.com>

```

Gives this incorrect output:

```html
<p><a href="mailto:foo+special@Bar.baz-bar0.com">foo+special@Bar.baz-bar0.com</a></p>

```

Example 602

This markdown:

```markdown
<foo\+@bar.example.com>

```

Gives this incorrect output:

```html
<p>&lt;foo+@bar.example.com&gt;</p>

```

Example 603

This markdown:

```markdown
<>

```

Gives this incorrect output:

```html
<p>&lt;&gt;</p>

```

Example 604

This markdown:

```markdown
< http://foo.bar >

```

Gives this incorrect output:

```html
<p>&lt; http://foo.bar &gt;</p>

```

Example 605

This markdown:

```markdown
<m:abc>

```

Gives this incorrect output:

```html
<p>&lt;m:abc&gt;</p>

```

Example 606

This markdown:

```markdown
<foo.bar.baz>

```

Gives this incorrect output:

```html
<p>&lt;foo.bar.baz&gt;</p>

```

### Soft line breaks

Example 645

This markdown:

```markdown
foo
baz

```

Gives this incorrect output:

```html
<p>foo
baz</p>

```

Example 646

This markdown:

```markdown
foo 
 baz

```

Gives this incorrect output:

```html
<p>foo
baz</p>

```

### ATX headings

Example 33

This markdown:

```markdown
####### foo

```

Gives this incorrect output:

```html
<p>####### foo</p>

```

Example 34

This markdown:

```markdown
#5 bolt

#hashtag

```

Gives this incorrect output:

```html
<p>#5 bolt</p>
<p>#hashtag</p>

```

Example 35

This markdown:

```markdown
\## foo

```

Gives this incorrect output:

```html
<p>## foo</p>

```

Example 36

This markdown:

```markdown
# foo *bar* \*baz\*

```

Gives this incorrect output:

```html
<h1>foo <em>bar</em> *baz*</h1>

```

Example 38

This markdown:

```markdown
 ### foo
  ## foo
   # foo

```

Gives this incorrect output:

```html
<h3>foo</h3>
<h2>foo</h2>
<h1>foo</h1>

```

Example 40

This markdown:

```markdown
foo
    # bar

```

Gives this incorrect output:

```html
<p>foo
# bar</p>

```

Example 41

This markdown:

```markdown
## foo ##
  ###   bar    ###

```

Gives this incorrect output:

```html
<h2>foo</h2>
<h3>bar</h3>

```

Example 43

This markdown:

```markdown
### foo ###     

```

Gives this incorrect output:

```html
<h3>foo</h3>

```

Example 45

This markdown:

```markdown
# foo#

```

Gives this incorrect output:

```html
<h1>foo#</h1>

```

Example 46

This markdown:

```markdown
### foo \###
## foo #\##
# foo \#

```

Gives this incorrect output:

```html
<h3>foo ###</h3>
<h2>foo ###</h2>
<h1>foo #</h1>

```

### Indented code blocks

Example 77

This markdown:

```markdown
    a simple
      indented code block

```

Gives this incorrect output:

```html
<pre><code>a simple
  indented code block
</code></pre>

```

Example 78

This markdown:

```markdown
  - foo

    bar

```

Gives this incorrect output:

```html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

```

Example 79

This markdown:

```markdown
1.  foo

    - bar

```

Gives this incorrect output:

```html
<ol>
<li>
<p>foo</p>
<ul>
<li>bar</li>
</ul>
</li>
</ol>

```

Example 80

This markdown:

```markdown
    <a/>
    *hi*

    - one

```

Gives this incorrect output:

```html
<pre><code>&lt;a/&gt;
*hi*

- one
</code></pre>

```

Example 81

This markdown:

```markdown
    chunk1

    chunk2
  
 
 
    chunk3

```

Gives this incorrect output:

```html
<pre><code>chunk1

chunk2



chunk3
</code></pre>

```

Example 82

This markdown:

```markdown
    chunk1
      
      chunk2

```

Gives this incorrect output:

```html
<pre><code>chunk1
  
  chunk2
</code></pre>

```

Example 83

This markdown:

```markdown
Foo
    bar


```

Gives this incorrect output:

```html
<p>Foo
bar</p>

```

Example 85

This markdown:

```markdown
# Heading
    foo
Heading
------
    foo
----

```

Gives this incorrect output:

```html
<h1>Heading</h1>
<pre><code>foo
</code></pre>
<h2>Heading</h2>
<pre><code>foo
</code></pre>
<hr />

```

Example 86

This markdown:

```markdown
        foo
    bar

```

Gives this incorrect output:

```html
<pre><code>    foo
bar
</code></pre>

```

Example 87

This markdown:

```markdown

    
    foo
    


```

Gives this incorrect output:

```html
<pre><code>foo
</code></pre>

```

### Lists

Example 271

This markdown:

```markdown
- foo
- bar
+ baz

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
<li>bar</li>
</ul>
<ul>
<li>baz</li>
</ul>

```

Example 272

This markdown:

```markdown
1. foo
2. bar
3) baz

```

Gives this incorrect output:

```html
<ol>
<li>foo</li>
<li>bar</li>
</ol>
<ol start="3">
<li>baz</li>
</ol>

```

Example 273

This markdown:

```markdown
Foo
- bar
- baz

```

Gives this incorrect output:

```html
<p>Foo</p>
<ul>
<li>bar</li>
<li>baz</li>
</ul>

```

Example 274

This markdown:

```markdown
The number of windows in my house is
14.  The number of doors is 6.

```

Gives this incorrect output:

```html
<p>The number of windows in my house is
14.  The number of doors is 6.</p>

```

Example 275

This markdown:

```markdown
The number of windows in my house is
1.  The number of doors is 6.

```

Gives this incorrect output:

```html
<p>The number of windows in my house is</p>
<ol>
<li>The number of doors is 6.</li>
</ol>

```

Example 276

This markdown:

```markdown
- foo

- bar


- baz

```

Gives this incorrect output:

```html
<ul>
<li>
<p>foo</p>
</li>
<li>
<p>bar</p>
</li>
<li>
<p>baz</p>
</li>
</ul>

```

Example 277

This markdown:

```markdown
- foo
  - bar
    - baz


      bim

```

Gives this incorrect output:

```html
<ul>
<li>foo
<ul>
<li>bar
<ul>
<li>
<p>baz</p>
<p>bim</p>
</li>
</ul>
</li>
</ul>
</li>
</ul>

```

Example 278

This markdown:

```markdown
- foo
- bar

<!-- -->

- baz
- bim

```

Gives this incorrect output:

```html
<ul>
<li>foo</li>
<li>bar</li>
</ul>
<!-- -->
<ul>
<li>baz</li>
<li>bim</li>
</ul>

```

Example 279

This markdown:

```markdown
-   foo

    notcode

-   foo

<!-- -->

    code

```

Gives this incorrect output:

```html
<ul>
<li>
<p>foo</p>
<p>notcode</p>
</li>
<li>
<p>foo</p>
</li>
</ul>
<!-- -->
<pre><code>code
</code></pre>

```

Example 280

This markdown:

```markdown
- a
 - b
  - c
   - d
  - e
 - f
- g

```

Gives this incorrect output:

```html
<ul>
<li>a</li>
<li>b</li>
<li>c</li>
<li>d</li>
<li>e</li>
<li>f</li>
<li>g</li>
</ul>

```

Example 281

This markdown:

```markdown
1. a

  2. b

   3. c

```

Gives this incorrect output:

```html
<ol>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>c</p>
</li>
</ol>

```

Example 282

This markdown:

```markdown
- a
 - b
  - c
   - d
    - e

```

Gives this incorrect output:

```html
<ul>
<li>a</li>
<li>b</li>
<li>c</li>
<li>d
- e</li>
</ul>

```

Example 283

This markdown:

```markdown
1. a

  2. b

    3. c

```

Gives this incorrect output:

```html
<ol>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
</ol>
<pre><code>3. c
</code></pre>

```

Example 284

This markdown:

```markdown
- a
- b

- c

```

Gives this incorrect output:

```html
<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>c</p>
</li>
</ul>

```

Example 285

This markdown:

```markdown
* a
*

* c

```

Gives this incorrect output:

```html
<ul>
<li>
<p>a</p>
</li>
<li></li>
<li>
<p>c</p>
</li>
</ul>

```

Example 286

This markdown:

```markdown
- a
- b

  c
- d

```

Gives this incorrect output:

```html
<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
<p>c</p>
</li>
<li>
<p>d</p>
</li>
</ul>

```

Example 287

This markdown:

```markdown
- a
- b

  [ref]: /url
- d

```

Gives this incorrect output:

```html
<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>d</p>
</li>
</ul>

```

Example 288

This markdown:

```markdown
- a
- ```
  b


  ```
- c

```

Gives this incorrect output:

```html
<ul>
<li>a</li>
<li>
<pre><code>b


</code></pre>
</li>
<li>c</li>
</ul>

```

Example 289

This markdown:

```markdown
- a
  - b

    c
- d

```

Gives this incorrect output:

```html
<ul>
<li>a
<ul>
<li>
<p>b</p>
<p>c</p>
</li>
</ul>
</li>
<li>d</li>
</ul>

```

Example 290

This markdown:

```markdown
* a
  > b
  >
* c

```

Gives this incorrect output:

```html
<ul>
<li>a
<blockquote>
<p>b</p>
</blockquote>
</li>
<li>c</li>
</ul>

```

Example 291

This markdown:

```markdown
- a
  > b
  ```
  c
  ```
- d

```

Gives this incorrect output:

```html
<ul>
<li>a
<blockquote>
<p>b</p>
</blockquote>
<pre><code>c
</code></pre>
</li>
<li>d</li>
</ul>

```

Example 292

This markdown:

```markdown
- a

```

Gives this incorrect output:

```html
<ul>
<li>a</li>
</ul>

```

Example 293

This markdown:

```markdown
- a
  - b

```

Gives this incorrect output:

```html
<ul>
<li>a
<ul>
<li>b</li>
</ul>
</li>
</ul>

```

Example 294

This markdown:

```markdown
1. ```
   foo
   ```

   bar

```

Gives this incorrect output:

```html
<ol>
<li>
<pre><code>foo
</code></pre>
<p>bar</p>
</li>
</ol>

```

Example 295

This markdown:

```markdown
* foo
  * bar

  baz

```

Gives this incorrect output:

```html
<ul>
<li>
<p>foo</p>
<ul>
<li>bar</li>
</ul>
<p>baz</p>
</li>
</ul>

```

Example 296

This markdown:

```markdown
- a
  - b
  - c

- d
  - e
  - f

```

Gives this incorrect output:

```html
<ul>
<li>
<p>a</p>
<ul>
<li>b</li>
<li>c</li>
</ul>
</li>
<li>
<p>d</p>
<ul>
<li>e</li>
<li>f</li>
</ul>
</li>
</ul>

```

### Tabs

Example 2

This markdown:

```markdown
  	foo	baz		bim

```

Gives this incorrect output:

```html
<pre><code>foo	baz		bim
</code></pre>

```

Example 3

This markdown:

```markdown
    a	a
    ὐ	a

```

Gives this incorrect output:

```html
<pre><code>a	a
ὐ	a
</code></pre>

```

Example 4

This markdown:

```markdown
  - foo

	bar

```

Gives this incorrect output:

```html
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>

```

Example 5

This markdown:

```markdown
- foo

		bar

```

Gives this incorrect output:

```html
<ul>
<li>
<p>foo</p>
<pre><code>  bar
</code></pre>
</li>
</ul>

```

Example 6

This markdown:

```markdown
>		foo

```

Gives this incorrect output:

```html
<blockquote>
<pre><code>  foo
</code></pre>
</blockquote>

```

Example 7

This markdown:

```markdown
-		foo

```

Gives this incorrect output:

```html
<ul>
<li>
<pre><code>  foo
</code></pre>
</li>
</ul>

```

Example 8

This markdown:

```markdown
    foo
	bar

```

Gives this incorrect output:

```html
<pre><code>foo
bar
</code></pre>

```

Example 9

This markdown:

```markdown
 - foo
   - bar
	 - baz

```

Gives this incorrect output:

```html
<ul>
<li>foo
<ul>
<li>bar
<ul>
<li>baz</li>
</ul>
</li>
</ul>
</li>
</ul>

```

Example 11

This markdown:

```markdown
*	*	*	

```

Gives this incorrect output:

```html
<hr />

```

### Inlines

Example 297

This markdown:

```markdown
`hi`lo`

```

Gives this incorrect output:

```html
<p><code>hi</code>lo`</p>

```


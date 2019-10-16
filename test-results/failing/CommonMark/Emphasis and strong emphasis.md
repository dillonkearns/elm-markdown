# CommonMark - Emphasis and strong emphasis

## [Example 350](https://spec.commonmark.org/0.29/#example-350)

This markdown:

```markdown
*foo bar*

```

Should give output:

```html
<p><em>foo bar</em></p>
```

But instead was:

```html
<ul><li><p>foo bar</p></li></ul>
```
## [Example 351](https://spec.commonmark.org/0.29/#example-351)

This markdown:

```markdown
a * foo bar*

```

Should give output:

```html
<p>a * foo bar*</p>
```

But instead was:

```html
<p>a<em>foo bar</em></p>
```
## [Example 352](https://spec.commonmark.org/0.29/#example-352)

This markdown:

```markdown
a*"foo"*

```

Should give output:

```html
<p>a*&quot;foo&quot;*</p>
```

But instead was:

```html
<p>a<em>&quot;foo&quot;</em></p>
```
## [Example 353](https://spec.commonmark.org/0.29/#example-353)

This markdown:

```markdown
* a *

```

Should give output:

```html
<p>* a *</p>
```

But instead was:

```html
<ul><li><p>a</p></li></ul>
```
## [Example 356](https://spec.commonmark.org/0.29/#example-356)

This markdown:

```markdown
_foo bar_

```

Should give output:

```html
<p><em>foo bar</em></p>
```

But instead was:

```html
<p>_foo bar_</p>
```
## [Example 363](https://spec.commonmark.org/0.29/#example-363)

This markdown:

```markdown
foo-_(bar)_

```

Should give output:

```html
<p>foo-<em>(bar)</em></p>
```

But instead was:

```html
<p>foo-_(bar)_</p>
```
## [Example 364](https://spec.commonmark.org/0.29/#example-364)

This markdown:

```markdown
_foo*

```

Should give output:

```html
<p>_foo*</p>
```

But instead was:

```html
<p>_foo</p>
```
## [Example 365](https://spec.commonmark.org/0.29/#example-365)

This markdown:

```markdown
*foo bar *

```

Should give output:

```html
<p>*foo bar *</p>
```

But instead was:

```html
<ul><li><p>foo bar</p></li></ul>
```
## [Example 366](https://spec.commonmark.org/0.29/#example-366)

This markdown:

```markdown
*foo bar
*

```

Should give output:

```html
<p>*foo bar *</p>
```

But instead was:

```html
<ul><li><p>foo bar</p></li><li><p></p></li></ul>
```
## [Example 367](https://spec.commonmark.org/0.29/#example-367)

This markdown:

```markdown
*(*foo)

```

Should give output:

```html
<p>*(*foo)</p>
```

But instead was:

```html
<ul><li><p>(<em>foo)</em></p></li></ul>
```
## [Example 368](https://spec.commonmark.org/0.29/#example-368)

This markdown:

```markdown
*(*foo*)*

```

Should give output:

```html
<p><em>(<em>foo</em>)</em></p>
```

But instead was:

```html
<ul><li><p>(<em>foo</em>)</p></li></ul>
```
## [Example 369](https://spec.commonmark.org/0.29/#example-369)

This markdown:

```markdown
*foo*bar

```

Should give output:

```html
<p><em>foo</em>bar</p>
```

But instead was:

```html
<ul><li><p>foo<em>bar</em></p></li></ul>
```
## [Example 372](https://spec.commonmark.org/0.29/#example-372)

This markdown:

```markdown
_(_foo_)_

```

Should give output:

```html
<p><em>(<em>foo</em>)</em></p>
```

But instead was:

```html
<p>_(_foo_)_</p>
```
## [Example 375](https://spec.commonmark.org/0.29/#example-375)

This markdown:

```markdown
_foo_bar_baz_

```

Should give output:

```html
<p><em>foo_bar_baz</em></p>
```

But instead was:

```html
<p>_foo_bar_baz_</p>
```
## [Example 376](https://spec.commonmark.org/0.29/#example-376)

This markdown:

```markdown
_(bar)_.

```

Should give output:

```html
<p><em>(bar)</em>.</p>
```

But instead was:

```html
<p>_(bar)_.</p>
```
## [Example 377](https://spec.commonmark.org/0.29/#example-377)

This markdown:

```markdown
**foo bar**

```

Should give output:

```html
<p><strong>foo bar</strong></p>
```

But instead was:

```html
<ul><li><p><em>foo bar</em></p></li></ul>
```
## [Example 378](https://spec.commonmark.org/0.29/#example-378)

This markdown:

```markdown
** foo bar**

```

Should give output:

```html
<p>** foo bar**</p>
```

But instead was:

```html
<ul><li><p><em>foo bar</em></p></li></ul>
```
## [Example 379](https://spec.commonmark.org/0.29/#example-379)

This markdown:

```markdown
a**"foo"**

```

Should give output:

```html
<p>a**&quot;foo&quot;**</p>
```

But instead was:

```html
<p>a<strong>&quot;foo&quot;</strong></p>
```
## [Example 381](https://spec.commonmark.org/0.29/#example-381)

This markdown:

```markdown
__foo bar__

```

Should give output:

```html
<p><strong>foo bar</strong></p>
```

But instead was:

```html
<p>__foo bar__</p>
```
## [Example 388](https://spec.commonmark.org/0.29/#example-388)

This markdown:

```markdown
__foo, __bar__, baz__

```

Should give output:

```html
<p><strong>foo,<strong>bar</strong>, baz</strong></p>
```

But instead was:

```html
<p>__foo, __bar__, baz__</p>
```
## [Example 389](https://spec.commonmark.org/0.29/#example-389)

This markdown:

```markdown
foo-__(bar)__

```

Should give output:

```html
<p>foo-<strong>(bar)</strong></p>
```

But instead was:

```html
<p>foo-__(bar)__</p>
```
## [Example 390](https://spec.commonmark.org/0.29/#example-390)

This markdown:

```markdown
**foo bar **

```

Should give output:

```html
<p>**foo bar **</p>
```

But instead was:

```html
<ul><li><p><em>foo bar</em></p></li></ul>
```
## [Example 391](https://spec.commonmark.org/0.29/#example-391)

This markdown:

```markdown
**(**foo)

```

Should give output:

```html
<p>**(**foo)</p>
```

But instead was:

```html
<ul><li><p><em>(</em><strong>foo)</strong></p></li></ul>
```
## [Example 392](https://spec.commonmark.org/0.29/#example-392)

This markdown:

```markdown
*(**foo**)*

```

Should give output:

```html
<p><em>(<strong>foo</strong>)</em></p>
```

But instead was:

```html
<ul><li><p>(<strong>foo</strong>)</p></li></ul>
```
## [Example 393](https://spec.commonmark.org/0.29/#example-393)

This markdown:

```markdown
**Gomphocarpus (*Gomphocarpus physocarpus*, syn.
*Asclepias physocarpa*)**

```

Should give output:

```html
<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.<em>Asclepias physocarpa</em>)</strong></p>
```

But instead was:

```html
<ul><li><p><em>Gomphocarpus (</em>Gomphocarpus physocarpus<em>, syn.</em></p></li><li><p>Asclepias physocarpa<em>)</em></p></li></ul>
```
## [Example 394](https://spec.commonmark.org/0.29/#example-394)

This markdown:

```markdown
**foo "*bar*" foo**

```

Should give output:

```html
<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>
```

But instead was:

```html
<ul><li><p><em>foo &quot;</em>bar<em>&quot; foo</em></p></li></ul>
```
## [Example 395](https://spec.commonmark.org/0.29/#example-395)

This markdown:

```markdown
**foo**bar

```

Should give output:

```html
<p><strong>foo</strong>bar</p>
```

But instead was:

```html
<ul><li><p><em>foo</em><strong>bar</strong></p></li></ul>
```
## [Example 398](https://spec.commonmark.org/0.29/#example-398)

This markdown:

```markdown
_(__foo__)_

```

Should give output:

```html
<p><em>(<strong>foo</strong>)</em></p>
```

But instead was:

```html
<p>_(__foo__)_</p>
```
## [Example 401](https://spec.commonmark.org/0.29/#example-401)

This markdown:

```markdown
__foo__bar__baz__

```

Should give output:

```html
<p><strong>foo__bar__baz</strong></p>
```

But instead was:

```html
<p>__foo__bar__baz__</p>
```
## [Example 402](https://spec.commonmark.org/0.29/#example-402)

This markdown:

```markdown
__(bar)__.

```

Should give output:

```html
<p><strong>(bar)</strong>.</p>
```

But instead was:

```html
<p>__(bar)__.</p>
```
## [Example 403](https://spec.commonmark.org/0.29/#example-403)

This markdown:

```markdown
*foo [bar](/url)*

```

Should give output:

```html
<p><em>foo<a href="/url">bar</a></em></p>
```

But instead was:

```html
<ul><li><p>foo<a href="/url">bar</a></p></li></ul>
```
## [Example 404](https://spec.commonmark.org/0.29/#example-404)

This markdown:

```markdown
*foo
bar*

```

Should give output:

```html
<p><em>foo bar</em></p>
```

But instead was:

```html
<ul><li><p>foo</p></li></ul><p>bar</p>
```
## [Example 405](https://spec.commonmark.org/0.29/#example-405)

This markdown:

```markdown
_foo __bar__ baz_

```

Should give output:

```html
<p><em>foo<strong>bar</strong>baz</em></p>
```

But instead was:

```html
<p>_foo __bar__ baz_</p>
```
## [Example 406](https://spec.commonmark.org/0.29/#example-406)

This markdown:

```markdown
_foo _bar_ baz_

```

Should give output:

```html
<p><em>foo<em>bar</em>baz</em></p>
```

But instead was:

```html
<p>_foo _bar_ baz_</p>
```
## [Example 407](https://spec.commonmark.org/0.29/#example-407)

This markdown:

```markdown
__foo_ bar_

```

Should give output:

```html
<p><em><em>foo</em>bar</em></p>
```

But instead was:

```html
<p>__foo_ bar_</p>
```
## [Example 408](https://spec.commonmark.org/0.29/#example-408)

This markdown:

```markdown
*foo *bar**

```

Should give output:

```html
<p><em>foo<em>bar</em></em></p>
```

But instead was:

```html
<ul><li><p>foo<em>bar</em></p></li></ul>
```
## [Example 409](https://spec.commonmark.org/0.29/#example-409)

This markdown:

```markdown
*foo **bar** baz*

```

Should give output:

```html
<p><em>foo<strong>bar</strong>baz</em></p>
```

But instead was:

```html
<ul><li><p>foo<strong>bar</strong>baz</p></li></ul>
```
## [Example 410](https://spec.commonmark.org/0.29/#example-410)

This markdown:

```markdown
*foo**bar**baz*

```

Should give output:

```html
<p><em>foo<strong>bar</strong>baz</em></p>
```

But instead was:

```html
<ul><li><p>foo<strong>bar</strong>baz</p></li></ul>
```
## [Example 411](https://spec.commonmark.org/0.29/#example-411)

This markdown:

```markdown
*foo**bar*

```

Should give output:

```html
<p><em>foo**bar</em></p>
```

But instead was:

```html
<ul><li><p>foo<strong>bar</strong></p></li></ul>
```
## [Example 412](https://spec.commonmark.org/0.29/#example-412)

This markdown:

```markdown
***foo** bar*

```

Should give output:

```html
<p><em><strong>foo</strong>bar</em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 413](https://spec.commonmark.org/0.29/#example-413)

This markdown:

```markdown
*foo **bar***

```

Should give output:

```html
<p><em>foo<strong>bar</strong></em></p>
```

But instead was:

```html
<ul><li><p>foo<strong>bar</strong></p></li></ul>
```
## [Example 414](https://spec.commonmark.org/0.29/#example-414)

This markdown:

```markdown
*foo**bar***

```

Should give output:

```html
<p><em>foo<strong>bar</strong></em></p>
```

But instead was:

```html
<ul><li><p>foo<strong>bar</strong></p></li></ul>
```
## [Example 415](https://spec.commonmark.org/0.29/#example-415)

This markdown:

```markdown
foo***bar***baz

```

Should give output:

```html
<p>foo<em><strong>bar</strong></em>baz</p>
```

But instead was:

```html
<p>foo<strong>bar</strong>baz</p>
```
## [Example 416](https://spec.commonmark.org/0.29/#example-416)

This markdown:

```markdown
foo******bar*********baz

```

Should give output:

```html
<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>
```

But instead was:

```html
<p>foo<strong>bar</strong><strong>baz</strong></p>
```
## [Example 417](https://spec.commonmark.org/0.29/#example-417)

This markdown:

```markdown
*foo **bar *baz* bim** bop*

```

Should give output:

```html
<p><em>foo<strong>bar<em>baz</em>bim</strong>bop</em></p>
```

But instead was:

```html
<ul><li><p>foo<strong>bar</strong><strong>baz</strong><strong>bim</strong>bop</p></li></ul>
```
## [Example 418](https://spec.commonmark.org/0.29/#example-418)

This markdown:

```markdown
*foo [*bar*](/url)*

```

Should give output:

```html
<p><em>foo<a href="/url"><em>bar</em></a></em></p>
```

But instead was:

```html
<ul><li><p>foo<a href="/url"><em>bar</em></a></p></li></ul>
```
## [Example 419](https://spec.commonmark.org/0.29/#example-419)

This markdown:

```markdown
** is not an empty emphasis

```

Should give output:

```html
<p>** is not an empty emphasis</p>
```

But instead was:

```html
<ul><li><p><em>is not an empty emphasis</em></p></li></ul>
```
## [Example 420](https://spec.commonmark.org/0.29/#example-420)

This markdown:

```markdown
**** is not an empty strong emphasis

```

Should give output:

```html
<p>**** is not an empty strong emphasis</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 421](https://spec.commonmark.org/0.29/#example-421)

This markdown:

```markdown
**foo [bar](/url)**

```

Should give output:

```html
<p><strong>foo<a href="/url">bar</a></strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em><a href="/url">bar</a></p></li></ul>
```
## [Example 422](https://spec.commonmark.org/0.29/#example-422)

This markdown:

```markdown
**foo
bar**

```

Should give output:

```html
<p><strong>foo bar</strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em></p></li></ul><p>bar</p>
```
## [Example 423](https://spec.commonmark.org/0.29/#example-423)

This markdown:

```markdown
__foo _bar_ baz__

```

Should give output:

```html
<p><strong>foo<em>bar</em>baz</strong></p>
```

But instead was:

```html
<p>__foo _bar_ baz__</p>
```
## [Example 424](https://spec.commonmark.org/0.29/#example-424)

This markdown:

```markdown
__foo __bar__ baz__

```

Should give output:

```html
<p><strong>foo<strong>bar</strong>baz</strong></p>
```

But instead was:

```html
<p>__foo __bar__ baz__</p>
```
## [Example 425](https://spec.commonmark.org/0.29/#example-425)

This markdown:

```markdown
____foo__ bar__

```

Should give output:

```html
<p><strong><strong>foo</strong>bar</strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 426](https://spec.commonmark.org/0.29/#example-426)

This markdown:

```markdown
**foo **bar****

```

Should give output:

```html
<p><strong>foo<strong>bar</strong></strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em><strong>bar</strong></p></li></ul>
```
## [Example 427](https://spec.commonmark.org/0.29/#example-427)

This markdown:

```markdown
**foo *bar* baz**

```

Should give output:

```html
<p><strong>foo<em>bar</em>baz</strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em>bar<em>baz</em></p></li></ul>
```
## [Example 428](https://spec.commonmark.org/0.29/#example-428)

This markdown:

```markdown
**foo*bar*baz**

```

Should give output:

```html
<p><strong>foo<em>bar</em>baz</strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em>bar<em>baz</em></p></li></ul>
```
## [Example 429](https://spec.commonmark.org/0.29/#example-429)

This markdown:

```markdown
***foo* bar**

```

Should give output:

```html
<p><strong><em>foo</em>bar</strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 430](https://spec.commonmark.org/0.29/#example-430)

This markdown:

```markdown
**foo *bar***

```

Should give output:

```html
<p><strong>foo<em>bar</em></strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em>bar</p></li></ul>
```
## [Example 431](https://spec.commonmark.org/0.29/#example-431)

This markdown:

```markdown
**foo *bar **baz**
bim* bop**

```

Should give output:

```html
<p><strong>foo<em>bar<strong>baz</strong>bim</em>bop</strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em>bar<strong>baz</strong></p></li></ul><p>bim<em>bop</em></p>
```
## [Example 432](https://spec.commonmark.org/0.29/#example-432)

This markdown:

```markdown
**foo [*bar*](/url)**

```

Should give output:

```html
<p><strong>foo<a href="/url"><em>bar</em></a></strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em><a href="/url"><em>bar</em></a></p></li></ul>
```
## [Example 434](https://spec.commonmark.org/0.29/#example-434)

This markdown:

```markdown
____ is not an empty strong emphasis

```

Should give output:

```html
<p>____ is not an empty strong emphasis</p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 435](https://spec.commonmark.org/0.29/#example-435)

This markdown:

```markdown
foo ***

```

Should give output:

```html
<p>foo ***</p>
```

But instead was:

```html
<p>foo</p>
```
## [Example 436](https://spec.commonmark.org/0.29/#example-436)

This markdown:

```markdown
foo *\**

```

Should give output:

```html
<p>foo<em>*</em></p>
```

But instead was:

```html
<p>foo<em>\</em></p>
```
## [Example 438](https://spec.commonmark.org/0.29/#example-438)

This markdown:

```markdown
foo *****

```

Should give output:

```html
<p>foo *****</p>
```

But instead was:

```html
<p>foo</p>
```
## [Example 439](https://spec.commonmark.org/0.29/#example-439)

This markdown:

```markdown
foo **\***

```

Should give output:

```html
<p>foo<strong>*</strong></p>
```

But instead was:

```html
<p>foo<strong>\</strong></p>
```
## [Example 441](https://spec.commonmark.org/0.29/#example-441)

This markdown:

```markdown
**foo*

```

Should give output:

```html
<p>*<em>foo</em></p>
```

But instead was:

```html
<ul><li><p><em>foo</em></p></li></ul>
```
## [Example 442](https://spec.commonmark.org/0.29/#example-442)

This markdown:

```markdown
*foo**

```

Should give output:

```html
<p><em>foo</em>*</p>
```

But instead was:

```html
<ul><li><p>foo</p></li></ul>
```
## [Example 443](https://spec.commonmark.org/0.29/#example-443)

This markdown:

```markdown
***foo**

```

Should give output:

```html
<p>*<strong>foo</strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 444](https://spec.commonmark.org/0.29/#example-444)

This markdown:

```markdown
****foo*

```

Should give output:

```html
<p>***<em>foo</em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 445](https://spec.commonmark.org/0.29/#example-445)

This markdown:

```markdown
**foo***

```

Should give output:

```html
<p><strong>foo</strong>*</p>
```

But instead was:

```html
<ul><li><p><em>foo</em></p></li></ul>
```
## [Example 446](https://spec.commonmark.org/0.29/#example-446)

This markdown:

```markdown
*foo****

```

Should give output:

```html
<p><em>foo</em>***</p>
```

But instead was:

```html
<ul><li><p>foo</p></li></ul>
```
## [Example 448](https://spec.commonmark.org/0.29/#example-448)

This markdown:

```markdown
foo _\__

```

Should give output:

```html
<p>foo<em>_</em></p>
```

But instead was:

```html
<p>foo _\__</p>
```
## [Example 449](https://spec.commonmark.org/0.29/#example-449)

This markdown:

```markdown
foo _*_

```

Should give output:

```html
<p>foo<em>*</em></p>
```

But instead was:

```html
<p>foo _<em>_</em></p>
```
## [Example 451](https://spec.commonmark.org/0.29/#example-451)

This markdown:

```markdown
foo __\___

```

Should give output:

```html
<p>foo<strong>_</strong></p>
```

But instead was:

```html
<p>foo __\___</p>
```
## [Example 452](https://spec.commonmark.org/0.29/#example-452)

This markdown:

```markdown
foo __*__

```

Should give output:

```html
<p>foo<strong>*</strong></p>
```

But instead was:

```html
<p>foo __<em>__</em></p>
```
## [Example 453](https://spec.commonmark.org/0.29/#example-453)

This markdown:

```markdown
__foo_

```

Should give output:

```html
<p>_<em>foo</em></p>
```

But instead was:

```html
<p>__foo_</p>
```
## [Example 454](https://spec.commonmark.org/0.29/#example-454)

This markdown:

```markdown
_foo__

```

Should give output:

```html
<p><em>foo</em>_</p>
```

But instead was:

```html
<p>_foo__</p>
```
## [Example 455](https://spec.commonmark.org/0.29/#example-455)

This markdown:

```markdown
___foo__

```

Should give output:

```html
<p>_<strong>foo</strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 456](https://spec.commonmark.org/0.29/#example-456)

This markdown:

```markdown
____foo_

```

Should give output:

```html
<p>___<em>foo</em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 457](https://spec.commonmark.org/0.29/#example-457)

This markdown:

```markdown
__foo___

```

Should give output:

```html
<p><strong>foo</strong>_</p>
```

But instead was:

```html
<p>__foo___</p>
```
## [Example 458](https://spec.commonmark.org/0.29/#example-458)

This markdown:

```markdown
_foo____

```

Should give output:

```html
<p><em>foo</em>___</p>
```

But instead was:

```html
<p>_foo____</p>
```
## [Example 459](https://spec.commonmark.org/0.29/#example-459)

This markdown:

```markdown
**foo**

```

Should give output:

```html
<p><strong>foo</strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em></p></li></ul>
```
## [Example 460](https://spec.commonmark.org/0.29/#example-460)

This markdown:

```markdown
*_foo_*

```

Should give output:

```html
<p><em><em>foo</em></em></p>
```

But instead was:

```html
<ul><li><p>_foo_</p></li></ul>
```
## [Example 461](https://spec.commonmark.org/0.29/#example-461)

This markdown:

```markdown
__foo__

```

Should give output:

```html
<p><strong>foo</strong></p>
```

But instead was:

```html
<p>__foo__</p>
```
## [Example 462](https://spec.commonmark.org/0.29/#example-462)

This markdown:

```markdown
_*foo*_

```

Should give output:

```html
<p><em><em>foo</em></em></p>
```

But instead was:

```html
<p>_<em>foo</em>_</p>
```
## [Example 463](https://spec.commonmark.org/0.29/#example-463)

This markdown:

```markdown
****foo****

```

Should give output:

```html
<p><strong><strong>foo</strong></strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 464](https://spec.commonmark.org/0.29/#example-464)

This markdown:

```markdown
____foo____

```

Should give output:

```html
<p><strong><strong>foo</strong></strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 465](https://spec.commonmark.org/0.29/#example-465)

This markdown:

```markdown
******foo******

```

Should give output:

```html
<p><strong><strong><strong>foo</strong></strong></strong></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 466](https://spec.commonmark.org/0.29/#example-466)

This markdown:

```markdown
***foo***

```

Should give output:

```html
<p><em><strong>foo</strong></em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 467](https://spec.commonmark.org/0.29/#example-467)

This markdown:

```markdown
_____foo_____

```

Should give output:

```html
<p><em><strong><strong>foo</strong></strong></em></p>
```

But instead was:

```html
ERROR Problem at row 1 Expecting end Problem at row 1 Expecting newline
```
## [Example 468](https://spec.commonmark.org/0.29/#example-468)

This markdown:

```markdown
*foo _bar* baz_

```

Should give output:

```html
<p><em>foo _bar</em>baz_</p>
```

But instead was:

```html
<ul><li><p>foo _bar<em>baz_</em></p></li></ul>
```
## [Example 469](https://spec.commonmark.org/0.29/#example-469)

This markdown:

```markdown
*foo __bar *baz bim__ bam*

```

Should give output:

```html
<p><em>foo<strong>bar *baz bim</strong>bam</em></p>
```

But instead was:

```html
<ul><li><p>foo __bar<em>baz bim__ bam</em></p></li></ul>
```
## [Example 470](https://spec.commonmark.org/0.29/#example-470)

This markdown:

```markdown
**foo **bar baz**

```

Should give output:

```html
<p>**foo<strong>bar baz</strong></p>
```

But instead was:

```html
<ul><li><p><em>foo</em><strong>bar baz</strong></p></li></ul>
```
## [Example 471](https://spec.commonmark.org/0.29/#example-471)

This markdown:

```markdown
*foo *bar baz*

```

Should give output:

```html
<p>*foo<em>bar baz</em></p>
```

But instead was:

```html
<ul><li><p>foo<em>bar baz</em></p></li></ul>
```
## [Example 472](https://spec.commonmark.org/0.29/#example-472)

This markdown:

```markdown
*[bar*](/url)

```

Should give output:

```html
<p>*<a href="/url">bar*</a></p>
```

But instead was:

```html
<ul><li><p><a href="/url">bar</a></p></li></ul>
```
## [Example 474](https://spec.commonmark.org/0.29/#example-474)

This markdown:

```markdown
*<img src="foo" title="*"/>

```

Should give output:

```html
<p>*<img src="foo" title="*"></p>
```

But instead was:

```html
<ul><li><p>&lt;img src=&quot;foo&quot; title=&quot;<em>&quot;/&gt;</em></p></li></ul>
```
## [Example 475](https://spec.commonmark.org/0.29/#example-475)

This markdown:

```markdown
**<a href="**">

```

Should give output:

```html
<p>**<a href="**"></p>
```

But instead was:

```html
<ul><li><p><em>&lt;a href=&quot;</em><strong>&quot;&gt;</strong></p></li></ul>
```
## [Example 476](https://spec.commonmark.org/0.29/#example-476)

This markdown:

```markdown
__<a href="__">

```

Should give output:

```html
<p>__<a href="__"></p>
```

But instead was:

```html
<p>__&lt;a href=&quot;__&quot;&gt;</p>
```
## [Example 477](https://spec.commonmark.org/0.29/#example-477)

This markdown:

```markdown
*a `*`*

```

Should give output:

```html
<p><em>a<code>*</code></em></p>
```

But instead was:

```html
<ul><li><p>a</p></li></ul>
```
## [Example 478](https://spec.commonmark.org/0.29/#example-478)

This markdown:

```markdown
_a `_`_

```

Should give output:

```html
<p><em>a<code>_</code></em></p>
```

But instead was:

```html
<p>_a<code>_</code>_</p>
```
## [Example 479](https://spec.commonmark.org/0.29/#example-479)

This markdown:

```markdown
**a<http://foo.bar/?q=**>

```

Should give output:

```html
<p>**a<a href="http://foo.bar/?q=**">http://foo.bar/?q=**</a></p>
```

But instead was:

```html
<ul><li><p><em>a&lt;http://foo.bar/?q=</em><strong>&gt;</strong></p></li></ul>
```
## [Example 480](https://spec.commonmark.org/0.29/#example-480)

This markdown:

```markdown
__a<http://foo.bar/?q=__>

```

Should give output:

```html
<p>__a<a href="http://foo.bar/?q=__">http://foo.bar/?q=__</a></p>
```

But instead was:

```html
<p>__a&lt;http://foo.bar/?q=__&gt;</p>
```

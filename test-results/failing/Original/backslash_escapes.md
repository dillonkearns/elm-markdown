# Original - backslash_escapes

## Example undefined

This markdown:

````````````markdown
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

````````````

Should give output:

````````````html
<p>These should all get escaped:</p><p>Backslash: \</p><p>Backtick: `</p><p>Asterisk: *</p><p>Underscore: _</p><p>Left brace: {</p><p>Right brace: }</p><p>Left bracket: [</p><p>Right bracket: ]</p><p>Left paren: (</p><p>Right paren: )</p><p>Greater-than: &gt;</p><p>Hash: #</p><p>Period: .</p><p>Bang: !</p><p>Plus: +</p><p>Minus: -</p><p>These should not, because they occur within a code block:</p><pre><code>Backslash: \\ Backtick: \` Asterisk: \* Underscore: \_ Left brace: \{ Right brace: \} Left bracket: \[ Right bracket: \] Left paren: \( Right paren: \) Greater-than: \&gt; Hash: \# Period: \. Bang: \! Plus: \+ Minus: \-</code></pre><p>Nor should these, which occur in code spans:</p><p>Backslash:<code>\\</code></p><p>Backtick:<code>\`</code></p><p>Asterisk:<code>\*</code></p><p>Underscore:<code>\_</code></p><p>Left brace:<code>\{</code></p><p>Right brace:<code>\}</code></p><p>Left bracket:<code>\[</code></p><p>Right bracket:<code>\]</code></p><p>Left paren:<code>\(</code></p><p>Right paren:<code>\)</code></p><p>Greater-than:<code>\&gt;</code></p><p>Hash:<code>\#</code></p><p>Period:<code>\.</code></p><p>Bang:<code>\!</code></p><p>Plus:<code>\+</code></p><p>Minus:<code>\-</code></p><p>These should get escaped, even though they&#39;re matching pairs for other Markdown constructs:</p><p>*asterisks*</p><p>_underscores_</p><p>`backticks`</p><p>This is a code span with a literal backslash-backtick sequence:<code>\`</code></p><p>This is a tag with unescaped backticks<span attr="`ticks`">bar</span>.</p><p>This is a tag with backslashes<span attr="\\backslashes\\">bar</span>.</p>
````````````

But instead was:

````````````html
<p>These should all get escaped:</p><p>Backslash: \</p><p>Backtick: `</p><p>Asterisk: *</p><p>Underscore: _</p><p>Left brace: {</p><p>Right brace: }</p><p>Left bracket: [</p><p>Right bracket: ]</p><p>Left paren: (</p><p>Right paren: )</p><p>Greater-than: &gt;</p><p>Hash: #</p><p>Period: .</p><p>Bang: !</p><p>Plus: +</p><p>Minus: -</p><p>These should not, because they occur within a code block:</p><pre><code>Backslash: \\ Backtick: \` Asterisk: \* Underscore: \_ Left brace: \{ Right brace: \} Left bracket: \[ Right bracket: \] Left paren: \( Right paren: \) Greater-than: \&gt; Hash: \# Period: \. Bang: \! Plus: \+ Minus: \-</code></pre><p>Nor should these, which occur in code spans:</p><p>Backslash:<code>\\</code></p><p>Backtick:<code>\`</code></p><p>Asterisk:<code>\*</code></p><p>Underscore:<code>\_</code></p><p>Left brace:<code>\{</code></p><p>Right brace:<code>\}</code></p><p>Left bracket:<code>\[</code></p><p>Right bracket:<code>\]</code></p><p>Left paren:<code>\(</code></p><p>Right paren:<code>\)</code></p><p>Greater-than:<code>\&gt;</code></p><p>Hash:<code>\#</code></p><p>Period:<code>\.</code></p><p>Bang:<code>\!</code></p><p>Plus:<code>\+</code></p><p>Minus:<code>\-</code></p><p>These should get escaped, even though they&#39;re matching pairs for other Markdown constructs:</p><p>*asterisks*</p><p>_underscores_</p><p>`backticks`</p><p>This is a code span with a literal backslash-backtick sequence:<code>\`</code></p><p>This is a tag with unescaped backticks<span attr="`ticks`"><p>bar</p></span>.</p><p>This is a tag with backslashes<span attr="\\backslashes\\"><p>bar</p></span>.</p>
````````````

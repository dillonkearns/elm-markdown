# GFM - [extension] Disallowed Raw HTML

## [Example 653](https://github.github.com/gfm/#example-653)

This markdown:

```markdown
<strong> <title> <style> <em>

<blockquote>
  <xmp> is disallowed.  <XMP> is also disallowed.
</blockquote>
```

Should give output:

```html
<p><strong>&lt;title&gt; &lt;style&gt;<em></p><blockquote>&lt;xmp&gt; is disallowed. &lt;XMP&gt; is also disallowed.</blockquote>
```

But instead was:

```html
<p>&lt;strong&gt; &lt;title&gt; &lt;style&gt; &lt;em&gt;</p><p>&lt;blockquote&gt; &lt;xmp&gt; is disallowed. &lt;XMP&gt; is also disallowed. &lt;/blockquote&gt;</p>
```

# Original - hard_wrapped_paragraphs_with_list_like_lines

## Example undefined

This markdown:

````````````markdown
In Markdown 1.0.0 and earlier. Version
8. This line turns into a list item.
Because a hard-wrapped line in the
middle of a paragraph looked like a
list item.

Here's one with a bullet.
* criminey.

````````````

Should give output:

````````````html
<p>In Markdown 1.0.0 and earlier. Version 8. This line turns into a list item. Because a hard-wrapped line in the middle of a paragraph looked like a list item.</p><p>Here&#39;s one with a bullet. * criminey.</p>
````````````

But instead was:

````````````html
<p>In Markdown 1.0.0 and earlier. Version 8. This line turns into a list item. Because a hard-wrapped line in the middle of a paragraph looked like a list item.</p><p>Here&#39;s one with a bullet.</p><ul><li>criminey.</li></ul>
````````````

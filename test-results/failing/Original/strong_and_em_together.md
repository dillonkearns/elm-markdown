# Original - strong_and_em_together

## Example undefined

This markdown:

````````````markdown
***This is strong and em.***

So is ***this*** word.

___This is strong and em.___

So is ___this___ word.

````````````

Should give output:

````````````html
<p><strong><em>This is strong and em.</em></strong></p><p>So is<strong><em>this</em></strong>word.</p><p><strong><em>This is strong and em.</em></strong></p><p>So is<strong><em>this</em></strong>word.</p>
````````````

But instead was:

````````````html
<p><em><strong>This is strong and em.</strong></em></p><p>So is<em><strong>this</strong></em>word.</p><p><em><strong>This is strong and em.</strong></em></p><p>So is<em><strong>this</strong></em>word.</p>
````````````

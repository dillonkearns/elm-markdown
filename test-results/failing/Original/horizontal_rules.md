# Original - horizontal_rules

## Example undefined

This markdown:

````````````markdown
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

````````````

Should give output:

````````````html
<p>Dashes:</p><hr><hr><hr><hr><pre><code>---</code></pre><hr><hr><hr><hr><pre><code>- - -</code></pre><p>Asterisks:</p><hr><hr><hr><hr><pre><code>***</code></pre><hr><hr><hr><hr><pre><code>* * *</code></pre><p>Underscores:</p><hr><hr><hr><hr><pre><code>___</code></pre><hr><hr><hr><hr><pre><code>_ _ _</code></pre><p>Not horizontal rules:</p><p>--*</p><p>-*-</p><p>*--</p><p>-_-</p><p>__-</p><p>-__</p><pre><code>_-_</code></pre><p>Long rules:</p><hr><hr><hr>
````````````

But instead was:

````````````html
<p>Dashes:</p><hr><hr><hr><hr><pre><code>---</code></pre><hr><hr><hr><hr><pre><code>- - -</code></pre><p>Asterisks:</p><hr><hr><hr><hr><pre><code>***</code></pre><hr><hr><hr><hr><pre><code>* * *</code></pre><p>Underscores:</p><p>___</p><p>___</p><p>___</p><p>___</p><pre><code>___</code></pre><p>_ _ _</p><p>_ _ _</p><p>_ _ _</p><p>_ _ _</p><pre><code>_ _ _</code></pre><p>Not horizontal rules:</p><p>--*</p><p>-*-</p><p>*--</p><p>-_-</p><p>__-</p><p>-__</p><pre><code>_-_</code></pre><p>Long rules:</p><hr><p>___________</p><hr>
````````````

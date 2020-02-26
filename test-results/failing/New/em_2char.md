# New - em_2char

## Example undefined

This markdown:

````````````markdown
_123_

*123*

_12_

*12*

_1_

*1*

__

**

_123 _

*123 *

_ 123_

_1__

*1**

````````````

Should give output:

````````````html
<p><em>123</em></p><p><em>123</em></p><p><em>12</em></p><p><em>12</em></p><p><em>1</em></p><p><em>1</em></p><p>__</p><p>**</p><p>_123 _</p><p>*123 *</p><p>_ 123_</p><p><em>1_</em></p><p><em>1*</em></p>
````````````

But instead was:

````````````html
<p><em>123</em></p><p><em>123</em></p><p><em>12</em></p><p><em>12</em></p><p><em>1</em></p><p><em>1</em></p><p>__</p><p>**</p><p>_123 _</p><p>*123 *</p><p>_ 123_</p><p><em>1</em>_</p><p><em>1</em>*</p>
````````````

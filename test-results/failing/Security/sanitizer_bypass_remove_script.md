# Security - sanitizer_bypass_remove_script

## Example undefined

This markdown:

````````````markdown
AAA<script> <img <script> src=x onerror=alert(1) />BBB

````````````

Should give output:

````````````html
<p>AAA</p>
````````````

But instead was:

````````````html
<p>AAA&lt;script&gt; &lt;img &lt;script&gt; src=x onerror=alert(1) /&gt;BBB</p>
````````````

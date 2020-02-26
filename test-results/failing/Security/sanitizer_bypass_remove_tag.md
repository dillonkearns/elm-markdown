# Security - sanitizer_bypass_remove_tag

## Example undefined

This markdown:

````````````markdown
AAA<sometag> <img <sometag> src=x onerror=alert(1)BBB

````````````

Should give output:

````````````html
<p>AAA &lt;img src=x onerror=alert(1)BBB</p>
````````````

But instead was:

````````````html
<p>AAA&lt;sometag&gt; &lt;img &lt;sometag&gt; src=x onerror=alert(1)BBB</p>
````````````

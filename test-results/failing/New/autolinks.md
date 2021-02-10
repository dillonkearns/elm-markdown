# New - autolinks

## Example undefined

This markdown:

````````````markdown
(See https://www.example.com/fhqwhgads.)

((http://foo.com))

((http://foo.com.))

HTTP://FOO.COM

hTtP://fOo.CoM

~~hello@email.com~~

**me@example.com**

__test@test.com__
````````````

Should give output:

````````````html
<p>(See<a href="https://www.example.com/fhqwhgads">https://www.example.com/fhqwhgads</a>.)</p><p>((<a href="http://foo.com">http://foo.com</a>))</p><p>((<a href="http://foo.com">http://foo.com</a>.))</p><p><a href="HTTP://FOO.COM">HTTP://FOO.COM</a></p><p><a href="hTtP://fOo.CoM">hTtP://fOo.CoM</a></p><p><del><a href="mailto:hello@email.com">hello@email.com</a></del></p><p><strong><a href="mailto:me@example.com">me@example.com</a></strong></p><p><strong><a href="mailto:test@test.com">test@test.com</a></strong></p>
````````````

But instead was:

````````````html
<p>(See https://www.example.com/fhqwhgads.)</p><p>((http://foo.com))</p><p>((http://foo.com.))</p><p>HTTP://FOO.COM</p><p>hTtP://fOo.CoM</p><p><del>hello@email.com</del></p><p><strong>me@example.com</strong></p><p><strong>test@test.com</strong></p>
````````````

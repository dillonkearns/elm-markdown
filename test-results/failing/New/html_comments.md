# New - html_comments

## Example undefined

This markdown:

````````````markdown
### Example 1

<!-- comment -->

### Example 2

<!---->

### Example 3

<!-- -->

### Example 4

<!-- - -->

### Example 5

<!-- -- -->

### Example 6

<!-- --->

### Example 7

<!----->

### Example 8

<!------>

### Example 9

<!-- My favorite operators are > and <!-->

### Example 10

<!-- multi
line	
comment
-->

### Example 11

   <!-- indented comment -->

    <!-- too much indentation -->

### Example 12

<!--> not a comment -->

<!---> not a comment -->

<!-- <!-- not a comment? --> -->
````````````

Should give output:

````````````html
<h3 id="example-1">Example 1</h3><h3 id="example-2">Example 2</h3><h3 id="example-3">Example 3</h3><h3 id="example-4">Example 4</h3><h3 id="example-5">Example 5</h3><h3 id="example-6">Example 6</h3><h3 id="example-7">Example 7</h3><h3 id="example-8">Example 8</h3><h3 id="example-9">Example 9</h3><h3 id="example-10">Example 10</h3><h3 id="example-11">Example 11</h3><pre><code>&lt;!-- too much indentation --&gt;</code></pre><h3 id="example-12">Example 12</h3><p>&lt;!--&gt; not a comment --&gt;</p><p>&lt;!---&gt; not a comment --&gt;</p>--&gt;
````````````

But instead was:

````````````html
<h3>Example 1</h3><h3>Example 2</h3><h3>Example 3</h3><h3>Example 4</h3><h3>Example 5</h3><h3>Example 6</h3><h3>Example 7</h3><h3>Example 8</h3><h3>Example 9</h3><h3>Example 10</h3><h3>Example 11</h3><p></p><pre><code>&lt;!-- too much indentation --&gt;</code></pre><h3>Example 12</h3><p>--&gt;</p>
````````````

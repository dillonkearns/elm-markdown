# New - table_cells

## Example undefined

This markdown:

````````````markdown
|1|
|-|
|1|

|1|
|-|
|\||

|1|
|-|
|1\\1|

|1|
|-|
|\\\\||

|1|
|-|
|\\\\\||

|1|2|
|-|-|
||2|

|1|2|
|-|-|
|1\|\\|2\|\\|

|1|2|
|-|-|
| |2|

1|2
-|-
1\|\\|2\|\\

1|2
-|-
 |2

1|2
-|-
1|2\|

1|2
-|-
1|2\|

|1|2|
|-|-|
|1|2\||

|1|2|
|-|-|
|1|2\||

````````````

Should give output:

````````````html
<table><thead><tr><th>1</th></tr></thead><tbody><tr><td>1</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>|</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>1\1</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>\\</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>\\|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1|\</td><td>2|\</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1|\</td><td>2|\</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table>
````````````

But instead was:

````````````html
<table><thead><tr><th>1</th></tr></thead><tbody><tr><td>1</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>|</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>1\1</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>|</td></tr></tbody></table><table><thead><tr><th>1</th></tr></thead><tbody><tr><td>\|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>2</td><td></td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1||2||</td><td></td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1||2|\</td><td></td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td></td><td>2</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table><table><thead><tr><th>1</th><th>2</th></tr></thead><tbody><tr><td>1</td><td>2|</td></tr></tbody></table>
````````````

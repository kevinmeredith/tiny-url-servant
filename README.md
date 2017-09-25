# project

## How to Run

```bash
$stack build
$stack exec project-exe
# it's now running the web server

# open another terminal

$curl --header "Accept: text/html" localhost:8081/tinyUrl/asdfasdf
<tr><td>asdfasdf</td></tr>
$
$curl --header "Accept: application/json" localhost:8081/tinyUrl/asdfasdf
{"value":"asdfasdf"}$
```

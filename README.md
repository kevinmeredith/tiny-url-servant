# project

## How to Run

```bash
$stack build
$stack exec project-exe
-- it's now running

# open another terminal

$curl -i localhost:8081/tinyUrl/foobar
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Fri, 22 Sep 2017 12:39:19 GMT
Server: Warp/3.2.13
Content-Type: application/json;charset=utf-8

{"value":"foobar"}

$curl -i localhost:8081/tinyUrl/zigzag
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Fri, 22 Sep 2017 12:39:22 GMT
Server: Warp/3.2.13
Content-Type: application/json;charset=utf-8

{"value":"zigzag"}
```

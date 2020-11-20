# executor-test

Simple example of a test executor, which is completely in-memory.

## Usage

Start the executor with

```shell
> lein run
```

This will create a local server that listens on port `3001` and which you can
send commands to via POST `/api/command` end-point.

This executor has two components `inc` and `store`, the store component holds a
single value, and the inc component is used to try to increment the value. It
tries to increment the value by first asking for the value and then setting it
to `(inc value)` this can of course go horribly wrong if two concurrent
increment request are in flight at the same time.

```shell
> http :3001/api/command command:='{"name": "do-inc", "parameters": {}}' component-id="inc"                
HTTP/1.1 200 OK
Content-Length: 110
Content-Type: application/octet-stream
Date: Wed, 12 Aug 2020 13:42:06 GMT
Server: http-kit

[{"component-id":"store","command":{"name":"fetch","parameters":{"return":"inc","return-name":"$set-value"}}}]
```

The response are an array of new request to be scheduled, in this case a request to `store` with the
`fetch` command (which as parameters has the return address, which maybe always should be present?).



## License

Copyright © 2020 Symbiont.io

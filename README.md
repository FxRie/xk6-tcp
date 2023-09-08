# xk6-tcp

A k6 extension for sending strings to TCP port

## Build

To build a `k6` binary with this plugin, first ensure you have the prerequisites:

- [Go toolchain](https://go101.org/article/go-toolchain.html)
- Git

Then:

1. Install `xk6`:

  ```shell
  go install github.com/k6io/xk6/cmd/xk6@latest
  ```

2. Build the binary:

  ```shell
  xk6 build master \
    --with github.com/NAlexandrov/xk6-tcp
  ```

## Example

```javascript
import tcp from 'k6/x/tcpx';
import { expect } from "https://jslib.k6.io/k6chaijs/4.3.4.3/index.js";


export default function () {
  // Should be done here as there will be an error when all the VUs use the same tcp connection
  const conn = tcp.connect('host:port');

  tcp.writeLn(conn, 'Say Hello', '\r\n');
  tcp.closeWrite(conn);

  let res = String.fromCharCode(...tcp.read(conn, 1024));
  expect(res).to.equal("Hello\r\n");

  tcp.close(conn);
}
```

## TODO 
* Add support for metrics here are some helpful links
  - https://pkg.go.dev/github.com/bastjan/netstat#section-readme
  - https://github.com/bastjan/netstat/blob/v1.0.0/examples/netstat_tulpen/netstat_tulpen.go
  - https://github.com/mostafa/xk6-kafka/blob/6e723849f636d77cadefd0fd9d91884ddf489c7b/stats.go
* Add read line support
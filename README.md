# Open api generator (still work in progress)

This project allows to generate stuff from an OpenAPI definition.
The generator is based on OpenAPI v3 definition but does support also OpenAPI v2 definition.

This project is written in Scala 2.13 with sbt as build system, the generation is done via [play twirl](https://github.com/playframework/twirl).

### Available generators

#### Akka http server
Generates a fully asynchronous HTTP/1.1 server based on [akka-http](https://doc.akka.io/docs/akka-http/current/index.html).
The server is generated as sbt module.

It supports the following OpenAPI features:
- Body request decode for json format.
- Body response encode for json format.
- Request parameter validation: `minimum`, `maximum`, `minItems`, `maxItems`, `minLength`, `maxLength`.
- Request body request based on `Content-Type` request header.
  If the header is without charset, a default one is used.
- Response body response based on `Accept` request header.
  If `Accept` is not provided or it does not match any response defined, the server returns the first response defined in the OpenAPI.

Additional features:
- Custom handler for requests that does not match any path defined (ie. return 404 error response with proper body).
- Default charset for requests with `Content-Type` without charset.
- Request limits based on byte size and transfer time (see [Slowloris DDoS attack](https://en.wikipedia.org/wiki/Slowloris_(computer_security))).
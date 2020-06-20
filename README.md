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
- Request limits based on byte size and transfer time (see [Slowloris DDoS attack](https://en.wikipedia.org/wiki/Slowloris_(computer_security)).

TODO (in priority order):
- Add request parameter (query, path and header) decode:
  - support required parameters
  - support primitive types
  - think a way to support [style](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.3.md#style-values)
- Add more specific structure for error modelling:
  - distinguish between request error and response error
  - improve request error model (model parameter errors, body field errors, error cause)
- Review some definitions:
  - `OperationsHandler.getOperationsHandler` maybe we should add a specific configuration object
  - A response is defined as case class with all possible response models defined, they should be lazy
    ```scala
    case class FindPetsByTags200 (
      `application/json`: () => List[Pet] ,
      `application/xml`: () => List[Pet]
    )
    ```
    or
    ```scala
    trait FindPetsByTags200 {
      def `application/json`(): List[Pet]
      def `application/xml`(): List[Pet]
    }
    ```
  - Which is the better way to start and use the server?
    ```scala
    implicit val system = ActorSystem("MyApplication")
    implicit val materializer = ActorMaterializer()
   
    val operationImpl:Operations = ???
    def defaultResponse(ctx:RequestContext) =
      HttpResponse(StatusCodes.BadRequest, s"Invalid request (method=${ctx.request.method.value},path=${ctx.request.uri.path})")
    
    Http().bindAndHandle(
      handler = OperationsHandler.getOperationsHandler(operationImpl, defaultResponse, 1.hour, 1000, StandardCharsets.UTF_8),
      interface = "localhost",
      port = 8080
    )
    ```
- Complete or add support for missing schemas: date time, boolean, typed map, free map, file.
- Add support for content types: `application/xml`, `application/form`, `multipart/form`, `octet/stream`.
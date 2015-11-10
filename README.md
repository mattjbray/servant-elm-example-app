Servant-Elm Example App
=======================

This is an example application making use of the
[servant-elm](https://github.com/mattjbray/servant-elm) library.

To build and run this application, you need to install
[stack](https://github.com/commercialhaskell/stack) and
[Elm](http://elm-lang.org/install).

Assuming you have both tools installed, you can run the example with:

```
make && make serve
```


App layout
----------

If you study [servant-elm-example-app.cabal](servant-elm-example-app.cabal), you will see that the Haskell part
of this project comprises four build targets:

1. an `api` library, which will contain our Servant API type and server
   implementation;

2. a `backend` executable, which serves a home page and our API under `/api`, as
   well as our static assets;

3. a `code-generator` executable, which uses
   [servant-elm](https://github.com/mattjbray/servant-elm) to generate Elm code
   for interacting with our API; and

4. a test-suite (not yet implemented).

Our Elm code is in the [frontend/src](frontend/src) directory.


Build process
-------------

The [Makefile](Makefile) ties everything together.

First we build the `api` library and the `backend` and `code-generator`
executables by running `stack build`.

Then we run `stack exec code-generator`, directing the output Elm code to
[frontend/src/Generated/Api.elm](frontend/src/Generated/Api.elm).

Finally, we run `elm-make` to build `frontend/dist/app.js`.


Components in detail
--------------------


### The API

The definition of our API lives in [api/Api/Types.hs](api/Api/Types.hs). It's
pretty simple. You can:

* `POST /counter/inc`
* `GET /counter`

Both endpoints return a representation of the counter in JSON.

The implementation lives in [api/Api/Server.hs](api/Api/Server.hs). The counter
state lives in a `TVar Int`, which must be supplied by the user.


### The backend

[backend/Main.hs](backend/Main.hs) defines a type called `SiteApi`, which wraps
our counter API under `/api`, provides an index route and serves assets under
`/assets`.

The `server` function implements this `SiteApi`. It wraps the API server
implementation, serves the `frontend/dist` directory as `/assets`, and serves
a home page.

The home page simply sets a page title and bootstraps our Elm app (which will be
built to `frontend/dist/app.js`).

The `main` function creates a new `TVar` for our counter API and starts the app
on port 8000.


### The code generator

[code-generator/Main.hs](code-generator/Main.hs) imports our Api type and calls
servant-elm's `elmJSWith` with some options:

* `moduleName` configures the name of the Elm module that will be generated.
  This must match the filepath that the Elm code will be written to, and any
  import statements in your own Elm code using the generated code. In our case
  we have used `Generated.Api`.

* `urlPrefix` specifies where the frontend code can connect to the API. In our
  case it is `http://localhost:8000/api`.


### The frontend

In [frontend/src/Main.elm](frontend/src/Main.elm) we import the `Generated.Api`
module. Our Elm app uses the `StartApp` module (see
[The Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/) for
details).

The functions `fetchCounter` and `incCounter` demonstrate how the generated Elm
functions can be used. Each generated function takes a `Json.Decode.Decoder`
which is responsable for marshalling the Api's JSON response into an Elm value.
In our case this is just `Json.Decode.int`.

The return value of the generated functions is `Task Http.Error a`, which in
this example are transformed to `Effects (Maybe Int)` for use with `StartApp`.


TODO
----

* Demonstrate API endpoints that take parameters and captures.
* Try to make coupling between components more explicit:
** port and API prefix must match between `backend` and `code-generator`,
** Elm module name must match between `Makefile` and `code-generator` and
   `frontend`,
** frontend build directory must match between `Makefile` and `backend`.

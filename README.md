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

If you study `servant-elm-example-app.cabal`, you will see that the Haskell part
of this project comprises four build targets:

1. a library (in `./api`), which will contain our Servant API type and server
   implementation;

2. a `backend` executable (in `./backend`), which serves a home page and our API
   under `/api`, as well as our static assets;

3. a `code-generator` executable (in `./code-generator`), which uses
   [servant-elm](https://github.com/mattjbray/servant-elm) to generate Elm code
   to interact with our API; and

4. a test-suite (not yet implemented).

Our Elm code is in the `frontend/src` directory.


Build process
-------------

The `Makefile` ties everything together.

First we build the `api` library and the `backend` and `code-generator`
executables by running `stack build`.

Then we run `stack exec code-generator`, directing the output Elm code to
`frontend/src/Generated/Api.elm`.

Finally, we run `elm-make` to build `frontend/dist/app.js`.

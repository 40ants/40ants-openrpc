<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# 40ants-openrpc - A set of helpers to start JSON-RPC server based on https://40ants.com/openrpc/ library.

<a id="40-ants-openrpc-asdf-system-details"></a>

## 40ANTS-OPENRPC ASDF System Details

* Description: A set of helpers to start `JSON-RPC` server based on https://40ants.com/openrpc/ library.
* Licence: Unlicense
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/40ants-openrpc/][e778]
* Bug tracker: [https://github.com/40ants/40ants-openrpc/issues][a84b]
* Source control: [GIT][8702]
* Depends on: [40ants-logging][422a], [40ants-slynk][2e1d], [alexandria][8236], [cl+ssl][fca9], [cl-json-web-tokens][674d], [clack][482d], [clack-cors][087e], [clack-prometheus][babd], [dexador][8347], [jsonrpc][a9bd], [lack-request][6a02], [local-time][46a1], [log4cl][7f8b], [log4cl-extras][691c], [openrpc-client][b8fd], [openrpc-server][c8e7], [serapeum][c41d], [with-user-abort][ad05]

[![](https://github-actions.40ants.com/40ants/40ants-openrpc/matrix.svg?only=ci.run-tests)][bc95]

![](http://quickdocs.org/badge/40ants-openrpc.svg)

<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :40ants-openrpc)
```
<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

This is a set of opionated helpers for starting Common Lisp `OpenRPC` servers which uses:

* https://40ants.com/openrpc/
* https://40ants.com/logging/
* https://40ants.com/slynk/

Also, it can manage a multiple Open`RPC` servers in the one Lisp image.

The easiest way to start a server is to define one or more api methods using `OpenRPC` library
and then call [`start`][999c]. This will bring `API` on http://localhost:8000/ and it's spec will be available
as http://localhost:8000/openrpc.json

This system uses following environment variables to configure the server:

* `APP_PORT` and `APP_INTERFACE` are used in [`start-in-production`][e921] function to control on which port and interface
  `API` should be started on.
* `DEBUG` also used in [`start-in-production`][e921] function to control how verbose logging should be. If it is given
  then logging will be with `DEBUG` level.
* `CORS_ALLOWED_ORIGIN` and `CORS_ALLOWED_HEADERS` are control how `API` will respond with `CORS` related headers.
  Learn more about used middleware in [`clack-cors`][5314] system documentation.

<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### API

<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-4040ANTS-OPENRPC-2FCLIENT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### 40ANTS-OPENRPC/CLIENT

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-2240ANTS-OPENRPC-2FCLIENT-22-29-20PACKAGE-29"></a>

##### [package](7ca2) `40ants-openrpc/client`

<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-7C-4040ANTS-OPENRPC-2FCLIENT-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Macros

<a id="x-2840ANTS-OPENRPC-2FCLIENT-3AGENERATE-CLIENT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

###### [macro](b16a) `40ants-openrpc/client:generate-client` name url

<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-4040ANTS-OPENRPC-2FJWT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### 40ANTS-OPENRPC/JWT

<a id="x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-2240ANTS-OPENRPC-2FJWT-22-29-20PACKAGE-29"></a>

##### [package](6b91) `40ants-openrpc/jwt`

<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-7C-4040ANTS-OPENRPC-2FJWT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-2840ANTS-OPENRPC-2FJWT-3ADECODE-20FUNCTION-29"></a>

###### [function](cc5f) `40ants-openrpc/jwt:decode` token

<a id="x-2840ANTS-OPENRPC-2FJWT-3AISSUE-TOKEN-20FUNCTION-29"></a>

###### [function](f6f5) `40ants-openrpc/jwt:issue-token` payload &key ttl

Encodes payload into a `JWT` token.

If `TTL` argument is given, it should be specified in seconds. After this number of seconds, token will become invalid.

<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-7C-4040ANTS-OPENRPC-2FJWT-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Macros

<a id="x-2840ANTS-OPENRPC-2FJWT-3AWITH-SESSION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

###### [macro](4b3a) `40ants-openrpc/jwt:with-session` ((&REST BINDINGS) &KEY (REQUIRE T) (PROCESSORS '(("roles" . ENSURE-LIST-OF-KEYWORDS)))) &BODY BODY

<a id="x-2840ANTS-OPENRPC-2FJWT-3AWITH-TEST-TOKEN-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

###### [macro](c35c) `40ants-openrpc/jwt:with-test-token` (token) &body body

<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-4040ANTS-OPENRPC-2FSERVER-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### 40ANTS-OPENRPC/SERVER

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-2240ANTS-OPENRPC-2FSERVER-22-29-20PACKAGE-29"></a>

##### [package](7f71) `40ants-openrpc/server`

<a id="x-2840ANTS-OPENRPC-DOCS-2FINDEX-3A-3A-7C-4040ANTS-OPENRPC-2FSERVER-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Functions

<a id="x-2840ANTS-OPENRPC-2FSERVER-3ASTART-20FUNCTION-29"></a>

###### [function](1b1f) `40ants-openrpc/server:start` &key (port \*default-port\*) (api openrpc-server/api::default-api) (interface \*default-interface\*) (debug nil)

Starts Open `RPC` `API` server on given `PORT` and `INTERFACE`.
Also it configures logging and Slynk.

Slynk is started only if `SLYNK`_`PORT` env variable is set to some value.
You will find more details in the [`40ants-slynk`][04ac] system documentation.

<a id="x-2840ANTS-OPENRPC-2FSERVER-3ASTART-IN-PRODUCTION-20FUNCTION-29"></a>

###### [function](26b0) `40ants-openrpc/server:start-in-production` &key (api openrpc-server/api::default-api)

Entry point for `API` webserver, started in the Docker or Kubernetes.
It works like a [`start`][999c] but blocks forever.

<a id="x-2840ANTS-OPENRPC-2FSERVER-3ASTOP-20FUNCTION-29"></a>

###### [function](b155) `40ants-openrpc/server:stop` &key (port \*default-port\*) (interface \*default-interface\*)

Stops `API` server running on given `PORT` and `INTERFACE`.


[e778]: https://40ants.com/40ants-openrpc/
[999c]: https://40ants.com/40ants-openrpc/#x-2840ANTS-OPENRPC-2FSERVER-3ASTART-20FUNCTION-29
[e921]: https://40ants.com/40ants-openrpc/#x-2840ANTS-OPENRPC-2FSERVER-3ASTART-IN-PRODUCTION-20FUNCTION-29
[5314]: https://40ants.com/clack-cors/#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-22clack-cors-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[04ac]: https://40ants.com/slynk/#x-28-23A-28-2812-29-20BASE-CHAR-20-2E-20-2240ants-slynk-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[8702]: https://github.com/40ants/40ants-openrpc
[bc95]: https://github.com/40ants/40ants-openrpc/actions
[7ca2]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/client.lisp#L1
[b16a]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/client.lisp#L36
[6b91]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/jwt.lisp#L1
[cc5f]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/jwt.lisp#L31
[f6f5]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/jwt.lisp#L35
[c35c]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/jwt.lisp#L62
[4b3a]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/jwt.lisp#L83
[7f71]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/server.lisp#L1
[26b0]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/server.lisp#L100
[1b1f]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/server.lisp#L45
[b155]: https://github.com/40ants/40ants-openrpc/blob/594739c82adc399e6256e8635ee1162c25474e6b/src/server.lisp#L90
[a84b]: https://github.com/40ants/40ants-openrpc/issues
[422a]: https://quickdocs.org/40ants-logging
[2e1d]: https://quickdocs.org/40ants-slynk
[8236]: https://quickdocs.org/alexandria
[fca9]: https://quickdocs.org/cl+ssl
[674d]: https://quickdocs.org/cl-json-web-tokens
[482d]: https://quickdocs.org/clack
[087e]: https://quickdocs.org/clack-cors
[babd]: https://quickdocs.org/clack-prometheus
[8347]: https://quickdocs.org/dexador
[a9bd]: https://quickdocs.org/jsonrpc
[6a02]: https://quickdocs.org/lack-request
[46a1]: https://quickdocs.org/local-time
[7f8b]: https://quickdocs.org/log4cl
[691c]: https://quickdocs.org/log4cl-extras
[b8fd]: https://quickdocs.org/openrpc-client
[c8e7]: https://quickdocs.org/openrpc-server
[c41d]: https://quickdocs.org/serapeum
[ad05]: https://quickdocs.org/with-user-abort

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]

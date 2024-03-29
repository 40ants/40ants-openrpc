<a id="x-2840ANTS-OPENRPC-DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-2840ANTS-OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0 (2023-11-17)

<a id="additions"></a>

### Additions

* Added [`40ants-openrpc/jwt:with-test-token`][e182] macro which allows to simulate "authenticated" requests using Lack test client.

<a id="x-2840ANTS-OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0 (2023-05-27)

<a id="changes"></a>

### Changes

* [`40ants-openrpc/server:start`][999c] function now uses [`openrpc-server/clack:app-middlewares`][a0d7] generic-function.

<a id="additions"></a>

### Additions

* Added [`40ants-openrpc/client:generate-client`][bc77] macro to generate client while storing remote spec in a local cache file.
* Added [`40ants-openrpc/jwt:with-session`][a8b1] macro to decode `JWT` token and extract user-id and other params out of it. Also this package provides [`40ants-openrpc/jwt:issue-token`][1315] function which is useful for logging user in.

<a id="x-2840ANTS-OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0 (2023-05-01)

* Initial version.


[bc77]: https://40ants.com/40ants-openrpc/#x-2840ANTS-OPENRPC-2FCLIENT-3AGENERATE-CLIENT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[1315]: https://40ants.com/40ants-openrpc/#x-2840ANTS-OPENRPC-2FJWT-3AISSUE-TOKEN-20FUNCTION-29
[a8b1]: https://40ants.com/40ants-openrpc/#x-2840ANTS-OPENRPC-2FJWT-3AWITH-SESSION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[e182]: https://40ants.com/40ants-openrpc/#x-2840ANTS-OPENRPC-2FJWT-3AWITH-TEST-TOKEN-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[999c]: https://40ants.com/40ants-openrpc/#x-2840ANTS-OPENRPC-2FSERVER-3ASTART-20FUNCTION-29
[a0d7]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FCLACK-3AAPP-MIDDLEWARES-20GENERIC-FUNCTION-29

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]

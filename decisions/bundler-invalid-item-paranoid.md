# Bundler Invalid Item Must Exercise HTTP

## Superseded Decision

The previous decision narrowed `dev_bundler:invalid_item_test_parallel/0` to a
direct device call so cache-write paranoia would not see an intentionally
invalid signed body before `bundler@1.0` handled it. Morning review rejected
that as reduced integration coverage.

Invalid signed input should be rejected by the real ingress path with a client
error, or by the bundler device when the request reaches it. The test must keep
both surfaces covered: the HTTP/server path and the direct device contract.

## Current Rule

Keep the invalid-item HTTP integration assertion. Do not weaken paranoid cache
verification and do not replace the integration path with only a direct device
call.

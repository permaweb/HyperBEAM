%%% @doc Shared definitions for launch digest computation modules.
%%%
%%% This header file contains the gctx record definition and common helper
%%% functions used across launch digest sub-modules.

%% Record for SEV-SNP launch digest context
-record(gctx, {ld = <<0:?LAUNCH_DIGEST_BITS>> :: binary()}).  % ld = launch digest (?LAUNCH_DIGEST_SIZE bytes)

%% Helper: Convert binary to hex string for logging
-define(BINARY_TO_HEX_STRING(Binary), hb_util:list(hb_util:to_hex(Binary))).


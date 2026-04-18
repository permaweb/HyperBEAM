%%% @doc Runs the `hb_message_test_vectors' battery against `~ipfs@1.0',
%%% declaring the handful of vectors that do not apply to a
%%% content-addressed, unsigned-only codec via `skip' on the opts entry —
%%% so the IPFS-specific skip list lives with the IPFS device instead of
%%% inside the generic test-vector module.
-module(dev_codec_ipfs_message_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Run the message test-vector battery for `~ipfs@1.0'.
suite_test_() ->
    hb_test_utils:suite_with_opts(
        hb_message_test_vectors:codec_test_suite([<<"ipfs@1.0">>]),
        opts()).

%% @doc Opts shaped for `hb_test_utils:suite_with_opts/2'. The `skip' list
%% names the vectors that don't apply to `~ipfs@1.0' — each with a reason.
opts() ->
    [#{
        name     => ipfs,
        parallel => true,
        desc     => <<"ipfs@1.0">>,
        opts     => #{
            store       => hb_test_utils:test_store(),
            priv_wallet => hb:wallet()
        },
        skip => [
            %% `atom' has no IPLD representation beyond null/true/false,
            %% so non-null/true/false atoms throw on encode.
            <<"Structured field atom parsing">>,
            %% `~ipfs@1.0' is unsigned-only (content-addressed); the
            %% node-message signing path requires a signed commitment.
            <<"Sign node message">>,
            %% `priv' is session-only state and is stripped by `to/3' —
            %% it must never cross the content-addressed boundary.
            <<"Priv survives conversion">>,
            %% `{link, CID}' flattens to the CID string in phase 2. A
            %% link-aware mapping through `hb_link' is the next phase.
            <<"ID of linked message">>
        ]
    }].

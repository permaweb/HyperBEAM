%%% @doc Deterministic admission vectors for full Arweave block validation.
%%%
%%% A live, full account transition is provided by
%%% `dev_arweave_sync_test_vectors:live_account_transition/0'. It hydrates a
%%% recent checkpoint because public peers prune historical wallet lists.
-module(dev_arweave_block_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Mainnet blocks 1132210-1132214, the first five above the 2.6 fork, as
%%% `{Height, RewardAddr, Reward, Denomination, RewardHistoryHash}'. Below the
%%% 2.8 fork the committed hash covers the whole locked window, which at these
%%% heights is the whole history, so these are the only blocks on the chain
%%% whose `reward-history-hash' proves a representation of the history rather
%%% than of its newest element.
-define(REWARD_HISTORY_VECTOR, [
    {1132210, <<"SOtIrcaiJwVs4h3yqXKjOs0P9bNJG5F77y0-TilnQ2U">>,
        1465707980036, 1, <<"I190QrZCm05s9aIbt9j6bjFJn1BkNvZVMPEXJ7QFTtI">>},
    {1132211, <<"RTot9yL_q-RIsObxmHsZ4ApyVyADn5VxPJC7KefjNHM">>,
        1464501084774, 1, <<"NjA1DoZCYBzrowI9okx_yxt7tXFWD6XpVAtf2VjO81E">>},
    {1132212, <<"UW8wkuhbnjYkT1fqan6sGNiWT_kYULCdr7hqUhEWcGg">>,
        1466463708872, 1, <<"5gBXh5FEEj82filFyfv41h2LLeNo_b7YXfHj1kLdTvs">>},
    {1132213, <<"RTot9yL_q-RIsObxmHsZ4ApyVyADn5VxPJC7KefjNHM">>,
        1464450490014, 1, <<"1Y_BTWQEVZy-QmdrPIYk-9BhPhEYU6xJdzeWw5RUFvo">>},
    {1132214, <<"I03LhhAocpOu4FpH7f0mJ55CfK3cSnu1JJZtITQ4sA8">>,
        1464446627463, 1, <<"bpTKBba9IyNy-vAtj_gFoZRvjlop1h9XwkV44rY8pqQ">>}
]).

%%% The difficulty all five of those blocks were mined at, which is what
%%% `ar_difficulty:get_hash_rate_fixed_ratio/1' derives the hash rate their
%%% reward-history element carries from.
-define(REWARD_HISTORY_VECTOR_DIFF,
    115792084401224566807772651152730092358388734887252574358647557874619658061534
).

%%% Mainnet blocks 1275479-1275483: the 2.7 fork block's parent and the first
%%% four blocks the block-time history covers, as
%%% `{Height, Timestamp, GlobalStepNumber, BlockTimeHistoryHash}'. The parent
%%% carries no hash -- it is below the fork and contributes no element -- but
%%% its timestamp and step number are what the first element is measured
%%% against.
-define(BLOCK_TIME_HISTORY_VECTOR, [
    {1275479, 1696513946, 20992452, none},
    {1275480, 1696513952, 20992460,
        <<"VWw6aNPou6Nj6WImgaYNcYEKyc7SCBCTUwrwt_wxrK8">>},
    {1275481, 1696514212, 20992706,
        <<"L7q3syXwKAXNNisnFNocZkX-6CP-WXonxcA9Foc2olI">>},
    {1275482, 1696514242, 20992737,
        <<"Ryy0lEcLZtViBMuOuOHcuoOBXMK0aqU2USVqG_20ZBc">>},
    {1275483, 1696514280, 20992766,
        <<"zbbfsrF24QhcrbSewNnbLilvdODTPFUBbYo84OlTwbE">>}
]).

%% @doc A wide range carries the standard 10,800-step suffix without being
%% rejected merely because the parent is farther away.
wide_step_range_test() ->
    Output = <<0:256>>,
    Steps = lists:duplicate(10800, Output),
    Prev =
        #block{
            nonce_limiter_info =
                #nonce_limiter_info{
                    global_step_number = 1,
                    output = Output
                }
        },
    Next =
        #block{
            nonce_limiter_info =
                #nonce_limiter_info{
                    global_step_number = 10802,
                    prev_output = Output,
                    steps = Steps
                }
        },
    ?assertEqual(ok, dev_arweave_block:check_step_number(Next, Prev)),
    NextInfo = Next#block.nonce_limiter_info,
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-number">> }},
        dev_arweave_block:check_step_number(
            Next#block{
                nonce_limiter_info =
                    NextInfo#nonce_limiter_info{ steps = tl(Steps) }
            },
            Prev
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-number">> }},
        dev_arweave_block:check_step_number(
            Next#block{
                nonce_limiter_info =
                    NextInfo#nonce_limiter_info{ steps = [Output | Steps] }
            },
            Prev
        )
    ).

%% @doc A VDF answer is accepted only when it carries the atom `true'.
vdf_fails_closed_test() ->
    Refused =
        fun(Answer) ->
            case dev_arweave_block:holds(
                maps:get(<<"valid">>, Answer, false) =:= true,
                <<"invalid-vdf-chain">>,
                <<"detail">>
            ) of
                ok -> accepted;
                {error, Error} -> maps:get(<<"message">>, Error)
            end
        end,
    ?assertEqual(accepted, Refused(#{ <<"valid">> => true })),
    ?assertEqual(
        <<"invalid-vdf-chain">>,
        Refused(#{ <<"valid">> => false })
    ),
    ?assertEqual(
        <<"invalid-vdf-chain">>,
        Refused(#{ <<"valid">> => <<"true">> })
    ),
    ?assertEqual(<<"invalid-vdf-chain">>, Refused(#{})).

%% @doc The reward history hash mainnet committed to, recomputed by the check
%% itself over a history read back out of the persistent list.
%%
%% Five blocks, so the check runs against histories of none, one, two, three and
%% four elements, and every one of them is walked in whole. The state each block
%% is checked against is the one the block before it produced, so what is proven
%% is the pair: what `next_reward_history/3' writes is what `reward_history/2'
%% reads back, and what it reads back is what mainnet signed.
reward_history_hash_matches_mainnet_test() ->
    Opts = test_opts(),
    lists:foldl(
        fun(Row, {Head, Previous}) ->
            Next = reward_history_block(Row),
            State = #{ <<"reward-history">> => Head },
            ?assertEqual(
                ok,
                dev_arweave_block:check_reward_history_hash(
                    Next,
                    #block{
                        height = Next#block.height - 1,
                        reward_history_hash = Previous,
                        reward_history = reward_history(State, Opts)
                    }
                )
            ),
            {
                lib_arweave_state:next_reward_history(State, Next, Opts),
                Next#block.reward_history_hash
            }
        end,
        {[], <<>>},
        ?REWARD_HISTORY_VECTOR
    ).

%% @doc The block-time history hash mainnet committed to, recomputed by the
%% check itself over a history read back out of the persistent list.
%%
%% The elements are not stated: they are measured from the blocks' own
%% timestamps and step numbers by the vendored rule, so what the list holds is
%% what mainnet hashed rather than a transcription of it.
block_time_history_hash_matches_mainnet_test() ->
    Opts = test_opts(),
    [Parent | Blocks] = ?BLOCK_TIME_HISTORY_VECTOR,
    lists:foldl(
        fun(Row, {Head, Previous}) ->
            Next = block_time_history_block(Row),
            State = #{ <<"block-time-history">> => Head },
            Prev =
                Previous#block{
                    block_time_history = block_time_history(State, Opts)
                },
            ?assertEqual(
                ok,
                dev_arweave_block:check_block_time_history_hash(Next, Prev)
            ),
            {
                lib_arweave_state:next_block_time_history(
                    State, Next, Prev, Opts),
                Next
            }
        end,
        {[], block_time_history_block(Parent)},
        Blocks
    ).

%% @doc Malformed wire bytes are reported as a codec error rather than
%% escaping as an exception.
rejects_corrupt_binary_test() ->
    Opts = test_opts(),
    {error, Error} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-block@2.9">>,
                <<"body">> => <<0:512>>
            },
            <<"from-binary">>,
            Opts
        ),
    ?assertEqual(
        <<"invalid-block-encoding">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%%% Test helpers.

%% @doc A store of this vector's own, so that what one vector writes cannot be
%% read by another.
test_opts() ->
    #{ <<"store">> => [hb_test_utils:test_store()] }.

%% @doc Read the reward history a state carries, from the entries in the store.
reward_history(State, Opts) ->
    read(fun() -> lib_arweave_state:reward_history(State, Opts) end).

%% @doc Read the block-time history a state carries, from the entries in the
%% store.
block_time_history(State, Opts) ->
    read(fun() -> lib_arweave_state:block_time_history(State, Opts) end).

%% @doc Read a history in a process of its own, so that what answers is the
%% entries in the store rather than the window the process that wrote them
%% memoised. A node reads a chain state back in whichever process is applying
%% blocks, which is never the one that built the entries, so this is the read
%% these vectors are about.
read(Fun) ->
    Caller = self(),
    Ref = make_ref(),
    spawn(fun() -> Caller ! {Ref, Fun()} end),
    receive {Ref, Values} -> Values after 10000 -> error(read_timeout) end.

%% @doc The block a reward-history vector row stands for, carrying the fields
%% `ar_rewards:add_element/2' reads its element out of.
reward_history_block({Height, Address, Reward, Denomination, Hash}) ->
    #block{
        height = Height,
        diff = ?REWARD_HISTORY_VECTOR_DIFF,
        reward_addr = hb_util:decode(Address),
        reward = Reward,
        denomination = Denomination,
        reward_history_hash = hb_util:decode(Hash)
    }.

%% @doc The block a block-time-history vector row stands for, carrying the
%% fields `ar_block_time_history:update_history/2' measures its element from.
block_time_history_block({Height, Timestamp, Steps, none}) ->
    #block{
        height = Height,
        timestamp = Timestamp,
        nonce_limiter_info = #nonce_limiter_info{ global_step_number = Steps }
    };
block_time_history_block({Height, Timestamp, Steps, Hash}) ->
    Block = block_time_history_block({Height, Timestamp, Steps, none}),
    Block#block{ block_time_history_hash = hb_util:decode(Hash) }.

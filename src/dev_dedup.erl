%%% @doc A device that deduplicates messages in an evaluation stream, returning
%%% status `skip' if the message has already been seen.
%%%
%%% This device is typically used to ensure that a message is only executed
%%% once, even if assigned multiple times, upon a `~process@1.0' evaluation.
%%% It can, however, be used in many other contexts.
%%%
%%% This device honors the `pass' key if it is present in the message. If so,
%%% it will only run on the first pass. Additionally, the device supports
%%% a `subject-key' key that allows the caller to specify the key whose ID
%%% should be used for deduplication. If the `subject-key' key is not present,
%%% the device will use the `body' of the request as the subject. If the key is
%%% set to `request', the device will use the entire request itself as the
%%% subject.
%%%
%%% This device runs on the first pass of the `compute' key call if executed
%%% in a stack, and not in subsequent passes. Currently the device stores its
%%% list of already seen items in memory, but at some point it will likely make
%%% sense to drop them in the cache.
-module(dev_dedup).
-export([info/1]).
-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").


info(_M1) ->
    #{
      default => fun handle/4,
      exclude => [keys, set, id, commit]
     }.


%% @doc Forward the keys and `set' functions to the message device, handle all
%% others with deduplication. This allows the device to be used in any context
%% where a key is called. If the `dedup-key
handle(<<"keys">>, M1, _M2, _Opts) ->
    dev_message:keys(M1);
handle(<<"set">>, M1, M2, Opts) ->
    dev_message:set(M1, M2, Opts);
handle(Key, M1, M2, Opts) ->
    ?event({dedup_handle, {key, Key}, {msg1, M1}, {msg2, M2}}),
    % Find the relevant parameters from the messages. We search for the
    % `dedup-key' key in the first message, and use that value as the key to
    % look for in the second message.
    SubjectKey =
        hb_ao:get_first(
          [{{as, <<"message@1.0">>, M1}, <<"dedup-subject">>},
           {{as, <<"message@1.0">>, M2}, <<"dedup-subject">>}],
          <<"body">>,
          Opts),
    % Get the subject of the second message.
    Subject =
        if
            SubjectKey == <<"request">> ->
                % The subject is the request itself.
                M2;
            true ->
                % The subject is the value of the subject key, which will have
                % defaulted to the `body' key if not set in the base message.
                hb_ao:get_first(
                  [{{as, <<"message@1.0">>, M1}, SubjectKey},
                   {{as, <<"message@1.0">>, M2}, SubjectKey}],
                  Opts)
        end,
    % Is this the first pass, if we are executing in a stack?
    FirstPass = hb_ao:get(<<"pass">>, {as, <<"message@1.0">>, M1}, 1, Opts) == 1,
    % Get the list of already seen subjects.
    DedupList = hb_ao:get(<<"dedup">>, {as, <<"message@1.0">>, M1}, [], Opts),
    ?event({dedup_handle,
            {key, Key},
            {msg1, M1},
            {msg2, M2},
            {subject_key, SubjectKey},
            {subject, Subject}}),
    case {FirstPass, Subject} of
        {false, _} ->
            % If this is not the first pass, we can skip the deduplication
            % check.
            {ok, M1};
        {true, not_found} ->
            % If the subject key is not present, we can skip the deduplication
            % check.
            {ok, M1};
        {true, _} ->
            % If this is the first pass, we need to check if the subject has
            % already been seen.
            SubjectID = hb_message:id(Subject, all),
            ?event({dedup_checking, {existing, DedupList}}),
            case lists:member(SubjectID, DedupList) of
                true ->
                    ?event({already_seen, SubjectID}),
                    {skip, M1};
                false ->
                    ?event({not_seen, SubjectID}),
                    M3 =
                        hb_ao:set(
                          M1,
                          #{<<"dedup">> => [SubjectID | DedupList]},
                          Opts),
                    ?event({dedup_updated, M3}),
                    {ok, M3}
            end
    end.


%%% Tests


dedup_test() ->
    hb:init(),
    % Create a stack with a dedup device and 2 devices that will append to a
    % `Result' key.
    Msg = #{
            <<"device">> => <<"Stack@1.0">>,
            <<"dedup-subject">> => <<"request">>,
            <<"device-stack">> =>
                #{
                  <<"1">> => <<"dedup@1.0">>,
                  <<"2">> => dev_stack:generate_append_device(<<"+D2">>),
                  <<"3">> => dev_stack:generate_append_device(<<"+D3">>)
                 },
            <<"result">> => <<"INIT">>
           },
    % Send the same message twice, with the same binary.
    {ok, Msg2} = hb_ao:resolve(Msg,
                               #{<<"path">> => <<"append">>, <<"bin">> => <<"_">>},
                               #{}),
    {ok, Msg3} = hb_ao:resolve(Msg2,
                               #{<<"path">> => <<"append">>, <<"bin">> => <<"_">>},
                               #{}),
    % Send the same message twice, with another binary.
    {ok, Msg4} = hb_ao:resolve(Msg3,
                               #{<<"path">> => <<"append">>, <<"bin">> => <<"/">>},
                               #{}),
    {ok, Msg5} = hb_ao:resolve(Msg4,
                               #{<<"path">> => <<"append">>, <<"bin">> => <<"/">>},
                               #{}),
    % Ensure that downstream devices have only seen each message once.
    ?assertMatch(
      #{<<"result">> := <<"INIT+D2_+D3_+D2/+D3/">>},
      Msg5).


dedup_with_multipass_test() ->
    % Create a stack with a dedup device and 2 devices that will append to a
    % `Result' key and a `Multipass' device that will repeat the message for
    % an additional pass. We want to ensure that Multipass is not hindered by
    % the dedup device.
    Msg = #{
            <<"device">> => <<"Stack@1.0">>,
            <<"dedup-subject">> => <<"request">>,
            <<"device-stack">> =>
                #{
                  <<"1">> => <<"Dedup@1.0">>,
                  <<"2">> => dev_stack:generate_append_device(<<"+D2">>),
                  <<"3">> => dev_stack:generate_append_device(<<"+D3">>),
                  <<"4">> => <<"Multipass@1.0">>
                 },
            <<"result">> => <<"INIT">>,
            <<"passes">> => 2
           },
    % Send the same message twice, with the same binary.
    {ok, Msg2} = hb_ao:resolve(Msg, #{<<"path">> => <<"append">>, <<"bin">> => <<"_">>}, #{}),
    {ok, Msg3} = hb_ao:resolve(Msg2, #{<<"path">> => <<"append">>, <<"bin">> => <<"_">>}, #{}),
    % Send the same message twice, with another binary.
    {ok, Msg4} = hb_ao:resolve(Msg3, #{<<"path">> => <<"append">>, <<"bin">> => <<"/">>}, #{}),
    {ok, Msg5} = hb_ao:resolve(Msg4, #{<<"path">> => <<"append">>, <<"bin">> => <<"/">>}, #{}),
    % Ensure that downstream devices have only seen each message once.
    ?assertMatch(
      #{<<"result">> := <<"INIT+D2_+D3_+D2_+D3_+D2/+D3/+D2/+D3/">>},
      Msg5).

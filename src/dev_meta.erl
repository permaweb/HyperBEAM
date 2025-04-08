-module(dev_meta).
-moduledoc """
The hyperbeam meta device, which is the default entry point
for all messages processed by the machine. This device executes a
AO-Core singleton request, after first applying the node's 
pre-processor, if set. The pre-processor can halt the request by
returning an error, or return a modified version if it deems necessary --
the result of the pre-processor is used as the request for the AO-Core
resolver. Additionally, a post-processor can be set, which is executed after
the AO-Core resolver has returned a result.
""".
-export([info/1, info/3, handle/2, adopt_node_message/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-doc """
Ensure that the helper function `adopt_node_message/2' is not exported.
The naming of this method carefully avoids a clash with the exported `info/3'
function. We would like the node information to be easily accessible via the
`info' endpoint, but AO-Core also uses `info' as the name of the function
that grants device information. The device call takes two or fewer arguments,
so we are safe to use the name for both purposes in this case, as the user 
info call will match the three-argument version of the function. If in the 
future the `request' is added as an argument to AO-Core's internal `info'
function, we will need to find a different approach.
""".
info(_) -> #{ exports => [info] }.

-doc """
Normalize and route messages downstream based on their path. Messages
with a `Meta' key are routed to the `handle_meta/2' function, while all
other messages are routed to the `handle_resolve/2' function.
""".
handle(NodeMsg, RawRequest) ->
    ?event({singleton_tabm_request, RawRequest}),
    NormRequest = hb_singleton:from(RawRequest),
    ?event(http, {request, hb_ao:normalize_keys(NormRequest)}),
    case hb_opts:get(initialized, false, NodeMsg) of
        false ->
            Res =
                embed_status(
                    hb_ao:force_message(
                        handle_initialize(NormRequest, NodeMsg),
                        NodeMsg
                    )
                ),
            Res;
        _ -> handle_resolve(RawRequest, NormRequest, NodeMsg)
    end.

handle_initialize([Base = #{ <<"device">> := Device}, Req = #{ <<"path">> := Path }|_], NodeMsg) ->
    ?event({got, {device, Device}, {path, Path}}),
    case {Device, Path} of
        {<<"meta@1.0">>, <<"info">>} -> info(Base, Req, NodeMsg);
        _ -> {error, <<"Node must be initialized before use.">>}
    end;
handle_initialize([{as, <<"meta@1.0">>, _}|Rest], NodeMsg) ->
    handle_initialize([#{ <<"device">> => <<"meta@1.0">>}|Rest], NodeMsg);
handle_initialize([_|Rest], NodeMsg) ->
    handle_initialize(Rest, NodeMsg);
handle_initialize([], _NodeMsg) ->
    {error, <<"Node must be initialized before use.">>}.

-doc """
Get/set the node message. If the request is a `POST', we check that the
request is signed by the owner of the node. If not, we return the node message
as-is, aside all keys that are private (according to `hb_private').
""".
info(_, Request, NodeMsg) ->
    case hb_ao:get(<<"method">>, Request, NodeMsg) of
        <<"GET">> ->
            ?event({get_config_req, Request, NodeMsg}),
			DynamicKeys = add_dynamic_keys(NodeMsg),	
			?event(green_zone, {get_config, DynamicKeys}),
            embed_status({ok, filter_node_msg(DynamicKeys)});
        <<"POST">> ->
            case hb_ao:get(<<"initialized">>, NodeMsg, not_found, NodeMsg) of
                permanent ->
                    embed_status(
                        {error,
                            <<"The node message of this machine is already "
                                "permanent. It cannot be changed.">>
                        }
                    );
                _ ->
                    update_node_message(Request, NodeMsg)
            end;
        _ -> embed_status({error, <<"Unsupported Meta/info method.">>})
    end.

-doc """
Remove items from the node message that are not encodable into a
message.
""".
filter_node_msg(Msg) when is_map(Msg) ->
    maps:map(fun(_, Value) -> filter_node_msg(Value) end, hb_private:reset(Msg));
filter_node_msg(Msg) when is_list(Msg) ->
    lists:map(fun filter_node_msg/1, Msg);
filter_node_msg(Tuple) when is_tuple(Tuple) ->
    <<"Unencodable value.">>;
filter_node_msg(Other) ->
    Other.

-doc """
Add dynamic keys to the node message.
""".
add_dynamic_keys(NodeMsg) ->
    case hb_opts:get(priv_wallet, no_viable_wallet, NodeMsg) of
        no_viable_wallet ->
            NodeMsg;
        Wallet ->
            %% Create a new map with address and merge it (overwriting existing)
			Address = hb_util:id(ar_wallet:to_address(Wallet)),
            NodeMsg#{ address => Address, <<"address">> => Address }
    end.

-doc """
Validate that the request is signed by the operator of the node, then
allow them to update the node message.
""".
update_node_message(Request, NodeMsg) ->
    RequestSigners = hb_message:signers(Request),
    Operator =
        hb_opts:get(
            operator,
            case hb_opts:get(priv_wallet, no_viable_wallet, NodeMsg) of
                no_viable_wallet -> unclaimed;
                Wallet -> ar_wallet:to_address(Wallet)
            end,
            NodeMsg
        ),
    EncOperator =
        case Operator of
            unclaimed -> unclaimed;
            NativeAddress -> hb_util:human_id(NativeAddress)
        end,
    case EncOperator == unclaimed orelse lists:member(EncOperator, RequestSigners) of
        false ->
            ?event({set_node_message_fail, Request}),
            embed_status({error, <<"Unauthorized">>});
        true ->
            case adopt_node_message(Request, NodeMsg) of
                {ok, NewNodeMsg} ->
                    NewH = hb_opts:get(node_history, [], NewNodeMsg),
                    embed_status(
                        {ok,
                            #{
                                <<"body">> =>
                                    iolist_to_binary(
                                        io_lib:format(
                                            "Node message updated. History: ~p updates.",
                                            [length(NewH)]
                                        )
                                    ),
                                <<"history-length">> => length(NewH)
                            }
                        }
                    );
                {error, Reason} ->
                    ?event({set_node_message_fail, Request, Reason}),
                    embed_status({error, Reason})
            end
    end.

-doc """
Attempt to adopt changes to a node message.
""".
adopt_node_message(Request, NodeMsg) ->
    ?event({set_node_message_success, Request}),
    MergedOpts =
        maps:merge(
            NodeMsg,
            hb_opts:mimic_default_types(hb_message:uncommitted(Request), new_atoms)
        ),
    % Ensure that the node history is updated and the http_server ID is
    % not overridden.
    case hb_opts:get(initialized, permanent, NodeMsg) of
        permanent ->
            {error, <<"Node message is already permanent.">>};
        _ ->
            hb_http_server:set_opts(
                MergedOpts#{
                    http_server => hb_opts:get(http_server, no_server, NodeMsg),
                    node_history => [Request|hb_opts:get(node_history, [], NodeMsg)]
                }
            ),
            {ok, MergedOpts}
    end.

-doc """
Handle an AO-Core request, which is a list of messages. We apply
the node's pre-processor to the request first, and then resolve the request
using the node's AO-Core implementation if its response was `ok'.
After execution, we run the node's `postprocessor' message on the result of
the request before returning the result it grants back to the user.
""".
handle_resolve(Req, Msgs, NodeMsg) ->
    % Apply the pre-processor to the request.
    case resolve_processor(<<"preprocess">>, preprocessor, Req, Msgs, NodeMsg) of
        {ok, PreProcessedMsg} ->
            ?event(
                {result_after_preprocessing,
                    hb_ao:normalize_keys(PreProcessedMsg)}
            ),
            AfterPreprocOpts = hb_http_server:get_opts(NodeMsg),
            % Resolve the request message.
            HTTPOpts = maps:merge(
                AfterPreprocOpts,
                hb_opts:get(http_extra_opts, #{}, NodeMsg)
            ),
            Res =
                try
                    hb_ao:resolve_many(
                        PreProcessedMsg,
                        HTTPOpts#{ force_message => true }
                    )
                catch
                    throw:{necessary_message_not_found, MsgID} ->
                        ID = hb_util:human_id(MsgID),
                        {error, #{
                            <<"status">> => 404,
                            <<"unavilable">> => ID,
                            <<"body">> =>
                                <<"Message necessary to resolve request not found: ",
                                    ID/binary>>
                        }}
                end,
            {ok, StatusEmbeddedRes} =
                embed_status(
                    Res
                ),
            ?event({res, StatusEmbeddedRes}),
            AfterResolveOpts = hb_http_server:get_opts(NodeMsg),
            % Apply the post-processor to the result.
            Output = maybe_sign(
                embed_status(
                    resolve_processor(
                        <<"postprocess">>,
                        postprocessor,
                        Req,
                        StatusEmbeddedRes,
                        AfterResolveOpts
                    )
                ),
                NodeMsg
            ),
            ?event(http, {response, Output}),
            Output;
        Res -> embed_status(hb_ao:force_message(Res, NodeMsg))
    end.

-doc """
Execute a message from the node message upon the user's request. The
invocation of the processor provides a request of the following form:
```
     /path => preprocess | postprocess
     /request => the original request singleton
     /body => list of messages the user wishes to process
'''
""".
resolve_processor(PathKey, Processor, Req, Query, NodeMsg) ->
    case hb_opts:get(Processor, undefined, NodeMsg) of
        undefined -> {ok, Query};
        ProcessorMsg ->
            ?event(processor, {processor_resolving, PathKey, ProcessorMsg}),
            Res = hb_ao:resolve(
                ProcessorMsg,
                #{
                    <<"path">> => PathKey,
                    <<"body">> => Query,
                    <<"request">> => Req
                },
                NodeMsg#{ hashpath => ignore }
            ),
            ?event(processor, {processor_result, {type, PathKey}, {res, Res}}),
            Res
    end.

-doc """
Wrap the result of a device call in a status.
""".
embed_status({ErlStatus, Res}) when is_map(Res) ->
    case lists:member(<<"status">>, hb_message:committed(Res)) of
        false ->
            HTTPCode = status_code({ErlStatus, Res}),
            {ok, Res#{ <<"status">> => HTTPCode }};
        true ->
            {ok, Res}
    end;
embed_status({ErlStatus, Res}) ->
    HTTPCode = status_code({ErlStatus, Res}),
    {ok, #{ <<"status">> => HTTPCode, <<"body">> => Res }}.

-doc """
Calculate the appropriate HTTP status code for an AO-Core result.
The order of precedence is:
1. The status code from the message.
2. The HTTP representation of the status code.
3. The default status code.
""".
status_code({ErlStatus, Msg}) ->
    case message_to_status(Msg) of
        default -> status_code(ErlStatus);
        RawStatus -> RawStatus
    end;
status_code(ok) -> 200;
status_code(error) -> 400;
status_code(created) -> 201;
status_code(not_found) -> 404;
status_code(unavailable) -> 503.

-doc """
Get the HTTP status code from a transaction (if it exists).
""".
message_to_status(#{ <<"body">> := Status }) when is_atom(Status) ->
    status_code(Status);
message_to_status(Item) when is_map(Item) ->
    % Note: We use `dev_message' directly here, such that we do not cause 
    % additional AO-Core calls for every request. This is particularly important
    % if a remote server is being used for all AO-Core requests by a node.
    case dev_message:get(<<"status">>, Item) of
        {ok, RawStatus} when is_integer(RawStatus) -> RawStatus;
        {ok, RawStatus} when is_atom(RawStatus) -> status_code(RawStatus);
        {ok, RawStatus} -> binary_to_integer(RawStatus);
        _ -> default
    end;
message_to_status(Item) when is_atom(Item) ->
    status_code(Item);
message_to_status(_Item) ->
    default.

-doc """
Sign the result of a device call if the node is configured to do so.
""".
maybe_sign({Status, Res}, NodeMsg) ->
    {Status, maybe_sign(Res, NodeMsg)};
maybe_sign(Res, NodeMsg) ->
    ?event({maybe_sign, Res, NodeMsg}),
    case hb_opts:get(force_signed, false, NodeMsg) of
        true ->
            case hb_message:signers(Res) of
                [] -> hb_message:commit(Res, NodeMsg);
                _ -> Res
            end;
        false -> Res
    end.

%%% Tests

-doc """
Test that we can get the node message.
""".
config_test() ->
    Node = hb_http_server:start_node(#{ test_config_item => <<"test">> }),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test">>, hb_ao:get(<<"test_config_item">>, Res, #{})).

-doc """
Test that we can't get the node message if the requested key is private.
""".
priv_inaccessible_test() ->
    Node = hb_http_server:start_node(
        #{
            test_config_item => <<"test">>,
            priv_key => <<"BAD">>
        }
    ),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test">>, hb_ao:get(<<"test_config_item">>, Res, #{})),
    ?assertEqual(not_found, hb_ao:get(<<"priv_key">>, Res, #{})).

-doc """
Test that we can't set the node message if the request is not signed by
the owner of the node.
""".
unauthorized_set_node_msg_fails_test() ->
    Node = hb_http_server:start_node(#{ priv_wallet => ar_wallet:new() }),
    {error, _} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"evil_config_item">> => <<"BAD">>
                },
                ar_wallet:new()
            ),
            #{}
        ),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?assertEqual(not_found, hb_ao:get(<<"evil_config_item">>, Res, #{})),
    ?assertEqual(0, length(hb_ao:get(<<"node_history">>, Res, [], #{}))).

-doc """
Test that we can set the node message if the request is signed by the
owner of the node.
""".
authorized_set_node_msg_succeeds_test() ->
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_node(
        #{
            operator => hb_util:human_id(ar_wallet:to_address(Owner)),
            test_config_item => <<"test">>
        }
    ),
    {ok, SetRes} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"test_config_item">> => <<"test2">>
                },
                Owner
            ),
            #{}
        ),
    ?event({res, SetRes}),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test2">>, hb_ao:get(<<"test_config_item">>, Res, #{})),
    ?assertEqual(1, length(hb_ao:get(<<"node_history">>, Res, [], #{}))).

-doc """
Test that an uninitialized node will not run computation.
""".
uninitialized_node_test() ->
    Node = hb_http_server:start_node(#{ initialized => false }),
    {error, Res} = hb_http:get(Node, <<"/key1?1.key1=value1">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"Node must be initialized before use.">>, Res).

-doc """
Test that a permanent node message cannot be changed.
""".
permanent_node_message_test() ->
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_node(
        #{
            operator => unclaimed,
            initialized => false,
            test_config_item => <<"test">>
        }
    ),
    {ok, SetRes1} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"test_config_item">> => <<"test2">>,
                    initialized => <<"permanent">>
                },
                Owner
            ),
            #{}
        ),
    ?event({set_res, SetRes1}),
    {ok, Res} = hb_http:get(Node, #{ <<"path">> => <<"/~meta@1.0/info">> }, #{}),
    ?event({get_res, Res}),
    ?assertEqual(<<"test2">>, hb_ao:get(<<"test_config_item">>, Res, #{})),
    {error, SetRes2} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"test_config_item">> => <<"bad_value">>
                },
                Owner
            ),
            #{}
        ),
    ?event({set_res, SetRes2}),
    {ok, Res2} = hb_http:get(Node, #{ <<"path">> => <<"/~meta@1.0/info">> }, #{}),
    ?event({get_res, Res2}),
    ?assertEqual(<<"test2">>, hb_ao:get(<<"test_config_item">>, Res2, #{})),
    ?assertEqual(1, length(hb_ao:get(<<"node_history">>, Res2, [], #{}))).

-doc """
Test that we can claim the node correctly and set the node message after.
""".
claim_node_test() ->
    Owner = ar_wallet:new(),
    Address = ar_wallet:to_address(Owner),
    Node = hb_http_server:start_node(
        #{
            operator => unclaimed,
            test_config_item => <<"test">>
        }
    ),
    {ok, SetRes} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"operator">> => hb_util:human_id(Address)
                },
                Owner
            ),
            #{}
        ),
    ?event({res, SetRes}),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(hb_util:human_id(Address), hb_ao:get(<<"operator">>, Res, #{})),
    {ok, SetRes2} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"test_config_item">> => <<"test2">>
                },
                Owner
            ),
            #{}
        ),
    ?event({res, SetRes2}),
    {ok, Res2} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res2}),
    ?assertEqual(<<"test2">>, hb_ao:get(<<"test_config_item">>, Res2, #{})),
    ?assertEqual(2, length(hb_ao:get(<<"node_history">>, Res2, [], #{}))).

%% Test that we can use a preprocessor upon a request.
% preprocessor_test() ->
%     Parent = self(),
%     Node = hb_http_server:start_node(
%         #{
%             preprocessor =>
%                 #{
%                     <<"device">> => #{
%                         <<"preprocess">> =>
%                             fun(_, #{ <<"body">> := Msgs }, _) ->
%                                 Parent ! ok,
%                                 {ok, Msgs}
%                             end
%                     }
%                 }
%         }),
%     hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
%     ?assert(receive ok -> true after 1000 -> false end).

-doc """
Test that we can halt a request if the preprocessor returns an error.
""".
halt_request_test() ->
    Node = hb_http_server:start_node(
        #{
            preprocessor =>
                #{
                    <<"device">> => #{
                        <<"preprocess">> =>
                            fun(_, _, _) ->
                                {error, <<"Bad">>}
                            end
                    }
                }
        }),
    {error, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?assertEqual(<<"Bad">>, Res).

-doc """
Test that a preprocessor can modify a request.
""".
modify_request_test() ->
    Node = hb_http_server:start_node(
        #{
            preprocessor =>
                #{
                    <<"device">> => #{
                        <<"preprocess">> =>
                            fun(_, #{ <<"body">> := [M|Ms] }, _) ->
                                {ok, [M#{ <<"added">> => <<"value">> }|Ms]}
                            end
                    }
                }
        }),
    {ok, Res} = hb_http:get(Node, <<"/added">>, #{}),
    ?assertEqual(<<"value">>, Res).

%% Test that we can use a postprocessor upon a request. Calls the `test@1.0'
%% device's postprocessor, which sets the `postprocessor-called' key to true in
%% the HTTP server.
% postprocessor_test() ->
%     Node = hb_http_server:start_node(
%         #{
%             postprocessor => <<"test-device@1.0">>
%         }),
%     hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
%     timer:sleep(100),
%     {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info/postprocessor-called">>, #{}),
%     ?event({res, Res}),
%     ?assertEqual(true, Res).
%%% @doc Kernel hook execution for HyperBEAM nodes.
%%%
%%% Hooks are stored in the node message under `on'. Each hook name maps to one
%%% handler message or a list of handler messages. When a hook fires, its request
%%% is evaluated against each handler in sequence. The output of one handler is
%%% passed to the next.
-module(hb_hook).
-export([on/3, find/2, find/3]).
-include("include/hb.hrl").

%% @doc Execute a named hook with the provided request and options.
on(HookName, Req, Opts) ->
    ?event(hook, {attempting_execution_for_hook, HookName}),
    case find(HookName, Opts) of
        [] ->
            ?event(hook, {no_handlers_for_hook, HookName}),
            {ok, Req};
        Handlers ->
            execute_handlers(HookName, Handlers, Req, Opts)
    end.

%% @doc Get all handlers for a specific hook from the node message.
find(HookName, Opts) ->
    find(#{}, #{ <<"target">> => <<"body">>, <<"body">> => HookName }, Opts).
find(_Base, Req, Opts) ->
    HookName = maps:get(maps:get(<<"target">>, Req, <<"body">>), Req),
    case maps:get(HookName, hb_opts:get(on, #{}, Opts), []) of
        Handler when is_map(Handler) ->
            case hb_util:is_ordered_list(Handler, Opts) of
                true -> hb_util:message_to_ordered_list(Handler, Opts);
                false -> [Handler]
            end;
        Handlers when is_list(Handlers) ->
            Handlers;
        _ ->
            []
    end.

%% @doc Execute a list of handlers in sequence.
execute_handlers(_HookName, [], Req, _Opts) ->
    {ok, Req};
execute_handlers(HookName, [Handler|Rest], Req, Opts) ->
    ?event(hook, {executing_handler, HookName, Handler, Req}),
    case execute_handler(HookName, Handler, Req, Opts) of
        {ok, NewReq} ->
            ?event(hook, {handler_executed_successfully, HookName, NewReq}),
            execute_handlers(HookName, Rest, NewReq, Opts);
        {Status, Res} ->
            {Status, Res};
        Other ->
            ?event(hook_error, {unexpected_handler_result, HookName, Other}),
            {failure,
                <<
                    "Handler for hook `",
                        (hb_ao:normalize_key(HookName))/binary,
                        "` returned unexpected result."
                >>
            }
    end.

%% @doc Execute a single handler message.
execute_handler(
    <<"step">>,
    Handler,
    Req,
    Opts = #{ <<"on">> := On = #{ <<"step">> := _ }}
) ->
    execute_handler(
        <<"step">>,
        maps:remove(<<"step">>, Handler),
        Req,
        Opts#{ <<"on">> => maps:remove(<<"step">>, On) }
    );
execute_handler(HookName, Handler, Req, Opts) ->
    try
        BaseReq =
            Req#{
                <<"path">> =>
                    hb_maps:get(<<"path">>, Handler, HookName, Opts),
                <<"method">> =>
                    hb_maps:get(<<"method">>, Handler, <<"GET">>, Opts)
            },
        CommitReqBin =
            hb_util:bin(
                hb_util:deep_get(
                    <<"hook/commit-request">>,
                    Handler,
                    <<"false">>,
                    Opts
                )
            ),
        {PreparedBase, PreparedReq} =
            case CommitReqBin of
                <<"true">> ->
                    {
                        case hb_message:signers(Handler, Opts) of
                            [] -> hb_message:commit(Handler, Opts);
                            _ -> Handler
                        end,
                        hb_message:commit(BaseReq, Opts)
                    };
                <<"false">> ->
                    {Handler, BaseReq}
            end,
        ?event(hook,
            {resolving_handler,
                {name, HookName},
                {handler, Handler},
                {req, {explicit, PreparedReq}}
            }
        ),
        {Status, Res} =
            hb_ao:raw(
                PreparedBase,
                PreparedReq,
                Opts#{ <<"hashpath">> => ignore }
            ),
        ?event(hook,
            {handler_result,
                {name, HookName},
                {status, Status},
                {res, Res}
            }
        ),
        case {Status, hb_util:deep_get(<<"hook/result">>, Handler, <<"return">>, Opts)} of
            {ok, <<"ignore">>} -> {Status, Req};
            {ok, <<"return">>} -> {Status, Res};
            {ok, <<"error">>} -> {error, Res};
            _ -> {Status, Res}
        end
    catch
        Error:Reason:Stacktrace ->
            ?event(hook_error,
                {handler_exception,
                    {while_executing, HookName},
                    {error, Error},
                    {reason, Reason},
                    {stacktrace, {trace, Stacktrace}}
                }
            ),
            {failure, <<
                "Handler for hook `",
                (hb_ao:normalize_key(HookName))/binary,
                "` raised an exception: ",
                (iolist_to_binary(io_lib:format("~p:~p", [Error, Reason])))/binary
            >>}
    end.

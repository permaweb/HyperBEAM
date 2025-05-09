%%% @doc A generalized interface for `hooking' into HyperBEAM nodes.
%%% 
%%% This module allows users to define `hooks' that are executed at various
%%% points in the lifecycle of nodes and message evaluations.
%%% 
%%% Hooks are maintained in the `node message' options, under the key `on'
%%% key. Each `hook' may have zero or many `handlers' which their request is
%%% executed against. A new `handler' of a hook can be registered by simply
%%% adding a new key to that message. If multiple hooks need to be executed for
%%% a single event, the key's value can be set to a list of hooks.
%%% 
%%% `hook's themselves do not need to be added explicitly. Any device can add
%%% a hook by simply executing `dev_hook:on(HookName, Req, Opts)`. This
%%% function is does not affect the hashpath of a message and is not exported on
%%% the device's API, such that it is not possible to call it directly with
%%% AO-Core resolution.
%%% 
%%% All handlers are expressed in the form of a message, upon which the hook's
%%% request is evaluated:
%%% 
%%%     AO(HookMsg, Req, Opts) => {Status, Result}
%%% 
%%% The `Status' and `Result' of the evaluation can be used at the `hook' caller's
%%% discretion. If multiple handlers are to be executed for a single `hook', the
%%% result of each is used as the input to the next, on the assumption that the
%%% status of the previous is `ok'. If a non-`ok' status is encountered, the
%%% evaluation is halted and the result is returned to the caller. This means
%%% that in most cases, hooks take the form of chainable pipelines of functions,
%%% passing the most pertinent data in the `body' key of both the request and
%%% result. Hook definitions can also set the `hook/result' key to `ignore', if
%%% the result of the execution should be discarded and the prior value (the
%%% input to the hook) should be used instead. The `hook/commit-request' key can
%%% also be set to `true' if the request should be committed by the node before
%%% execution of the hook.
%%% 
%%% The default HyperBEAM node implements several useful hooks. They include:
%%% 
%%%     start: Executed when the node starts.
%%%         Req/body: The node's initial configuration.
%%%         Result/body: The node's possibly updated configuration.
%%%     request: Executed when a request is received via the HTTP API.
%%%         Req/body: The sequence of messages that the node will evaluate.
%%%         Req/request: The raw, unparsed singleton request.
%%%         Result/body: The sequence of messages that the node will evaluate.
%%%     step: Executed after each message in a sequence has been evaluated.
%%%         Req/body: The result of the evaluation.
%%%         Result/body: The result of the evaluation.
%%%     response: Executed when a response is sent via the HTTP API.
%%%         Req/body: The result of the evaluation.
%%%         Req/request: The raw, unparsed singleton request that was used to
%%%             generate the response.
%%%         Result/body: The message to be sent in response to the request.
%%% 
%%% Additionally, this module implements a traditional device API, allowing the
%%% node operator to register hooks to the node and find those that are
%%% currently active.
-module(dev_hook).
-export([info/1, on/3, find/2, find/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Device API information
info(_) ->
    #{ excludes => [<<"on">>] }.

%% @doc Execute a named hook with the provided request and options
%% This function finds all handlers for the hook and evaluates them in sequence.
%% The result of each handler is used as input to the next handler.
on(HookName, Req, Opts) ->
    ?event(hook, {attempting_execution_for_hook, HookName}),
    % Get all handlers for this hook from the options
    Handlers = find(HookName, Opts),
    % If no handlers are found, return the original request with ok status
    case Handlers of
        [] -> 
            ?event(hook, {no_handlers_for_hook, HookName}),
            {ok, Req};
        _ -> 
            % Execute each handler in sequence, passing the result of each to
            % the next as input.
            execute_handlers(HookName, Handlers, Req, Opts)
    end.

%% @doc Get all handlers for a specific hook from the node message options.
%% Handlers are stored in the `on' key of this message. The `find/2' variant of
%% this function only takes a hook name and node message, and is not called
%% directly via the device API. Instead it is used by `on/3' and other internal
%% functionality to find handlers when necessary. The `find/3' variant can,
%% however, be called directly via the device API.
find(HookName, Opts) ->
    find(#{}, #{ <<"target">> => <<"body">>, <<"body">> => HookName }, Opts).
find(_Base, Req, Opts) ->
    HookName = maps:get(maps:get(<<"target">>, Req, <<"body">>), Req),
    case maps:get(HookName, hb_opts:get(on, #{}, Opts), []) of
        Handler when is_map(Handler) -> 
            % If a single handler is found, wrap it in a list.
            [Handler];
        Handlers when is_list(Handlers) -> 
            % If multiple handlers are found, return them as is
            Handlers;
        _ -> 
            % If no handlers are found or the value is invalid, return an empty
            % list.
            []
    end.

%% @doc Execute a list of handlers in sequence.
%% The result of each handler is used as input to the next handler.
%% If a handler returns a non-ok status, execution is halted.
execute_handlers(_HookName, [], Req, _Opts) ->
    % If no handlers remain, return the final request with ok status
    {ok, Req};
execute_handlers(HookName, [Handler|Rest], Req, Opts) ->
    % Execute the current handler
    ?event(hook, {executing_handler, HookName, Handler, Req}),
    % Check the status of the execution
    case execute_handler(HookName, Handler, Req, Opts) of
        {ok, NewReq} ->
            % If status is ok, continue with the next handler
            ?event(hook, {handler_executed_successfully, HookName, NewReq}),
            execute_handlers(HookName, Rest, NewReq, Opts);
        {error, _} = Error ->
            % If status is error, halt execution and return the error
            ?event({handler_error, HookName, Error}),
            Error;
        Other ->
            % If status is unknown, convert to error and halt execution
            ?event({unexpected_handler_result, HookName, Other}),
            {failure,
                <<
                    "Handler for hook `",
                        (hb_ao:normalize_key(HookName))/binary,
                        "` returned unexpected result."
                >>
            }
    end.

%% @doc Execute a single handler
%% Handlers are expressed as messages that can be resolved via AO.
execute_handler(<<"step">>, Handler, Req, Opts = #{ on := On = #{ <<"step">> := _ }}) ->
    % The `step' hook is a special case: It is executed during the course of
    % a resolution, and as such, the key must be removed from the node message
    % before execution of the handler. Failure to do so will result in infinite
    % recursion.
    execute_handler(
        <<"step">>,
        maps:remove(<<"step">>, Handler),
        Req,
        Opts#{ on => maps:remove(<<"step">>, On) }
    );
execute_handler(HookName, Handler, Req, Opts) ->
    try
        % Resolve the handler message, setting the path to the handler name if
        % it is not already set. We ensure to ignore the hashpath such that the
        % handler does not affect the hashpath of a request's output. If the
        % `hook/commit` key is set to `true`, the handler request will be
        % committed before execution.
        BaseReq =
            Req#{
                <<"path">> => hb_ao:get(<<"path">>, Handler, HookName, Opts),
                <<"method">> => hb_ao:get(<<"method">>, Handler, <<"GET">>, Opts)
            },
        PreparedReq =
            case hb_ao:get(<<"hook/commit-request">>, Handler, false, Opts) of
                true -> hb_message:commit(BaseReq, Opts);
                false -> BaseReq
            end,
        ?event(hook,
            {resolving_handler, 
                {name, HookName},
                {handler, Handler},
                {req, {explicit, PreparedReq}}
            }
        ),
        % Resolve the prepared request upon the handler.
        {Status, Res} =
            hb_ao:resolve(
                Handler,
                PreparedReq,
                Opts#{ hashpath => ignore }
            ),
        ?event(hook,
            {handler_result,
                {name, HookName},
                {status, Status},
                {res, Res}
            }
        ),
        case {Status, hb_ao:get(<<"hook/result">>, Handler, <<"return">>, Opts)} of
            {ok, <<"ignore">>} -> {Status, Req};
            {ok, <<"return">>} -> {Status, Res};
            {ok, <<"error">>} -> {error, Res};
            _ -> {Status, Res}
        end
    catch
        Error:Reason:Stacktrace ->
            % If an exception occurs during execution, log it and return an error.
            ?event(hook, {handler_exception, Error, Reason, Stacktrace}),
            {failure, <<
                "Handler for hook `",
                (hb_ao:normalize_key(HookName))/binary,
                "` raised an exception: ",
                (iolist_to_binary(io_lib:format("~p:~p", [Error, Reason])))/binary
            >>}
    end.

%%% Tests

%% @doc Test that hooks with no handlers return the original request
no_handlers_test() ->
    Req = #{ <<"test">> => <<"value">> },
    Opts = #{},
    {ok, Result} = on(<<"test_hook">>, Req, Opts),
    ?assertEqual(Req, Result).

%% @doc Test that a single handler is executed correctly
single_handler_test() ->
    % Create a message with a mock handler that adds a key to the request.
    Handler = #{
        <<"device">> => #{
            <<"test-hook">> =>
                fun(_, Req, _) ->
                    {ok, Req#{ <<"handler_executed">> => true }}
                end
        }
    },
    Req = #{ <<"test">> => <<"value">> },
    Opts = #{ on => #{ <<"test-hook">> => Handler }},
    {ok, Result} = on(<<"test-hook">>, Req, Opts),
    ?assertEqual(true, maps:get(<<"handler_executed">>, Result)).

%% @doc Test that multiple handlers form a pipeline
multiple_handlers_test() ->
    % Create mock handlers that modify the request in sequence
    Handler1 = #{
        <<"device">> => #{
            <<"test-hook">> =>
                fun(_, Req, _) ->
                    {ok, Req#{ <<"handler1">> => true }}
                end
        }
    },
    Handler2 = #{
        <<"device">> => #{
            <<"test-hook">> =>
                fun(_, Req, _) ->
                    {ok, Req#{ <<"handler2">> => true }}
                end
        }
    },
    Req = #{ <<"test">> => <<"value">> },
    Opts = #{ on => #{ <<"test-hook">> => [Handler1, Handler2] }},
    {ok, Result} = on(<<"test-hook">>, Req, Opts),
    ?assertEqual(true, maps:get(<<"handler1">>, Result)),
    ?assertEqual(true, maps:get(<<"handler2">>, Result)).

%% @doc Test that pipeline execution halts on error
halt_on_error_test() ->
    % Create handlers where the second one returns an error
    Handler1 = #{
        <<"device">> => #{
            <<"test-hook">> =>
                fun(_, Req, _) ->
                    {ok, Req#{ <<"handler1">> => true }}
                end
        }
    },
    Handler2 = #{
        <<"device">> => #{
            <<"test-hook">> =>
                fun(_, _, _) ->
                    {error, <<"Error in handler2">>}
                end
        }
    },
    Handler3 = #{
        <<"device">> => #{
            <<"test-hook">> =>
                fun(_, Req, _) ->
                    {ok, Req#{ <<"handler3">> => true }}
                end
        }
    },
    Req = #{ <<"test">> => <<"value">> },
    Opts = #{ on => #{ <<"test-hook">> => [Handler1, Handler2, Handler3] }},
    {error, Result} = on(<<"test-hook">>, Req, Opts),
    ?assertEqual(<<"Error in handler2">>, Result).
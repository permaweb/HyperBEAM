%%% @doc Decorators that stamp trusted runtime context onto messages.
-module(dev_stamp_decorator).
-export([decorator/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Stamp the responding node's configured `http-reference' onto the
%% message in the request body. The node is supplied by the HTTP multirequest
%% layer as private execution context.
decorator(_Base, Req, Opts) ->
    case hb_maps:find(<<"body">>, Req, Opts) of
        {ok, Target} ->
            Node =
                hb_private:get(
                    <<"admissibility/node">>,
                    Req,
                    #{},
                    Opts
                ),
            NodeOpts = hb_maps:get(<<"opts">>, Node, #{}, Opts),
            Ref = hb_maps:get(<<"http-reference">>, NodeOpts, <<>>, Opts),
            {ok,
                (hb_message:uncommitted(Target, Opts))#{
                    <<"http-reference">> => Ref
                }};
        error ->
            {error, missing_target}
    end.

%%% Tests

-ifdef(TEST).

decorator_test() ->
    Base = #{ <<"device">> => <<"stamp-decorator@1.0">> },
    Req =
        hb_private:set(
            #{
                <<"path">> => <<"decorator">>,
                <<"body">> => #{ <<"test">> => <<"value">> }
            },
            <<"admissibility/node">>,
            #{
                <<"opts">> => #{ <<"http-reference">> => <<"peer-1">> }
            },
            #{}
        ),
    ?assertMatch(
        {ok,
            #{
                <<"test">> := <<"value">>,
                <<"http-reference">> := <<"peer-1">>
            }},
        hb_ao:resolve(Base, Req, #{})
    ).

missing_target_test() ->
    Base = #{ <<"device">> => <<"stamp-decorator@1.0">> },
    ?assertEqual(
        {error, missing_target},
        hb_ao:resolve(Base, #{ <<"path">> => <<"decorator">> }, #{})
    ).

-endif.

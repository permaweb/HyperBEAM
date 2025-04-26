-module(dev_evm).
-export([info/1, info/3, get_state/3]).

info(_) ->
    #{
        <<"default">> => dev_message,
        handlers => #{
            <<"info">> => fun info/3,
            <<"get_state">> => fun get_state/3
        }
    }.

info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"EVM device for interacting with load_revm_nif">>,
        <<"version">> => <<"1.0">>,
        <<"paths">> => #{
            <<"info">> => <<"Get device info">>,
            <<"get_state">> => <<"Get appchain state">>
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

get_state(Msg1, _Msg2, Opts) ->
    % Get chain_id from path parameters
    ChainId = case hb_ao:get(<<"chain_id">>, Msg1, not_found, Opts) of
        not_found -> <<"8009">>;  % default chain id
        Id -> Id
    end,
    try
        Result = load_revm_nif:get_appchain_state(ChainId),
        {ok, #{<<"status">> => 200, <<"body">> => Result}}
    catch
        error:Error ->
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to get appchain state">>,
                    <<"details">> => Error
                }
            }}
    end.

%%% @doc The errors that resolving a key returns to its caller, as messages
%%% that name what failed: a key that the spec of the resolving function
%%% requires and its input lacks (`required-key-missing'), a value that the
%%% spec cannot use as its type (`invalid-type'), and a message that the
%%% resolution needs and cannot load (`necessary-message-not-found'). Each
%%% gives the input (`base' or `request') and the key in it, and the device
%%% and key that were being resolved, as `resolving: ~device@1.0/key'.
%%%
%%% A resolution that needs a device that the node cannot load returns
%%% `device-not-loadable', with the `device', the `reason' and `resolving'. A
%%% device in `exclude-devices' has the reason `device-excluded' and the status
%%% 403. Any other device that cannot load has the status 404.
%%%
%%% `page/3' gives the fields of `~hyperbuddy@1.0''s `error.html' for an error
%%% reply to a browser.
-module(hb_ao_errors).
-export([from_throw/5, page/3]).

%%% The details that `error.html' lists for an error reply, by label and key.
-define(DETAILS, [
    {<<"Error type">>, <<"error">>},
    {<<"Offender">>, <<"offender">>},
    {<<"Missing">>, <<"missing">>},
    {<<"Key">>, <<"key">>},
    {<<"In">>, <<"input">>},
    {<<"Expected type">>, <<"expected-type">>},
    {<<"Received">>, <<"received">>},
    {<<"Device">>, <<"device">>},
    {<<"Reason">>, <<"reason">>},
    {<<"Resolving">>, <<"resolving">>}
]).

%% @doc The error that resolving `Req' on `Base' returns for a throw from
%% loading a device, or from the loading and varying of its inputs. Other
%% throws are raised again.
from_throw({required_key_missing, Path}, _Stacktrace, Base, Req, Opts) ->
    error_message(400, <<"required-key-missing">>, Path, #{}, Base, Req, Opts);
from_throw({invalid_type, Path, Type, Value}, _Stacktrace, Base, Req, Opts) ->
    error_message(
        400,
        <<"invalid-type">>,
        Path,
        (received(Path, Value))#{ <<"expected-type">> => format_type(Type) },
        Base,
        Req,
        Opts
    );
from_throw(
    {necessary_message_not_found, Path, Missing},
    _Stacktrace,
    Base,
    Req,
    Opts
) ->
    % `Missing' is the missing ID, or a link to it: `Link (to link): ID'.
    error_message(
        404,
        <<"necessary-message-not-found">>,
        Path,
        #{ <<"missing">> => lists:last(binary:split(Missing, <<": ">>)) },
        Base,
        Req,
        Opts
    );
from_throw(
    {device_not_loadable, Device, Reason},
    _Stacktrace,
    Base,
    Req,
    Opts
) ->
    % The device is not always the base's own, so the error gives no input or
    % key.
    error_message(
        case Reason of
            <<"device-excluded">> -> 403;
            _ -> 404
        end,
        <<"device-not-loadable">>,
        <<>>,
        #{ <<"device">> => Device, <<"reason">> => Reason },
        Base,
        Req,
        Opts
    );
from_throw(Reason, Stacktrace, _Base, _Req, _Opts) ->
    erlang:raise(throw, Reason, Stacktrace).

%% @doc An error message: its status and error, the input and key that its
%% path names, its details, and the device and key being resolved.
error_message(Status, Error, Path, Details, Base, Req, Opts) ->
    {error,
        maps:merge(
            input_key(Path),
            Details#{
                <<"status">> => Status,
                <<"error">> => Error,
                <<"resolving">> => resolving(Base, Req, Opts)
            }
        )
    }.

%% @doc The input and the key in it that a path names: `base/deep/slot' gives
%% `base' and `deep/slot'. A path that does not start at an input names none.
input_key(Path) ->
    case binary:split(Path, <<"/">>) of
        [Input, Key] when Input == <<"base">>; Input == <<"request">> ->
            #{ <<"input">> => Input, <<"key">> => Key };
        _ -> #{}
    end.

%% @doc The value received for a key, unless the value is not a literal or a
%% key on its path is private.
received(Path, Value) when is_binary(Value); is_number(Value); is_atom(Value) ->
    Keys = binary:split(Path, <<"/">>, [global]),
    case lists:any(fun hb_private:is_private/1, Keys) of
        true -> #{};
        false -> #{ <<"received">> => Value }
    end;
received(_Path, _Value) -> #{}.

%% @doc The device and key being resolved, as a path: `~test-device@1.0/key'.
resolving(Base, Req, Opts) ->
    Key = hb_util:bin(key(Req, Opts)),
    case device(hb_ao:normalize_keys(Base, Opts), Opts) of
        undefined -> Key;
        Device -> <<"~", Device/binary, "/", Key/binary>>
    end.

%% @doc The first key of a request's path. A request that is not a message is
%% a path; a link, `as' or `resolve' tuple has no key until it is resolved.
key(Req, Opts) when is_map(Req) -> hb_path:hd(Req, Opts);
key(Req, _Opts) when is_tuple(Req) -> undefined;
key(Path, Opts) -> hb_path:hd(#{ <<"path">> => Path }, Opts).

%% @doc The name of the base's device, or `undefined' if it cannot be read.
device(Base, Opts) when is_map(Base) ->
    try hb_maps:get(<<"device">>, Base, <<"message@1.0">>, Opts) of
        Device when is_binary(Device) -> Device;
        _ -> undefined
    catch throw:{necessary_message_not_found, _, _} -> undefined
    end;
device({as, Device, _}, _Opts) when is_binary(Device) -> Device;
device(_Base, _Opts) ->
    undefined.

%% @doc A schema of `hb_types' as a `-spec' writes it: `integer()', `[_]',
%% `boolean()' or `#{ slot := integer() }'.
format_type(
    #{
        <<"kind">> := <<"union">>,
        <<"members">> :=
            [#{ <<"value">> := true }, #{ <<"value">> := false }]
    }
) ->
    <<"boolean()">>;
format_type(#{ <<"kind">> := <<"union">>, <<"members">> := Members }) ->
    join(<<" | ">>, Members);
format_type(#{ <<"kind">> := <<"literal">>, <<"value">> := Value }) ->
    hb_util:bin(io_lib:format("~tp", [Value]));
format_type(#{ <<"kind">> := <<"list">>, <<"item">> := Item }) ->
    <<"[", (format_type(Item))/binary, "]">>;
format_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }) ->
    <<"{", (join(<<", ">>, Items))/binary, "}">>;
format_type(
    #{ <<"kind">> := <<"range">>, <<"min">> := Min, <<"max">> := Max }
) ->
    hb_util:bin(io_lib:format("~p..~p", [Min, Max]));
format_type(
    #{
        <<"kind">> := <<"message">>,
        <<"keys">> := Keys,
        <<"wildcard">> := Wildcard
    }
) ->
    Fields =
        [
            field(spec_key(Key), Field)
        ||
            Field = #{ <<"name">> := Key } <- lists:sort(Keys)
        ] ++ [ field(<<"_">>, Wildcard) || Wildcard =/= none ],
    case Fields of
        [] -> <<"#{}">>;
        _ -> <<"#{ ", (hb_util:bin(lists:join(<<", ">>, Fields)))/binary, " }">>
    end;
format_type(#{ <<"kind">> := <<"wildcard">> }) -> <<"_">>;
format_type(#{ <<"kind">> := <<"variable">>, <<"name">> := Name }) -> Name;
format_type(#{ <<"kind">> := <<"unknown">>, <<"ast">> := Ast }) -> Ast;
format_type(
    #{
        <<"kind">> := <<"remote">>,
        <<"module">> := Module,
        <<"name">> := Name,
        <<"args">> := Args
    }
) ->
    <<
        (underscored(Module))/binary, ":", (underscored(Name))/binary,
        "(", (join(<<", ">>, Args))/binary, ")"
    >>;
format_type(#{ <<"kind">> := <<"alias">>, <<"name">> := Name }) ->
    <<(underscored(Name))/binary, "()">>;
format_type(#{ <<"kind">> := Kind }) ->
    <<(underscored(Kind))/binary, "()">>.

%% @doc A field of a message type: `slot := integer()' or `slot => _'.
field(Key, #{ <<"presence">> := required, <<"type">> := Type }) ->
    <<Key/binary, " := ", (format_type(Type))/binary>>;
field(Key, #{ <<"type">> := Type }) ->
    <<Key/binary, " => ", (format_type(Type))/binary>>.

%% @doc A key as a `-spec' writes it: `slot', or `'set-mode'' in quotes.
spec_key(Key) ->
    case re:run(Key, <<"\\A[a-z][A-Za-z0-9_@]*\\z">>, [{capture, none}]) of
        match -> Key;
        nomatch -> <<"'", Key/binary, "'">>
    end.

%% @doc The types of a list, written and joined with a separator.
join(Separator, Types) ->
    hb_util:bin(lists:join(Separator, [ format_type(Type) || Type <- Types ])).

%% @doc A dashed name with underscores: `non-neg-integer' gives
%% `non_neg_integer'.
underscored(Name) -> binary:replace(Name, <<"-">>, <<"_">>, [global]).

%% @doc The fields of `error.html' for an error reply: its status, a title and
%% a description, and the details that it holds, with the request's path.
page(Status, Msg, Req) ->
    {Title, Description} = describe(Status, Msg),
    #{
        <<"status">> => Status,
        <<"title">> => Title,
        <<"description">> => Description,
        <<"details">> =>
            [
                {Label, Value}
            ||
                {Label, Key} <- ?DETAILS,
                {ok, Value} <- [maps:find(Key, Msg)]
            ] ++
            [
                {<<"Request">>, Path}
            ||
                {ok, Path} <- [maps:find(<<"path">>, Req)]
            ]
    }.

%% @doc The title and description of an error page.
describe(_Status, #{ <<"offender">> := _ }) ->
    {
        <<"Request parsing error.">>,
        <<"This node cannot parse the part of your request below.">>
    };
describe(_Status, #{ <<"error">> := <<"required-key-missing">> }) ->
    {
        <<"Missing required key.">>,
        <<"The device cannot run this request without the key below.">>
    };
describe(_Status, #{ <<"error">> := <<"invalid-type">> }) ->
    {
        <<"Wrong value type.">>,
        <<"The device cannot use the value below as the type it needs.">>
    };
describe(_Status, #{ <<"error">> := <<"necessary-message-not-found">> }) ->
    {
        <<"Message not found.">>,
        <<"This request needs a message that this node cannot find, yet...">>
    };
describe(403, _Msg) ->
    {
        <<"Access denied.">>,
        <<"This request does not have permission to perform this operation.">>
    };
describe(_Status, _Msg) ->
    {
        <<"Page cannot be found.">>,
        <<"This hashpath cannot be resolved on this node, yet...">>
    }.

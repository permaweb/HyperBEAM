%%% @doc Reading and writing URIs in AO-Core.
%%%
%%% A URI is a binary that starts with a scheme, followed by a `:'. Each scheme
%%% has a device that reads and writes its URIs, with two keys:
%%% <ul>
%%%   <li>`from' loads the resource that a URI identifies.</li>
%%%   <li>`to' writes the URI of a message.</li>
%%% </ul>
%%%
%%% This module finds the device of a URI's scheme and resolves the key on it:
%%%
%%% ```
%%%     from(<<"Scheme:Rest">>, Opts)
%%%         -> resolve(#{ device => DeviceOfScheme, body => Rest },
%%%                    #{ path => from, type => uri }, Opts)
%%%         -> {ok, Message} | {error, Reason}
%%%     to(Message, #{ scheme => Scheme }, Opts)
%%%         -> DeviceOfScheme:to(Message, #{ type => uri }, Opts)
%%%         -> {ok, URI} | {error, Reason}
%%% '''
%%%
%%% `from' sets four options when it resolves the device:
%%% <ul>
%%%   <li>`hashpath' is `ignore', so the resolution adds no hashpath to the
%%%       result.</li>
%%%   <li>`return-context' is `false', so `from' returns the resource, not the
%%%       context of the resolution, when the caller asks for a context.</li>
%%%   <li>`cache-control' is `no-cache' and `no-store', so the result is not
%%%       read from the cache or written to it. Each `from' of a URI reads the
%%%       resource again.</li>
%%%   <li>`force-message' is `false', so a literal that a URI reads stays a
%%%       literal. HTTP requests set `force-message' to `true'. Without this
%%%       option, the same URI would read a message inside an HTTP request,
%%%       and a literal outside of one.</li>
%%% </ul>
%%%
%%% A `to' request may have the key `scheme': the scheme of the URI. The
%%% default is `ao'. With `characters => host', `to' gives the URI in host
%%% form (see below).
%%%
%%% A scheme is a letter, then letters, digits, `+', `-' or `.' (RFC 3986
%%% s3.1). It ends at the first `:'. We read schemes in lowercase.
%%%
%%% The device of a scheme is `<scheme>@1.0', unless the text after the last
%%% `-' of the scheme contains a `.'. In that case, that text is the version
%%% of the device:
%%%
%%% ```
%%%     ao       -> ao@1.0
%%%     did-pkh  -> did-pkh@1.0
%%%     sha-256  -> sha-256@1.0
%%%     lua-5.3a -> lua@5.3a
%%% '''
%%%
%%% The rest of a URI can itself start with another scheme, as in
%%% `did:pkh:<id>'. In this case, the device of the outer scheme can act as a
%%% proxy for a device named after both schemes, `<outer>-<inner>':
%%% <ul>
%%%   <li>`proxy_from/4' reads the text after the inner scheme with the `from'
%%%       key of `<outer>-<inner>@1.0'. For example, `did:pkh:x' is read by
%%%       `did-pkh@1.0', with the body `x'.</li>
%%%   <li>`proxy_to/4' writes the URI of a message whose device is
%%%       `<outer>-<inner>@...', with the `to' key of that device. That `to'
%%%       writes the whole URI, such as `did:pkh:x'.</li>
%%% </ul>
%%%
%%% A URI can also be in `host' form, in the characters of a DNS label (`a'-`z',
%%% `0'-`9' and `-'), which keep their meaning where case is lost: `uri-', the
%%% scheme, `-', and the rest of the URI in the form that the device of the
%%% scheme gives. `uri-did-ar-<...>' is a `did:ar:' URI in host form. A binary
%%% that does not start with a scheme and `:' is in host form if it starts
%%% with `uri-', then a scheme of lowercase letters and digits, then `-'. We
%%% read a URI in host form wherever we read any other:
%%% <ul>
%%%   <li>`from' strips `uri-<scheme>-' where it strips `<scheme>:', and gives
%%%       the device of the scheme the rest with `characters => host' in the
%%%       request. In such a rest, `proxy_from/4' ends the inner scheme at its
%%%       first `-'.</li>
%%%   <li>`to' with `characters => host' takes the URI in host form from the
%%%       device of the scheme. It returns the error `unsupported-characters'
%%%       if the device gives a URI in another form. Only the device can encode
%%%       its rest and split it, because a `-' of a rest cannot be told from a
%%%       separator.</li>
%%% </ul>
%%%
%%% A URI can be:
%%% <ul>
%%%   <li>the base or the request of a resolution (`hb_ao');</li>
%%%   <li>a path part (`hb_singleton');</li>
%%%   <li>a link value, or a path that `hb_cache' reads.</li>
%%% </ul>
%%% A URI in host form can be a host label or a header name. A URI in any other
%%% form cannot, because `:' is not an HTTP token character.
%%%
%%% The errors below are returned with the status 500 over HTTP. No node
%%% attests to such a response, so nodes need not agree on these errors:
%%% <ul>
%%%   <li>`from' and `to' return `unsupported-uri-scheme' for a scheme whose
%%%       device this node does not load.</li>
%%%   <li>`to' returns `unsupported-characters' for `characters => host' and a
%%%       device that gives no URI in host form.</li>
%%%   <li>`scheme/1' also returns `unsupported-uri-scheme' for a binary whose
%%%       first 8000 bytes are all scheme characters. The HTTP server does not
%%%       read request lines longer than 8000 bytes, so no path can contain a
%%%       longer scheme.</li>
%%% </ul>
-module(hb_uri).
-export([from/2, maybe_from/2, to/3, scheme/1, device/1]).
-export([proxy_from/4, proxy_to/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The longest request line that the HTTP server reads: cowboy's default
%%% `max_request_line_length'.
-define(MAX_SCHEME_LENGTH, 8000).

%% @doc Loads the resource that a URI identifies, with the `from' key of the
%% device of its scheme. Returns `{error, not_found}' for a binary with no
%% scheme.
from(URI, Opts) ->
    maybe
        {ok, Scheme, Rest, Form} ?= split(URI),
        from(Scheme, Rest, Form, Opts)
    end.

%% @doc Resolves `from' on the device of a scheme, with the rest of a URI as
%% the body of the base, and the keys of `Form' in the request. Returns the
%% error `unsupported-uri-scheme' if this node does not load the device.
from(Scheme, Rest, Form, Opts) ->
    maybe
        {ok, Device} ?= loaded_device(Scheme, Opts),
        hb_ao:resolve(
            #{ <<"device">> => Device, <<"body">> => Rest },
            Form#{ <<"path">> => <<"from">>, <<"type">> => <<"uri">> },
            Opts#{
                <<"hashpath">> => ignore,
                <<"return-context">> => false,
                <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
                <<"force-message">> => false
            }
        )
    end.

%% @doc Loads the resource associated with a binary, if the binary is a URI.
%% Any other value is returned as it is.
maybe_from(URI, Opts) when is_binary(URI) ->
    case scheme(URI) of
        {error, not_found} -> {ok, URI};
        _ -> from(URI, Opts)
    end;
maybe_from(Value, _Opts) ->
    {ok, Value}.

%% @doc Writes the URI of a message with the `to' key of the device of the
%% request's `scheme', `ao' by default. The rest of the request goes to `to'.
%% With `characters => host', the URI must be in host form.
to(Msg, Req, Opts) ->
    maybe
        {ok, Device} ?=
            loaded_device(hb_maps:get(<<"scheme">>, Req, <<"ao">>, Opts), Opts),
        {ok, URI} ?=
            to(Device, Msg, hb_maps:without([<<"scheme">>], Req, Opts), Opts),
        true ?=
            hb_maps:get(<<"characters">>, Req, <<"all">>, Opts) =/= <<"host">>
                orelse host(URI) =/= {error, not_found}
                orelse
                    {error,
                        #{
                            <<"status">> => 500,
                            <<"error">> => <<"unsupported-characters">>,
                            <<"offender">> => Device
                        }
                    },
        {ok, URI}
    end.

%% @doc Applies the `to' key of a device to a message as it is, with
%% `type: uri' in the request. It uses `hb_ao:raw/5', because the message can
%% be a binary, and `hb_ao:resolve/3' reads a binary base as a URI.
to(Device, Msg, Req, Opts) ->
    hb_ao:raw(Device, <<"to">>, Msg, Req#{ <<"type">> => <<"uri">> }, Opts).

%% @doc Loads the resource of a URI whose rest starts with another scheme. The
%% `from' of the device `<Scheme>-<Inner>@1.0' reads what follows the inner
%% scheme. For `did:pkh:x', `did@1.0' gets the rest `pkh:x', and
%% `did-pkh@1.0' reads `x'. If the request to `did@1.0' has
%% `characters => host', the inner scheme ends at the first `-' of the rest,
%% and the inner device gets `characters => host' too.
proxy_from(Scheme, Rest, Req, Opts) ->
    Form = hb_maps:with([<<"characters">>], Req, Opts),
    maybe
        {ok, Inner, InnerRest} ?= inner_scheme(Rest, Form),
        from(<<Scheme/binary, "-", Inner/binary>>, InnerRest, Form, Opts)
    end.

%% @doc Splits the rest of a URI into its inner scheme and the text after it.
inner_scheme(Rest, #{ <<"characters">> := <<"host">> }) ->
    host_scheme(Rest);
inner_scheme(Rest, _Form) ->
    scheme(Rest).

%% @doc Writes the URI of a message under a scheme whose URIs start with
%% another scheme. The `to' of the message's device writes the whole URI, if
%% that device is `<Scheme>-<Inner>@...'. Returns `{error, not_found}' for a
%% message with any other device.
proxy_to(Scheme, Msg, Req, Opts) ->
    Prefix = <<Scheme/binary, "-">>,
    PrefixSize = byte_size(Prefix),
    case hb_maps:get(<<"device">>, Msg, <<>>, Opts) of
        <<Prefix:PrefixSize/binary, _/binary>> = Device ->
            to(Device, Msg, Req, Opts);
        _ -> {error, not_found}
    end.

%% @doc Splits a URI into its scheme, in lowercase, and its rest: the text
%% after its `:', or in host form after `uri-<scheme>-'. Returns
%% `{error, not_found}' for a binary that does not start with a scheme.
%% Returns the error `unsupported-uri-scheme' for a binary whose first 8000
%% bytes are all scheme characters.
scheme(URI) ->
    case split(URI) of
        {ok, Scheme, Rest, _Form} -> {ok, Scheme, Rest};
        Error -> Error
    end.

%% @doc Splits a URI into its scheme, its rest and the keys of its form: none
%% for a URI that starts with its scheme and `:', and `characters => host' for
%% a URI in host form.
split(URI) ->
    case colon_scheme(URI) of
        {ok, Scheme, Rest} -> {ok, Scheme, Rest, #{}};
        {error, not_found} -> host(URI);
        Error -> Error
    end.

%% @doc Splits a URI in host form, `uri-<scheme>-<rest>'.
host(<<"uri-", Host/binary>>) ->
    maybe
        {ok, Scheme, Rest} ?= host_scheme(Host),
        {ok, Scheme, Rest, #{ <<"characters">> => <<"host">> }}
    end;
host(_URI) ->
    {error, not_found}.

%% @doc Splits a binary into the scheme of lowercase letters and digits that it
%% starts with, a letter first, and the text after the `-' that ends it.
host_scheme(<<C, Rest/binary>> = Bin) when C >= $a, C =< $z ->
    host_scheme(Bin, Rest, 1);
host_scheme(_Bin) ->
    {error, not_found}.
host_scheme(Bin, <<"-", Rest/binary>>, Length) ->
    {ok, binary_part(Bin, 0, Length), Rest};
host_scheme(Bin, <<C, Rest/binary>>, Length)
        when C >= $a, C =< $z; C >= $0, C =< $9 ->
    host_scheme(Bin, Rest, Length + 1);
host_scheme(_Bin, _Rest, _Length) ->
    {error, not_found}.

%% @doc Splits a URI that starts with its scheme and `:'.
colon_scheme(<<C, Rest/binary>> = URI)
        when C >= $a, C =< $z; C >= $A, C =< $Z ->
    scheme(URI, Rest, 1);
colon_scheme(_URI) ->
    {error, not_found}.
scheme(_URI, _Rest, ?MAX_SCHEME_LENGTH) ->
    unsupported(#{});
scheme(URI, <<":", Rest/binary>>, Length) ->
    {ok, hb_util:to_lower(binary_part(URI, 0, Length)), Rest};
scheme(URI, <<C, Rest/binary>>, Length)
        when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9;
            C == $+; C == $-; C == $. ->
    scheme(URI, Rest, Length + 1);
scheme(_URI, _Rest, _Length) ->
    {error, not_found}.

%% @doc Returns the name of the device that reads the URIs of a scheme.
device(Scheme) ->
    case string:split(Scheme, <<"-">>, trailing) of
        [Name, Version] ->
            case binary:match(Version, <<".">>) of
                nomatch -> <<Scheme/binary, "@1.0">>;
                _ -> <<Name/binary, "@", Version/binary>>
            end;
        [Scheme] -> <<Scheme/binary, "@1.0">>
    end.

%% @doc Returns the device of a scheme, if this node loads it. Returns the
%% error `unsupported-uri-scheme' if it does not.
loaded_device(Scheme, Opts) ->
    Device = device(Scheme),
    case hb_device_load:reference(Device, Opts) of
        {ok, _} -> {ok, Device};
        {error, _} -> unsupported(#{ <<"offender">> => Scheme })
    end.

%% @doc Returns the error `unsupported-uri-scheme', with the details given.
unsupported(Details) ->
    {error,
        Details#{
            <<"status">> => 500,
            <<"error">> => <<"unsupported-uri-scheme">>
        }
    }.

%%% Tests

scheme_test() ->
    ?assertEqual({ok, <<"ao">>, <<"abc">>}, scheme(<<"ao:abc">>)),
    ?assertEqual(
        {ok, <<"ni">>, <<"///sha-256;abc">>},
        scheme(<<"NI:///sha-256;abc">>)
    ),
    ?assertEqual({ok, <<"did">>, <<"pkh:a:b">>}, scheme(<<"did:pkh:a:b">>)),
    ?assertEqual({ok, <<"web+a.b-c">>, <<>>}, scheme(<<"web+a.b-c:">>)),
    ?assertEqual({error, not_found}, scheme(<<"k=v:w">>)),
    ?assertEqual({error, not_found}, scheme(<<"1a:b">>)),
    ?assertEqual({error, not_found}, scheme(<<"no-colon">>)),
    ?assertEqual({error, not_found}, scheme(<<>>)),
    % A URI in host form: `uri-', a scheme of lowercase letters and digits, and
    % `-'. A binary that starts with a scheme and `:' is not in host form.
    ?assertEqual({ok, <<"did">>, <<"ar-x">>}, scheme(<<"uri-did-ar-x">>)),
    ?assertEqual({ok, <<"uri-a">>, <<"b">>}, scheme(<<"uri-a:b">>)),
    ?assertEqual({error, not_found}, scheme(<<"uri-Did-x">>)),
    ?assertEqual({error, not_found}, scheme(<<"uri-1a-x">>)),
    ?assertEqual({error, not_found}, scheme(<<"uri-did">>)).

device_test() ->
    ?assertEqual(<<"ao@1.0">>, device(<<"ao">>)),
    ?assertEqual(<<"sha-256@1.0">>, device(<<"sha-256">>)),
    ?assertEqual(<<"did-pkh@1.0">>, device(<<"did-pkh">>)),
    ?assertEqual(<<"lua@5.3a">>, device(<<"lua-5.3a">>)).

%% @doc `scheme/1' reads a scheme of 7999 bytes, and refuses a binary that
%% starts with 8000 scheme characters.
scan_limit_test() ->
    Long = binary:copy(<<"a">>, ?MAX_SCHEME_LENGTH - 1),
    ?assertMatch({ok, Long, <<"x">>}, scheme(<<Long/binary, ":x">>)),
    ?assertMatch(
        {error, #{ <<"status">> := 500 }},
        scheme(<<Long/binary, "a:x">>)
    ).

from_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    ?assertMatch(
        {error,
            #{
                <<"status">> := 500,
                <<"error">> := <<"unsupported-uri-scheme">>,
                <<"offender">> := <<"no-such-scheme">>
            }
        },
        from(<<"No-Such-Scheme:x">>, Opts)
    ),
    % `hb_ao' reads a base with a scheme as a URI, before it resolves a key or
    % sets a device.
    ?assertMatch(
        {error, #{ <<"error">> := <<"unsupported-uri-scheme">> }},
        hb_ao:resolve(<<"no-such-scheme:x">>, <<"key">>, Opts)
    ),
    ?assertMatch(
        {error, #{ <<"error">> := <<"unsupported-uri-scheme">> }},
        hb_ao:resolve(
            <<"no-such-scheme:x">>,
            {as, <<"message@1.0">>, <<"key">>},
            Opts
        )
    ),
    % `resolve_many' returns a binary with no scheme as it is.
    ?assertEqual({ok, <<"no-colon">>}, hb_ao:resolve_many([<<"no-colon">>], Opts)),
    % A URI in host form is read by the device of its scheme.
    ?assertMatch(
        {error, #{ <<"offender">> := <<"nosuchscheme">> }},
        hb_ao:resolve_many([<<"uri-nosuchscheme-x">>], Opts)
    ).

%% @doc `proxy_from/4' reads `test:device:x' with `test-device@1.0', and
%% `proxy_to/4' writes the same URI back. In host form, the rest `device-x'
%% is read with `test-device@1.0' too.
proxy_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    {ok, Msg} = proxy_from(<<"test">>, <<"device:x">>, #{}, Opts),
    ?assertEqual({ok, <<"test:device:x">>}, proxy_to(<<"test">>, Msg, #{}, Opts)),
    ?assertEqual(
        {ok, Msg},
        proxy_from(
            <<"test">>,
            <<"device-x">>,
            #{ <<"characters">> => <<"host">> },
            Opts
        )
    ).

to_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    ?assertMatch(
        {error, #{ <<"offender">> := <<"no-such-scheme">> }},
        to(#{}, #{ <<"scheme">> => <<"no-such-scheme">> }, Opts)
    ),
    % `ao@1.0' gives no URI in host form.
    ?assertMatch(
        {error, #{ <<"error">> := <<"unsupported-characters">> }},
        to(#{}, #{ <<"characters">> => <<"host">> }, Opts)
    ).

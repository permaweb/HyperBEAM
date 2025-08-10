%%% @doc A utility device that manages setting and encoding/decoding the cookies
%%% found in requests from a caller. This device implements the `~cookie@1.0'
%%% codec, inline with the `~message@1.0' schema for conversion.
%%% 
%%% Additionally, a `commit' to a message using a secret generated and stored 
%%% in the cookies of the caller, and a `verify' key that validates said
%%% commitments. In addition, a `generate' key is provided to perform only the
%%% generation side of the commitment process. The `finalize' key may be 
%%% employed to add a `set' operation to the end of a message sequence, which
%%% is used in hooks that need to ensure a caller always receives cookies
%%% generated outside of the normal AO-Core execution flow. In totality, these
%%% keys implement the `generator' interface type, and may be employed in
%%% various contexts. For example, `~auth-hook@1.0' may be configured to use
%%% this device to generate and store secrets in the cookies of the caller,
%%% which are then used with the `~proxy-wallet@1.0' device to sign requests.
%%% 
%%% The `commit' and `verify' keys utilize the `~httpsig@1.0''s HMAC `secret'
%%% commitment scheme, which uses a secret key to commit to a message, with the
%%% `committer' being listed as a hash of the secret.
%%% 
%%% This device supports the following paths:
%%% 
%%% `/commit': Sets a `secret' key in the cookies of the caller. The name of 
%%% the cookie is calculated as the hash of the secret. 
%%% `/verify': Verifies the caller's request by checking the committer in the
%%% request matches the secret in the cookies of the base message.
%%% `/store': Sets the keys in the request message in the cookies of the caller.
%%% `/extract': Extracts the cookies from a base message.
%%% `/reset': Removes all cookie keys from the base message.
%%% `/to': Converts a message containing cookie sources (`cookie', `set-cookie',
%%% or `priv/cookie') into the format specified in the request message (e.g.
%%% `set-cookie', `cookie').
%%% `/from': Converts a message containing encoded cookies into a message
%%% containing the cookies parsed and normalized.
-module(dev_codec_cookie).
%%% Public cookie manipulation API.
-export([get_cookie/3, store/3, extract/3, reset/2]).
%%% Public message codec API.
-export([to/3, from/3]).
%%% Public commit/verify API.
-export([commit/3, verify/3]).
%%% Generator API.
-export([generate/3, finalize/3]).
%%% Public utility functions.
-export([opts/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Get the private store options to use for functions in the cookie device.
opts(Opts) -> hb_private:opts(Opts).

%%% ~message@1.0 Commitments API keys.
commit(Base, Req, RawOpts) -> dev_codec_cookie_auth:commit(Base, Req, RawOpts).
verify(Base, Req, RawOpts) -> dev_codec_cookie_auth:verify(Base, Req, RawOpts).

%% @doc Preprocessor keys that utilize cookies and the `~secret@1.0' device to
%% sign inbound HTTP requests from users if they are not already signed. We use
%% the `~hook@1.0' authentication framework to implement this.
generate(Base, Req, Opts) ->
    dev_codec_cookie_auth:generate(Base, Req, Opts).

%% @doc Finalize an `on-request' hook by adding the `set-cookie' header to the
%% end of the message sequence.
finalize(Base, Request, Opts) ->
    dev_codec_cookie_auth:finalize(Base, Request, Opts).

%% @doc Get the cookie with the given key from the base message. The format of
%% the cookie is determined by the `format' key in the request message:
%% - `default': The cookie is returned in its raw form. It will be a message
%%   if the source was a `set-cookie' header line containing attributes/flags,
%%   or a binary if only the value was provided (as with the `cookie' header).
%% - `set-cookie': The cookie is normalized to a message with `value',
%%   `attributes', and `flags' keys.
%% - `cookie': The cookie is normalized to a binary, ommitting any attributes
%%   or flags.
%% 
%% The `format' may be specified in the request message as the `req:format' key.
%% If no `format' is specified, the default is `default'.
get_cookie(Base, Req, RawOpts) ->
    Opts = opts(RawOpts),
    {ok, Cookies} = extract(Base, Req, Opts),
    Key = hb_maps:get(<<"key">>, Req, undefined, Opts),
    case hb_maps:get(Key, Cookies, undefined, Opts) of
        undefined -> {error, not_found};
        Cookie ->
            Format = hb_maps:get(<<"format">>, Req, <<"default">>, Opts),
            case Format of
                <<"default">> -> {ok, Cookie};
                <<"set-cookie">> -> {ok, normalize_cookie_value(Cookie)};
                <<"cookie">> -> {ok, value(Cookie)}
            end
    end.

%% @doc Return the parsed and normalized cookies from a message.
extract(Msg, Req, Opts) ->
    {ok, MsgWithCookie} = from(Msg, Req, Opts),
    Cookies = hb_private:get(<<"cookie">>, MsgWithCookie, #{}, Opts),
    {ok, Cookies}.

%% @doc Set the keys in the request message in the cookies of the caller. Removes
%% a set of base keys from the request message before setting the remainder as
%% cookies.
store(Base, Req, RawOpts) ->
    Opts = opts(RawOpts),
    ?event({store, {base, Base}, {req, Req}}),
    {ok, ExistingCookies} = extract(Base, Req, Opts),
    ?event({store, {existing_cookies, ExistingCookies}}),
    {ok, ResetBase} = reset(Base, Opts),
    ?event({store, {reset_base, ResetBase}}),
    MsgToSet =
        hb_maps:without(
            [
                <<"path">>,
                <<"accept-bundle">>,
                <<"ao-peer">>,
                <<"host">>,
                <<"method">>,
                <<"body">>
            ],
            hb_private:reset(Req),
            Opts
        ),
    ?event({store, {msg_to_set, MsgToSet}}),
    NewCookies = hb_maps:merge(ExistingCookies, MsgToSet, Opts),
    NewBase = hb_private:set(ResetBase, <<"cookie">>, NewCookies, Opts),
    {ok, NewBase}.

%% @doc Remove all cookie keys from the given message (including `cookie' and
%% `set-cookie' in the base, and `priv/cookie' in the request message).
reset(Base, RawOpts) ->
    Opts = opts(RawOpts),
    WithoutBaseCookieKeys =
        hb_maps:without(
            [<<"cookie">>, <<"set-cookie">>],
            Base,
            Opts
        ),
    WithoutPrivCookie =
        hb_private:set(
            WithoutBaseCookieKeys,
            <<"cookie">>,
            unset,
            Opts
        ),
    {ok, WithoutPrivCookie}.

%% @doc Convert a message containing cookie sources (`cookie', `set-cookie',
%% or `priv/cookie') into a message containing the cookies serialized as the
%% specified `format' (given in the request message). The `format' may take the
%% following values:
%% 
%% - `set-cookie': A list of encoded cookie binary header lines (e.g.
%%   `"key1=value1; attr1=value2; flag1; flag2..."').
%% - `cookie': A single, concatenated cookie header line without attributes or
%%   flags (e.g. `"key1=value1; key2=value2; ..."').
%% 
%% Note that the `format: cookie' form is information lossy: All provided
%% attributes and flags are discarded.
to(Msg, Req, Opts) ->
    ?event({to, {msg, Msg}, {req, Req}}),
    CookieOpts = opts(Opts),
    LoadedMsg = hb_cache:ensure_all_loaded(Msg, CookieOpts),
    ?event({to, {loaded_msg, LoadedMsg}}),
    do_to(LoadedMsg, Req, CookieOpts).
do_to(Msg, Req = #{ <<"format">> := <<"set-cookie">> }, Opts) when is_map(Msg) ->
    ?event({to_set_cookie, {msg, Msg}, {req, Req}}),
    {ok, ExtractedParsedCookies} = extract(Msg, Req, Opts),
    {ok, ResetBase} = reset(Msg, Opts),
    SetCookieLines =
        maps:values(
            maps:map(
                fun to_set_cookie_line/2,
                ExtractedParsedCookies
            )
        ),
    MsgWithSetCookie =
        ResetBase#{
            <<"set-cookie">> => SetCookieLines
        },
    {ok, MsgWithSetCookie};
do_to(Msg, Req = #{ <<"format">> := <<"cookie">> }, Opts) when is_map(Msg) ->
    ?event({to_cookie, {msg, Msg}, {req, Req}}),
    {ok, ExtractedParsedCookies} = extract(Msg, Req, Opts),
    {ok, ResetBase} = reset(Msg, Opts),
    CookieLines =
        hb_maps:values(
            hb_maps:map(
            fun to_cookie_line/2,
                ExtractedParsedCookies,
                Opts
            ),
            Opts
        ),
    ?event({to_cookie, {cookie_lines, CookieLines}}),
    CookieLine = join(CookieLines, <<"; ">>),
    {ok, ResetBase#{ <<"cookie">> => CookieLine }};
do_to(Msg, _Req, _Opts) when is_map(Msg) ->
    error({cookie_to_error, {no_format_specified, Msg}});
do_to(Msg, _Req, _Opts) ->
    error({cookie_to_error, {unexpected_message_format, Msg}}).

%% @doc Convert a single cookie into a `set-cookie' header line. The cookie 
%% may come in the form of `key => binary' or `key => cookie-message', where
%% the cookie-message is a map with the following keys:
%% 
%% - `value': The raw binary cookie value.
%% - `attributes': A map of cookie attribute key-value pairs.
%% - `flags`: A list of cookie flags, represented as binaries.
%% 
%% If the cookie is a binary, we normalize it to a cookie-message before 
%% processing.
%% Note: Assumes that the cookies have all been loaded from the cache fully.
to_set_cookie_line(Key, RawCookie) ->
    Cookie = normalize_cookie_value(RawCookie),
    % Encode the cookie key-value pair as a string to use as the base.
    ValueBin =
        <<
            Key/binary, "=\"",
            (maps:get(<<"value">>, Cookie))/binary,
            "\""
        >>,
    % Encode the cookie attributes as key-value (non-quoted) pairs, separated
    % by `;'.
    ?event({to_line, {key, Key}, {cookie, {explicit, Cookie}}, {value, ValueBin}}),
    AttributesBin =
        case maps:get(<<"attributes">>, Cookie, #{}) of
            EmptyAttributes when map_size(EmptyAttributes) == 0 ->
                ?event({attributes, {none_in, Cookie}}),
                <<>>;
            Attributes ->
                ?event({attributes, Attributes}),
                JointAttributes =
                    join(
                        [
                                << AttrKey/binary, "=", AttrValue/binary >>
                            ||
                                {AttrKey, AttrValue} <- to_sorted_list(Attributes)
                        ],
                        <<"; ">>
                    ),
                << "; ", JointAttributes/binary >>
        end,
    FlagsBin =
        case maps:get(<<"flags">>, Cookie, []) of
            [] -> <<>>;
            Flags -> << "; ", (join(Flags, <<"; ">>))/binary >>
        end,
    << ValueBin/binary, AttributesBin/binary, FlagsBin/binary >>.

%% @doc Convert a single cookie into a `cookie' header component. These
%% components can be joined to form a `cookie' header line. This function
%% reuses the `to_set_cookie_line' function to generate the components, but
%% unsets the `attributes' and `flags' keys first.
to_cookie_line(Key, Cookie) ->
    to_set_cookie_line(Key, value(Cookie)).

%% @doc Normalize a message containing a `cookie', `set-cookie', and potentially
%% a `priv/cookie' key into a message with only the `priv/cookie' key.
from(Msg, Req, Opts) ->
    CookieOpts = opts(Opts),
    LoadedMsg = hb_cache:ensure_all_loaded(Msg, Opts),
    do_from(LoadedMsg, Req, CookieOpts).
do_from(Msg, Req, Opts) when is_map(Msg) ->
    {ok, ResetBase} = reset(Msg, Opts),
    % Get the cookies, parsed, from each available source.
    {ok, FromCookie} = from_cookie(Msg, Req, Opts),
    {ok, FromSetCookie} = from_set_cookie(Msg, Req, Opts),
    FromPriv = hb_private:get(<<"cookie">>, Msg, #{}, Opts),
    % Merge all found cookies into a single map.
    MergedMsg = hb_maps:merge(FromCookie, FromSetCookie, Opts),
    AllParsed = hb_maps:merge(MergedMsg, FromPriv, Opts),
    % Set the cookies in the private element of the message.
    {ok, hb_private:set(ResetBase, <<"cookie">>, AllParsed, Opts)};
do_from(CookiesMsg, _Req, _Opts) ->
    error({cookie_from_error, {unexpected_message_format, CookiesMsg}}).

%% @doc Convert the `cookie' key into a parsed cookie message. `cookie' headers
%% are in the format of `key1=value1; key2=value2; ...'. There are no attributes
%% or flags, so we split on `;' and return a map of key-value pairs. We also
%% decode the values, in case they are URI-encoded.
from_cookie(#{ <<"cookie">> := Cookie }, Req, Opts) ->
    from_cookie(Cookie, Req, Opts);
from_cookie(Cookies, Req, Opts) when is_list(Cookies) ->
    MergedParsed =
        lists:foldl(
            fun(Cookie, Acc) ->
                {ok, Parsed} = from_cookie(Cookie, Req, Opts),
                hb_maps:merge(Acc, Parsed, Opts)
            end,
            #{},
            Cookies
        ),
    {ok, MergedParsed};
from_cookie(Cookie, _Req, _Opts) when is_binary(Cookie) ->
    BinaryCookiePairs = split(semicolon, Cookie),
    KeyValList =
        lists:map(
            fun(BinaryCookiePair) ->
                {[Key, Value], _Rest} = split(pair, BinaryCookiePair),
                {Key, hb_escape:decode(Value)}
            end,
            BinaryCookiePairs
        ),
    NormalizedMessage = maps:from_list(KeyValList),
    {ok, NormalizedMessage};
from_cookie(_MsgWithoutCookie, _Req, _Opts) ->
    % The cookie key is not present in the message, so we return an empty map.
    {ok, #{}}.

%% @doc Convert a `set-cookie' header line into a cookie message. The `set-cookie'
%% header has a `key=value' pair, and possibly attributes and flags. The form
%% looks as follows: `key=value; attr1=value1; attr2=value2; flag1; flag2'.
from_set_cookie(#{ <<"set-cookie">> := Cookie }, Req, Opts) ->
    ?event({from_set_cookie, {cookie, Cookie}}),
    from_set_cookie(Cookie, Req, Opts);
from_set_cookie(MsgWithoutSet, _Req, _Opts) when is_map(MsgWithoutSet) ->
    % The set-cookie key is not present in the message, so we return an empty map.
    {ok, #{}};
from_set_cookie(Lines, Req, Opts) when is_list(Lines) ->
    MergedParsed =
        lists:foldl(
            fun(Line, Acc) ->
                {ok, Parsed} = from_set_cookie(Line, Req, Opts),
                hb_maps:merge(Acc, Parsed)
            end,
            #{},
            Lines
        ),
    {ok, MergedParsed};
from_set_cookie(Line, _Req, Opts) when is_binary(Line) ->
    {[Key, Value], Rest} = split(pair, Line),
    ValueDecoded = hb_escape:decode(Value),
    % If there is no remaining binary after the pair, we have a simple key-value
    % pair, returning just the binary as the value. Otherwise, we split the
    % remaining binary into attributes and flags and return a message with the
    % value and those parsed elements.
    case Rest of
        <<>> -> {ok, #{ Key => ValueDecoded }};
        _ ->
            AllAttrs = split(semicolon, Rest),
            % We partition the attributes into pairs and flags, where flags are
            % any attributes that do not contain an `=' character.
            {AttrPairs, Flags} =
                lists:partition(
                    fun(Attr) ->
                        case hb_util:split_depth_string_aware_single($=, Attr) of
                            {no_match, _, _} -> false;
                            {_, _, _} -> true
                        end
                    end,
                    AllAttrs
                ),
            % We sort the flags and generate an attributes map from the pairs.
            SortedFlags = to_sorted_list(Flags),
            UnquotedFlags = lists:map(fun unquote/1, SortedFlags),
            ?event(
                {from_line,
                    {key, Key},
                    {value, {explicit, Value}},
                    {attrs, AttrPairs},
                    {flags, UnquotedFlags}
                }
            ),
            Attributes =
                maps:from_list(
                    lists:map(
                        fun(AttrPairBin) ->
                            {[AttrKey, AttrValue], _} = split(pair, AttrPairBin),
                            AttrKeyTrimmed = trim_bin(AttrKey),
                            AttrValueTrimmed = trim_bin(AttrValue),
                            {AttrKeyTrimmed, unquote(AttrValueTrimmed)}
                        end,
                        AttrPairs
                    )
                ),
            MaybeAttributes =
                if map_size(Attributes) > 0 -> #{ <<"attributes">> => Attributes };
                true -> #{}
                end,
            MaybeFlags =
                if length(UnquotedFlags) > 0 -> #{ <<"flags">> => UnquotedFlags };
                true -> #{}
                end,
            MaybeAllAttributes = hb_maps:merge(MaybeAttributes, MaybeFlags, Opts),
            {ok, #{ Key => MaybeAllAttributes#{ <<"value">> => ValueDecoded }}}
    end.

%%% Internal helpers

%% @doc Takes a message or list of binaries and returns a sorted list of key-
%% value pairs. Assumes that the message has been loaded from the cache fully.
to_sorted_list(Msg) when is_map(Msg) ->
    lists:keysort(
        1,
        [
            {trim_bin(hb_util:bin(K)), trim_bin(V)}
            || {K, V} <- maps:to_list(Msg)
        ]
    );
to_sorted_list(Binaries) when is_list(Binaries) ->
    lists:sort(
        lists:map(
            fun(Bin) -> trim_bin(hb_util:bin(Bin)) end,
            Binaries
        )
    ).

%% @doc Take a single parse cookie and return only the value (ignoring attributes
%% and flags).
value(Msg) when is_map(Msg) ->
    maps:get(<<"value">>, Msg, Msg);
value(Bin) when is_binary(Bin) ->
    Bin.

%% @doc Normalize a cookie value to a map with the following keys:
%% - `value': The raw binary cookie value.
%% - `attributes': A map of cookie attribute key-value pairs.
%% - `flags`: A list of cookie flags, represented as binaries.
normalize_cookie_value(Msg) when is_map(Msg) ->
    Msg#{
        <<"value">> => maps:get(<<"value">>, Msg, Msg),
        <<"attributes">> => maps:get(<<"attributes">>, Msg, #{}),
        <<"flags">> => maps:get(<<"flags">>, Msg, [])
    };
normalize_cookie_value(Bin) when is_binary(Bin) ->
    #{
        <<"value">> => Bin,
        <<"attributes">> => #{},
        <<"flags">> => []
    }.

%%% Internal helpers

%% @doc Trim a binary of leading and trailing whitespace.
trim_bin(Bin) when is_binary(Bin) ->
    list_to_binary(string:trim(binary_to_list(Bin))).

%% @doc Join a list of binaries into a `separator'-separated string. Abstracts
%% the complexities of converting to/from string lists, as Erlang only provides
%% a `binary:join` function as of OTP/28.
join(Binaries, Separator) ->
    hb_util:bin(
        string:join(
            lists:map(fun hb_util:list/1, Binaries),
            hb_util:list(Separator)
        )
    ).

%% @doc Split a binary by a separator type (`pair', `lines', or `attributes').
%% Separator types that are plural return a list of all parts. Singular types
%% return a single part and the remainder of the binary.
split(pair, Bin) ->
    [Key, ValueRest] = binary:split(Bin, <<"=">>),
    {_, Value, Rest} = hb_util:split_depth_string_aware_single($;, ValueRest),
    {[Key, unquote(Value)], trim_leading(Rest)};
split(lines, Bin) ->
    lists:map(fun trim_leading/1, hb_util:split_depth_string_aware($,, Bin));
split(semicolon, Bin) ->
    lists:map(fun trim_leading/1, hb_util:split_depth_string_aware($;, Bin)).

%% @doc Remove leading whitespace from a binary, if present.
trim_leading(Line) when not is_binary(Line) ->
    trim_leading(hb_util:bin(Line));
trim_leading(<<>>) -> <<>>;
trim_leading(<<" ", Rest/binary>>) -> trim_leading(Rest);
trim_leading(Line) -> Line.

%% @doc Unquote a binary if it is quoted. If it is not quoted, we return the
%% binary as is.
unquote(<< $\", Rest/binary>>) ->
    {Unquoted, _} = hb_util:split_escaped_single($\", Rest),
    Unquoted;
unquote(Bin) -> Bin.
%%% @doc A utility device that manages setting and encoding/decoding the cookies
%%% found in requests from the caller. This device implements the `~cookie@1.0'
%%% codec, inline with the `~message@1.0' schema for conversion. It does not
%%% support commitments, but does act as a valid target format for unsigned
%%% messages.
%%% 
%%% Additionally, a `generate' and `verify' API is provided to generate and
%%% verify a secret stored in the cookies of the caller.
%%% 
%%% The cryptographic scheme used in the generate and verify APIs is as follows:
%%% 
%%%     generate:
%%%         secret = 32 random bytes
%%%         name = req.name or base64url(sha256(secret))
%%%         commitment = sha256(secret ++ name)
%%%         return cookie(#{ secret, commitment, name })
%%% 
%%%     verify:
%%%         expected = sha256(req.cookie.secret ++ (req.name || req.cookie.name))
%%%         return req.cookie.commitment == expected
%%% 
%%% This scheme enables three different modes of operation:
%%% 
%%% 1. Generation with a random name.
%%%    Allowing: The caller to be identified by the node as the holder of the
%%%    secret for a name, but not necessarily known/remembered by the node. If
%%%    the node adds the `name' key to the verification request, they may gain
%%%    certainty that the caller is also indeed a holder of a specific secret
%%%    generated previously.
%%% 2. Generation with a specific name.
%%%    Allowing: The node to ensure that the caller has a secret that relates
%%%    only to the name provided during generation.
%%% 3. Generation with a specific name, but omitting the name from the cookie.
%%%    Allowing: The node to have a unique identifier for the caller that is
%%%    unknown to them.
%%% 
%%% Note: While names given by the caller of `generate' are combined with the
%%% secret to generate the commitment, allowing user's to be oblivious to their
%%% name, `~cookie@1.0` makes no guarantee that chosen names are unique. If no
%%% name is provided to the `generate' call, however, the name is chosen at
%%% random from a 256-bit address space.
%%% 
%%% This device supports the following paths:
%%% 
%%% `/generate': Sets a `secret', `nonce', and `name' key in the cookies of the
%%% caller. The name may be set by the caller, or will be calculated as the hash
%%% of the nonce. The `secret' is a SHA-256 hash commitment of the nonce and
%%% name. Optionally takes an `omit' parameter to omit the caller's `name' from
%%% the cookie.
%%% `/verify': Verifies the caller's request by checking the parameters in the
%%% request match the parameters in the cookies of the base message. Optionally,
%%% the `name' may be provided in the request to ensure that the caller is the
%%% holder of an associated secret.
%%% `/set-cookie': Sets the keys in the request message in the cookies of the
%%% caller.
-module(dev_codec_cookie).
%%% Public cookie manipulation API.
-export([get/3, set/3]).
%%% Public codec API.
-export([to/3, from/3]).
%%% Public commit/verify API.
-export([commit/3, verify/3]).
%%% Public utility functions.
-export([opts/1, reset/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% ~message@1.0 Commitments API keys.
commit(Base, Req, RawOpts) -> dev_codec_cookie_auth:commit(Base, Req, RawOpts).
verify(Base, Req, RawOpts) -> dev_codec_cookie_auth:verify(Base, Req, RawOpts).

%% @doc Get the options to use for functions in the cookie device. We use the
%% `priv_store' option if set, such that evaluations are not inadvertently
%% persisted in public storage. Additionally, we ensure that no cache entries
%% are generated from downstream AO-Core resolutions.
opts(Opts) ->
    Opts#{
        store =>
            case hb_opts:get(priv_store, undefined, Opts) of
                undefined -> hb_opts:get(store, undefined, Opts);
                PrivStore -> PrivStore
            end,
        cache_control => [<<"no-store">>, <<"no-cache">>]
    }.

%% @doc Get the cookie with the given key from the base message.
get(Base, Req, RawOpts) ->
    Opts = opts(RawOpts),
    {ok, ParsedBase} = from(Base, Req, Opts),
    Key = hb_maps:get(<<"key">>, Req, undefined, Opts),
    Cookie = hb_maps:get(<<"cookie">>, ParsedBase, #{}, Opts),
    case hb_maps:get(Key, Cookie, undefined, Opts) of
        undefined -> {error, not_found};
        Value -> {ok, Value}
    end.

%% @doc Set the keys in the request message in the cookies of the caller.
set(_, Req, RawOpts) ->
    Opts = opts(RawOpts),
    RawTABM =
        hb_maps:without(
            [<<"path">>],
            hb_message:convert(Req, tabm, <<"structured@1.0">>, Opts),
            Opts
        ),
    TABM =
        hb_maps:map(
            fun(_, V) -> hb_escape:encode(V) end,
            RawTABM
        ),
    Res = to(#{ <<"cookie">> => TABM }, Req, Opts),
    ?event({set, {req, Req}, {res, Res}}),
    Res.

%% @doc Remove the cookie from the given message.
reset(Base, RawOpts) ->
    Opts = opts(RawOpts),
    {ok, hb_ao:set(Base, #{ <<"cookie">> => unset }, Opts)}.

%% @doc Convert a set of cookie messages a cookie message. If the request
%% message contains a `bundle: true' key, we return the cookie as a comma-
%% separated string, as if the cookie headers had been combined in the style of
%% HTTP/3 header concatenation. This enables us to grant a single binary
%% representation of the cookies as needed.
to(Link, Req, Opts) when ?IS_LINK(Link) ->
    to(hb_cache:ensure_loaded(Link), Req, Opts);
to(Msg = #{ <<"cookie">> := Cookie }, Req, Opts) ->
    % If given a message containing a cookie, we encode just the cookie
    % component and return it as the `set-cookie' key -- ready for transmission
    % to a browser to set its values.
    MsgWithoutCookie = hb_maps:without([<<"cookie">>], Msg, Opts),
    to(MsgWithoutCookie#{ <<"set-cookie">> => Cookie }, Req, Opts);
to(Msg = #{ <<"set-cookie">> := SetCookie }, Req, Opts) ->
    {
        ok,
        Msg#{
            <<"set-cookie">> => hb_util:ok(to(SetCookie, Req, Opts))
        }
    };
to(BinaryCookie, _Req, _Opts) when is_binary(BinaryCookie) ->
    % If the cookie is already a binary, we return it as is.
    {ok, BinaryCookie};
to(Cookies = [Cookie|_], Req, RawOpts) when is_binary(Cookie) ->
    Opts = opts(RawOpts),
    % If the cookie is a list of binaries, we check the requested format and
    % return either the list as-is, or join them into a comma-separated string.
    case hb_maps:get(<<"bundle">>, Req, false, Opts) of
        false -> {ok, Cookies};
        true ->
            {ok, join(Cookies, <<", ">>)}
    end;
to(CookiesMsg, Req, RawOpts) ->
    % The cookie set is in message form: A map of key => [binary|cookie-message]
    % pairs. Key=>cookie-message pairs represent the cookie as:
    % - `value': The raw binary cookie value.
    % - `attributes': A map of cookie attribute key-value pairs.
    % - `flags`: A list of cookie flags, represented as binaries.
    % After we have prepared a list of `set-cookie' header lines, we call this
    % function again to potentially join them into a comma-separated string,
    % depending on the `bundle' key in the request message.
    Opts = opts(RawOpts),
    LoadedCookies = hb_cache:ensure_all_loaded(CookiesMsg, Opts),
    to(maps:values(maps:map(fun to_line/2, LoadedCookies)), Req, Opts).

%% @doc Convert a cookie message into a `set-cookie' header line.
%% Note: Assumes that the cookies have all been loaded from the cache fully.
to_line(Key, Value) when is_binary(Value) ->
    <<Key/binary, "=\"", Value/binary, "\"">>;
to_line(Key, Cookie) when is_map(Cookie) ->
    % Encode the cookie key-value pair as a string to use as the base.
    ValueBin =
        <<
            Key/binary, "=\"",
            (maps:get(<<"value">>, Cookie, <<>>))/binary,
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

%% @doc Convert a list of cookie messages or a comma-separated string of cookie
%% messages into a cookie message.
from(CookiesMsg, Req, Opts) when is_binary(CookiesMsg) ->
    % The cookie message is a comma-separated string of cookie header lines.
    % We split it using a string-aware split function (which will not split
    % within quoted strings), then remove any preceding whitespace. Finally, we
    % recurse to convert the lines into `~cookie@1.0' messages.
    from(split(lines, CookiesMsg), Req, opts(Opts));
from(CookiesMsg, _Req, _Opts) when is_list(CookiesMsg) ->
    {ok, maps:from_list(lists:map(fun from_line/1, CookiesMsg))}.

%% @doc Convert a cookie header line into a cookie message.
from_line(Line) ->
    {[Key, Value], Rest} = split(pair, Line),
    % If there is no remaining binary after the pair, we have a simple key-value
    % pair, returning just the binary as the value. Otherwise, we split the
    % remaining binary into attributes and flags and return a message with the
    % value and those parsed elements.
    case Rest of
        <<>> -> {Key, Value};
        _ ->
            AllAttrs = split(attributes, Rest),
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
            MaybeAllAttributes = maps:merge(MaybeAttributes, MaybeFlags),
            {Key, MaybeAllAttributes#{ <<"value">> => Value }}
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

%% Helper
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
split(attributes, Bin) ->
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

%%% Tests

%% @doc returns a map of tuples of the form `testset_name => {[before], after}'.
%% These sets are used to test the correctness of the parsing and serialization
%% of cookie messages. The `before` is a list of inputs for which all of the
%% outputs are expected to match the `after' value.
test_data() ->
    #{
        from_string_raw_value =>
            {
                [<<"k1=v1">>, <<"k1=\"v1\"">>],
                #{ <<"k1">> => <<"v1">> }
            },
        from_string_attributes =>
            {
                [<<"k1=v1; k2=v2">>, <<"k1=\"v1\"; k2=\"v2\"">>],
                #{
                    <<"k1">> =>
                        #{
                            <<"value">> => <<"v1">>,
                            <<"attributes">> => #{ <<"k2">> => <<"v2">> }
                        }
                }
            },
        from_string_flags =>
            {
                [<<"k1=v1; k2=v2; f1; f2">>, <<"k1=\"v1\"; k2=\"v2\"; f1; f2">>],
                #{
                    <<"k1">> =>
                        #{
                            <<"value">> => <<"v1">>,
                            <<"attributes">> => #{ <<"k2">> => <<"v2">> },
                            <<"flags">> => [<<"f1">>, <<"f2">>]
                        }
                }
            },
        to_string_raw_value =>
            {
                [
                    #{ <<"k1">> => <<"v1">> },
                    #{ <<"k1">> => #{ <<"value">> => <<"v1">> } },
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"attributes">> => #{},
                                <<"flags">> => []
                            }
                    }
                ],
                [<<"k1=\"v1\"">>]
            },
        to_string_attributes =>
            {
                [
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"attributes">> => #{ <<"k2">> => <<"v2">> }
                            }
                    },
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"attributes">> => #{ <<"k2">> => <<"v2">> },
                                <<"flags">> => []
                            }
                    }
                ],
                [<<"k1=\"v1\"; k2=v2">>]
            },
        to_string_flags =>
            {
                [
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"flags">> => [<<"f1">>, <<"f2">>]
                            }
                    },
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"attributes">> => #{},
                                <<"flags">> => [<<"f1">>, <<"f2">>]
                            }
                    }
                ],
                [<<"k1=\"v1\"; f1; f2">>]
            },
        parse_realworld_1 =>
            {
                [
                    [
                        <<"cart=110045_77895_53420; SameSite=Strict">>,
                        <<"affiliate=e4rt45dw; SameSite=Lax">>
                    ]
                ],
                #{
                    <<"cart">> =>
                        #{
                            <<"value">> => <<"110045_77895_53420">>,
                            <<"attributes">> => #{ <<"SameSite">> => <<"Strict">> }
                        },
                    <<"affiliate">> =>
                        #{
                            <<"value">> => <<"e4rt45dw">>,
                            <<"attributes">> => #{ <<"SameSite">> => <<"Lax">> }
                        }
                }
            },
        parse_user_settings_and_permissions =>
            {
                [
                    [
                        <<"user_settings=notifications=true,privacy=strict,layout=grid; Path=/; HttpOnly; Secure">>,
                        <<"user_permissions=\"read;write;delete\"; Path=/; SameSite=None; Secure">>
                    ]
                ],
                #{
                    <<"user_settings">> =>
                        #{
                            <<"value">> => <<"notifications=true,privacy=strict,layout=grid">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"HttpOnly">>, <<"Secure">>]
                        },
                    <<"user_permissions">> =>
                        #{
                            <<"value">> => <<"read;write;delete">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">>, <<"SameSite">> => <<"None">> },
                            <<"flags">> => [<<"Secure">>]
                        }
                }
            },
        parse_session_and_temp_data =>
            {
                [
                    [
                        <<"SESSION_ID=abc123xyz ; path= /dashboard ; samesite=Strict ; Secure">>,
                        <<"temp_data=cleanup_me; Max-Age=-1; Path=/">>
                    ]
                ],
                #{
                    <<"SESSION_ID">> =>
                        #{
                            <<"value">> => <<"abc123xyz ">>,
                            <<"attributes">> => #{ <<"path">> => <<"/dashboard">>, <<"samesite">> => <<"Strict">> },
                            <<"flags">> => [<<"Secure">>]
                        },
                    <<"temp_data">> =>
                        #{
                            <<"value">> => <<"cleanup_me">>,
                            <<"attributes">> => #{ <<"Max-Age">> => <<"-1">>, <<"Path">> => <<"/">> }
                        }
                }
            },
        parse_empty_and_anonymous =>
            {
                [
                    [
                        <<"user_preference=; Path=/; HttpOnly">>,
                        <<"=anonymous_session_123; Path=/guest">>
                    ]
                ],
                #{
                    <<"user_preference">> =>
                        #{
                            <<"value">> => <<"">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"HttpOnly">>]
                        },
                    <<>> =>
                        #{
                            <<"value">> => <<"anonymous_session_123">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/guest">> }
                        }
                }
            },
        parse_app_config_and_analytics =>
            {
                [
                    [
                        <<"$app_config$=theme@dark!%20mode; Path=/">>,
                        <<"analytics_session_data_with_very_long_name_for_tracking_purposes=comprehensive_user_behavior_analytics_data_including_page_views_click_events_scroll_depth_time_spent_geographic_location_device_info_browser_details_and_more; Path=/">>
                    ]
                ],
                #{
                    <<"$app_config$">> =>
                        #{
                            <<"value">> => <<"theme@dark!%20mode">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        },
                    <<"analytics_session_data_with_very_long_name_for_tracking_purposes">> =>
                        #{
                            <<"value">> => <<"comprehensive_user_behavior_analytics_data_including_page_views_click_events_scroll_depth_time_spent_geographic_location_device_info_browser_details_and_more">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        }
                }
            },
        parse_debug_and_tracking =>
            {
                [
                    [
                        <<"debug_info=\\tIndented\\t\\nMultiline\\n; Path=/">>,
                        <<"tracking_id=user_12345; CustomAttr=CustomValue; Analytics=Enabled; Path=/; HttpOnly">>
                    ]
                ],
                #{
                    <<"debug_info">> =>
                        #{
                            <<"value">> => <<"\\tIndented\\t\\nMultiline\\n">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        },
                    <<"tracking_id">> =>
                        #{
                            <<"value">> => <<"user_12345">>,
                            <<"attributes">> => #{
                                <<"CustomAttr">> => <<"CustomValue">>,
                                <<"Analytics">> => <<"Enabled">>,
                                <<"Path">> => <<"/">>
                            },
                            <<"flags">> => [<<"HttpOnly">>]
                        }
                }
            },
        parse_cache_and_form_token =>
            {
                [
                    [
                        <<"cache_bust=v1.2.3; Expires=Mon, 99 Feb 2099 25:99:99 GMT; Path=/">>,
                        <<"form_token=form_abc123; SameSite=Strick; Secure">>
                    ]
                ],
                #{
                    <<"cache_bust">> =>
                        #{
                            <<"value">> => <<"v1.2.3">>,
                            <<"attributes">> => #{
                                <<"Expires">> => <<"Mon, 99 Feb 2099 25:99:99 GMT">>,
                                <<"Path">> => <<"/">>
                            }
                        },
                    <<"form_token">> =>
                        #{
                            <<"value">> => <<"form_abc123">>,
                            <<"attributes">> => #{ <<"SameSite">> => <<"Strick">> },
                            <<"flags">> => [<<"Secure">>]
                        }
                }
            },
        parse_token_and_reactions =>
            {
                [
                    [
                        <<"access_token=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c; Path=/; HttpOnly; Secure">>,
                        <<"reaction_prefs=👍👎; Path=/; Secure">>
                    ]
                ],
                #{
                    <<"access_token">> =>
                        #{
                            <<"value">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"HttpOnly">>, <<"Secure">>]
                        },
                    <<"reaction_prefs">> =>
                        #{
                            <<"value">> => <<"👍👎">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"Secure">>]
                        }
                }
            },
        parse_error_log_and_auth_token =>
            {
                [
                    [
                        % ------- Failing because \n is getting tranferred to n\ while parsing
                        <<"error_log=\"timestamp=2024-01-15 10:30:00\\nlevel=ERROR\\tmessage=Database connection failed\"; Path=/">>,
                        % ------- Failing because wrong order of flags
                        <<"auth_token=bearer_xyz789; Secure; Path=/api; Secure; HttpOnly">>
                    ]
                ],
                #{
                    <<"error_log">> =>
                        #{
                            <<"value">> => <<"timestamp=2024-01-15 10:30:00\\nlevel=ERROR\\tmessage=Database connection failed">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        },
                    <<"auth_token">> =>
                        #{
                            <<"value">> => <<"bearer_xyz789">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/api">> },
                            <<"flags">> => [<<"HttpOnly">>,<<"Secure">>, <<"Secure">>]
                        }
                }
            },
        parse_csrf_and_quick_setting =>
            {
                [
                    [
                        % ---- Failing because of \ backlashes in the flag we should remove it
                        <<"csrf_token=abc123; \"HttpOnly\"; Path=/">>,
                        % ----- Failing because parsing as <<"quick_setting">> => <<"enabled">>
                        <<"quick_setting=\"enabled\"">>
                    ]
                ],
                #{
                    <<"csrf_token">> =>
                        #{
                            <<"value">> => <<"abc123">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"HttpOnly">>]
                        },
                    <<"quick_setting">> => <<"enabled">>
                }
            },
        parse_admin_and_upload =>
            {
                [
                    [
                        <<"secret_key=confidential; Path=%2Fadmin">>,
                        % ------ Failing because parsing have <<\"admin_flag\">>
                        <<"admin_flag=true; Path=/">>
                        
                    ]
                ],
                #{
                    <<"secret_key">> =>
                        #{
                            <<"value">> => <<"confidential">>,
                            <<"attributes">> => #{ <<"Path">> => <<"%2Fadmin">> }
                        },
                    <<"admin_flag">> =>
                        #{
                            <<"value">> => <<"true">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        }
                }
            },
        parse_search_and_tags =>
            {
                [
                    [
                        <<"search_history=\"query,results\"; Path=/">>,
                        <<"user_tags=\"work,personal\"; Path=/">>
                    ]
                ],
                #{
                    <<"search_history">> =>
                        #{
                            <<"value">> => <<"query,results">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        },
                    <<"user_tags">> =>
                        #{
                            <<"value">> => <<"work,personal">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        }
                }
            },
        to_string_realworld_1 =>
            {
                [
                    #{
                        <<"cart">> =>
                            #{
                                <<"value">> => <<"110045_77895_53420">>,
                                <<"attributes">> => #{ <<"SameSite">> => <<"Strict">> }
                            },
                        <<"affiliate">> =>
                            #{
                                <<"value">> => <<"e4rt45dw">>,
                                <<"attributes">> => #{ <<"SameSite">> => <<"Lax">> }
                            }
                    }
                ],
                [
                    <<"affiliate=\"e4rt45dw\"; SameSite=Lax">>,
                    <<"cart=\"110045_77895_53420\"; SameSite=Strict">>
                ]
            },
        to_string_user_settings_and_permissions =>
            {
                [
                    #{
                        <<"user_settings">> =>
                            #{
                                <<"value">> => <<"notifications=true,privacy=strict,layout=grid">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"HttpOnly">>, <<"Secure">>]
                            },
                        <<"user_permissions">> =>
                            #{
                                <<"value">> => <<"read;write;delete">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">>, <<"SameSite">> => <<"None">> },
                                <<"flags">> => [<<"Secure">>]
                            }
                    }
                ],
                [
                    <<"user_permissions=\"read;write;delete\"; Path=/; SameSite=None; Secure">>,
                    <<"user_settings=\"notifications=true,privacy=strict,layout=grid\"; Path=/; HttpOnly; Secure">>
                ]
            },
        to_string_session_and_temp_data =>
            {
                [
                    #{
                        <<"SESSION_ID">> =>
                            #{
                                <<"value">> => <<"abc123xyz ">>,
                                <<"attributes">> => #{ <<"path">> => <<"/dashboard">>, <<"samesite">> => <<"Strict">> },
                                <<"flags">> => [<<"Secure">>]
                            },
                        <<"temp_data">> =>
                            #{
                                <<"value">> => <<"cleanup_me">>,
                                <<"attributes">> => #{ <<"Max-Age">> => <<"-1">>, <<"Path">> => <<"/">> }
                            }
                    }
                ],
                [
                    <<"SESSION_ID=\"abc123xyz \"; path=/dashboard; samesite=Strict; Secure">>,
                    <<"temp_data=\"cleanup_me\"; Max-Age=-1; Path=/">>
                ]
            },
        to_string_empty_and_anonymous =>
            {
                [
                    #{
                        <<"user_preference">> =>
                            #{
                                <<"value">> => <<"">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"HttpOnly">>]
                            },
                        <<>> =>
                            #{
                                <<"value">> => <<"anonymous_session_123">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/guest">> }
                            }
                    }
                ],
                [
                    <<"=\"anonymous_session_123\"; Path=/guest">>,
                    <<"user_preference=\"\"; Path=/; HttpOnly">>
                ]
            },
        to_string_app_config_and_analytics =>
            {
                [
                    #{
                        <<"$app_config$">> =>
                            #{
                                <<"value">> => <<"theme@dark!%20mode">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            },
                        <<"analytics_session_data_with_very_long_name_for_tracking_purposes">> =>
                            #{
                                <<"value">> => <<"comprehensive_user_behavior_analytics_data_including_page_views_click_events_scroll_depth_time_spent_geographic_location_device_info_browser_details_and_more">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            }
                    }
                ],
                [
                    <<"$app_config$=\"theme@dark!%20mode\"; Path=/">>,
                    <<"analytics_session_data_with_very_long_name_for_tracking_purposes=\"comprehensive_user_behavior_analytics_data_including_page_views_click_events_scroll_depth_time_spent_geographic_location_device_info_browser_details_and_more\"; Path=/">>
                ]
            },
        to_string_debug_and_tracking =>
            {
                [
                    #{
                        <<"debug_info">> =>
                            #{
                                <<"value">> => <<"\\tIndented\\t\\nMultiline\\n">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            },
                        <<"tracking_id">> =>
                            #{
                                <<"value">> => <<"user_12345">>,
                                <<"attributes">> => #{
                                    <<"CustomAttr">> => <<"CustomValue">>,
                                    <<"Analytics">> => <<"Enabled">>,
                                    <<"Path">> => <<"/">>
                                },
                                <<"flags">> => [<<"HttpOnly">>]
                            }
                    }
                ],
                [
                    <<"debug_info=\"\\tIndented\\t\\nMultiline\\n\"; Path=/">>,
                    <<"tracking_id=\"user_12345\"; Analytics=Enabled; CustomAttr=CustomValue; Path=/; HttpOnly">>
                ]
            },
        to_string_cache_and_form_token =>
            {
                [
                    #{
                        <<"cache_bust">> =>
                            #{
                                <<"value">> => <<"v1.2.3">>,
                                <<"attributes">> => #{
                                    <<"Expires">> => <<"Mon, 99 Feb 2099 25:99:99 GMT">>,
                                    <<"Path">> => <<"/">>
                                }
                            },
                        <<"form_token">> =>
                            #{
                                <<"value">> => <<"form_abc123">>,
                                <<"attributes">> => #{ <<"SameSite">> => <<"Strick">> },
                                <<"flags">> => [<<"Secure">>]
                            }
                    }
                ],
                [
                    <<"cache_bust=\"v1.2.3\"; Expires=Mon, 99 Feb 2099 25:99:99 GMT; Path=/">>,
                    <<"form_token=\"form_abc123\"; SameSite=Strick; Secure">>
                ]
            },
        to_string_token_and_reactions =>
            {
                [
                    #{
                        <<"access_token">> =>
                            #{
                                <<"value">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"HttpOnly">>, <<"Secure">>]
                            },
                        <<"reaction_prefs">> =>
                            #{
                                <<"value">> => <<"👍👎">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"Secure">>]
                            }
                    }
                ],
                [
                    <<"access_token=\"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c\"; Path=/; HttpOnly; Secure">>,
                    <<"reaction_prefs=\"👍👎\"; Path=/; Secure">>
                ]
            },
        to_string_error_log_and_auth_token =>
            {
                [
                    #{
                        <<"error_log">> =>
                            #{
                                <<"value">> => <<"timestamp=2024-01-15 10:30:00\\nlevel=ERROR\\tmessage=Database connection failed">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            },
                        <<"auth_token">> =>
                            #{
                                <<"value">> => <<"bearer_xyz789">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/api">> },
                                <<"flags">> => [<<"HttpOnly">>, <<"Secure">>, <<"Secure">>]
                            }
                    }
                ],
                [
                    <<"auth_token=\"bearer_xyz789\"; Path=/api; HttpOnly; Secure; Secure">>,
                    <<"error_log=\"timestamp=2024-01-15 10:30:00\\nlevel=ERROR\\tmessage=Database connection failed\"; Path=/">>
                ]
            },
        to_string_csrf_and_quick_setting =>
            {
                [
                    #{
                        <<"csrf_token">> =>
                            #{
                                <<"value">> => <<"abc123">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"HttpOnly">>]
                            },
                        <<"quick_setting">> => <<"enabled">>
                    }
                ],
                [
                    <<"csrf_token=\"abc123\"; Path=/; HttpOnly">>,
                    <<"quick_setting=\"enabled\"">>
                ]
            },
        to_string_admin_and_upload =>
            {
                [
                    #{
                        <<"secret_key">> =>
                            #{
                                <<"value">> => <<"confidential">>,
                                <<"attributes">> => #{ <<"Path">> => <<"%2Fadmin">> }
                            },
                        <<"admin_flag">> =>
                            #{
                                <<"value">> => <<"true">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            }
                    }
                ],
                [
                    <<"admin_flag=\"true\"; Path=/">>,
                    <<"secret_key=\"confidential\"; Path=%2Fadmin">>
                ]
            },
        to_string_search_and_tags =>
            {
                [
                    #{
                        <<"search_history">> =>
                            #{
                                <<"value">> => <<"query,results">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            },
                        <<"user_tags">> =>
                            #{
                                <<"value">> => <<"work,personal">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            }
                    }
                ],
                [
                    <<"search_history=\"query,results\"; Path=/">>,
                    <<"user_tags=\"work,personal\"; Path=/">>
                ]
            }
    }.

from_string_basic_test() ->
    assert_set(from_string_raw_value, fun from_string/1).

from_string_attributes_test() ->
    assert_set(from_string_attributes, fun from_string/1).

from_string_flags_test() ->
    assert_set(from_string_flags, fun from_string/1).

to_string_basic_test() ->
    assert_set(to_string_raw_value, fun to_string/1).

to_string_attributes_test() ->
    assert_set(to_string_attributes, fun to_string/1).

to_string_flags_test() ->
    assert_set(to_string_flags, fun to_string/1).

parse_realworld_test() ->
    assert_set(parse_realworld_1, fun from_string/1).

parse_user_settings_and_permissions_test() ->
    assert_set(parse_user_settings_and_permissions, fun from_string/1).

parse_session_and_temp_data_test() ->
    assert_set(parse_session_and_temp_data, fun from_string/1).

parse_empty_and_anonymous_test() ->
    assert_set(parse_empty_and_anonymous, fun from_string/1).

parse_app_config_and_analytics_test() ->
    assert_set(parse_app_config_and_analytics, fun from_string/1).

parse_debug_and_tracking_test() ->
    assert_set(parse_debug_and_tracking, fun from_string/1).

parse_cache_and_form_token_test() ->
    assert_set(parse_cache_and_form_token, fun from_string/1).

parse_token_and_reactions_test() ->
    assert_set(parse_token_and_reactions, fun from_string/1).

parse_error_log_and_auth_token_test() ->
    assert_set(parse_error_log_and_auth_token, fun from_string/1).

parse_csrf_and_quick_setting_test() ->
    assert_set(parse_csrf_and_quick_setting, fun from_string/1).

parse_admin_and_upload_test() ->
    assert_set(parse_admin_and_upload, fun from_string/1).

parse_search_and_tags_test() ->
    assert_set(parse_search_and_tags, fun from_string/1).

to_string_realworld_1_test() ->
    assert_set(to_string_realworld_1, fun to_string/1).

to_string_user_settings_and_permissions_test() ->
    assert_set(to_string_user_settings_and_permissions, fun to_string/1).

to_string_session_and_temp_data_test() ->
    assert_set(to_string_session_and_temp_data, fun to_string/1).

to_string_empty_and_anonymous_test() ->
    assert_set(to_string_empty_and_anonymous, fun to_string/1).

to_string_app_config_and_analytics_test() ->
    assert_set(to_string_app_config_and_analytics, fun to_string/1).

to_string_debug_and_tracking_test() ->
    assert_set(to_string_debug_and_tracking, fun to_string/1).

to_string_cache_and_form_token_test() ->
    assert_set(to_string_cache_and_form_token, fun to_string/1).

to_string_token_and_reactions_test() ->
    assert_set(to_string_token_and_reactions, fun to_string/1).

to_string_error_log_and_auth_token_test() ->
    assert_set(to_string_error_log_and_auth_token, fun to_string/1).

to_string_csrf_and_quick_setting_test() ->
    assert_set(to_string_csrf_and_quick_setting, fun to_string/1).

to_string_admin_and_upload_test() ->
    assert_set(to_string_admin_and_upload, fun to_string/1).

to_string_search_and_tags_test() ->
    assert_set(to_string_search_and_tags, fun to_string/1).

%%% Test Helpers

%% @doc Convert a cookie message to a string.
to_string(CookieMsg) ->
    {ok, Cookie} = to(CookieMsg, #{}, #{}),
    Cookie.

%% @doc Convert a string to a cookie message.
from_string(String) ->
    {ok, Cookie} = from(String, #{}, #{}),
    Cookie.

%% @doc Assert that when given the inputs in the test set, the outputs are
%% all equal to the expected value when the function is applied to them.
assert_set(TestSet, Fun) ->
    {Inputs, Expected} = maps:get(TestSet, test_data()),
    lists:foreach(
        fun(Input) ->
            Res = Fun(Input),
            ?event(cookie, {output, {explicit, Res}}),
            ?assertEqual(Expected, Res)
        end,
        Inputs
    ).
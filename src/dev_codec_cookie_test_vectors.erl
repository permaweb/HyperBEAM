%%% @doc A battery of cookie parsing and encoding test vectors.
-module(dev_codec_cookie_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Test Helpers

%% @doc Assert that when given the inputs in the test set, the outputs are
%% all equal to the expected value when the function is applied to them.
assert_set(TestSet, Fun) ->
    {Inputs, Expected} = maps:get(TestSet, test_data()),
    ?event(match_cookie, {starting_group_match, {inputs, {explicit, Inputs}}}),
    lists:foreach(
        fun(Input) ->
            Res = Fun(Input),
            ?event(
                match_cookie,
                {matching,
                    {expected, {explicit, Expected}, {output, {explicit, Res}}}
                }
            ),
            ?assertEqual(Expected, Res)
        end,
        Inputs
    ).

%% @doc Convert a cookie message to a string.
to_string(CookieMsg) ->
    {ok, BaseMsg} = dev_codec_cookie:store(#{}, CookieMsg, #{}),
    {ok, Msg} =
        dev_codec_cookie:to(
            BaseMsg,
            #{ <<"format">> => <<"set-cookie">> },
            #{}
        ),
    hb_maps:get(<<"set-cookie">>, Msg, [], #{}).

%% @doc Convert a string to a cookie message.
from_string(String) ->
    {ok, BaseMsg} =
        dev_codec_cookie:from(
            #{ <<"set-cookie">> => String },
            #{},
            #{}
        ),
    {ok, Cookie} = dev_codec_cookie:extract(BaseMsg, #{}, #{}),
    Cookie.

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
                            <<"value">> => <<"theme@dark! mode">>,
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
                        <<"error_log=\"timestamp=2024-01-15 10:30:00\\nlevel=ERROR\\tmessage=Database connection failed\"; Path=/">>,
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
                        <<"csrf_token=abc123; \"HttpOnly\"; Path=/">>,
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
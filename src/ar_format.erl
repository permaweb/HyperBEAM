-module(ar_format).
-export([format/1, format/2, format/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

% How many bytes of a binary to print with `print/1'.
-define(BIN_PRINT, 20).
-define(INDENT_SPACES, 2).

%%%===================================================================
%%% Public interface.
%%%===================================================================

format(TX) -> format(TX, 0).
format(TX, Indent) -> format(TX, Indent, #{}).
format(TX, Indent, Opts) when is_list(TX); is_map(TX) ->
    format(ar_bundles:normalize(TX), Indent, Opts);
format(TX, Indent, Opts) when is_record(TX, tx) ->
    MustVerify = hb_opts:get(debug_ids, true, Opts),
    Valid =
        if MustVerify -> verify(TX);
        true -> true
        end,
    UnsignedID =
        if MustVerify -> hb_util:encode(id(TX, unsigned));
        true -> <<"[SKIPPED ID]">>
        end,
    SignedID =
        if MustVerify ->
            case id(TX, signed) of
                not_signed -> <<"[NOT SIGNED]">>;
                ID -> hb_util:encode(ID)
            end;
        true -> <<"[SKIPPED ID]">>
        end,
    format_line(
        "TX ( ~s: ~s ) {",
        [
            if
                MustVerify andalso TX#tx.signature =/= ?DEFAULT_SIG ->
                    lists:flatten(
                        io_lib:format(
                            "~s (signed) ~s (unsigned)",
                            [SignedID, UnsignedID]
                        )
                    );
                true -> UnsignedID
            end,
            if
                not MustVerify -> "[SKIPPED VERIFICATION]";
                Valid == true -> "[SIGNED+VALID]";
                true -> "[UNSIGNED/INVALID]"
            end
        ],
        Indent
    ) ++
    case MustVerify andalso (not Valid) andalso TX#tx.signature =/= ?DEFAULT_SIG of
        true ->
            format_line("!!! CAUTION: ITEM IS SIGNED BUT INVALID !!!", Indent + 1);
        false -> []
    end ++
    case ar_bundles:is_signed(TX) of
        true ->
            format_line("Signer: ~s",
                [hb_util:encode(ar_bundles:signer(TX))], 
                Indent + 1),
            format_line("Signature: ~s", 
                [hb_format:binary(TX#tx.signature)],
                Indent + 1);
        false -> []
    end ++
    format_fields(TX, Indent) ++
    format_line("Tags:", Indent + 1) ++
    lists:map(
        fun({Key, Val}) -> format_line("~s -> ~s", [Key, Val], Indent + 2) end,
        TX#tx.tags
    ) ++
    format_line("Data:", Indent + 1) ++ format_data(TX, Indent + 2) ++
    format_line("}", Indent);
format(TX, Indent, _Opts) ->
    % Whatever we have, its not a tx...
    format_line("INCORRECT ITEM: ~p", [TX], Indent).

format_data(#tx{ format = ans104 } = TX, Indent) when is_binary(TX#tx.data) ->
    case lists:keyfind(<<"bundle-format">>, 1, TX#tx.tags) of
        {_, _} ->
            format_data(ar_bundles:deserialize(ar_bundles:serialize(TX)), Indent);
        false ->
            format_line(
                "Binary: ~p... <~p bytes>",
                [format_binary(TX#tx.data), byte_size(TX#tx.data)],
                Indent
            )
    end;
format_data(TX, Indent) when is_binary(TX#tx.data) ->
    format_line(
        "Binary: ~p... <~p bytes>",
        [format_binary(TX#tx.data), byte_size(TX#tx.data)],
        Indent
    );
format_data(TX, Indent) when is_map(TX#tx.data) ->
    format_line("Map:", Indent) ++
    lists:map(
        fun({Name, MapItem}) ->
            format_line("~s ->", [Name], Indent + 1) ++
            format(MapItem, Indent + 2)
        end,
        maps:to_list(TX#tx.data)
    );
format_data(TX, Indent) when is_list(TX#tx.data) ->
    format_line("List:", Indent) ++
    lists:map(
        fun(ListItem) ->
            format(ListItem, Indent + 1)
        end,
        TX#tx.data
    ).

format_fields(#tx{ format = ans104 } = TX, Indent) ->
    format_target(TX, Indent) ++
    format_anchor(TX, Indent);
format_fields(TX, Indent) ->
    format_format(TX, Indent) ++
    format_target(TX, Indent) ++
    format_anchor(TX, Indent) ++
    format_quantity(TX, Indent) ++
    format_reward(TX, Indent) ++
    format_data_root(TX, Indent).

format_format(TX, Indent) ->
    format_line("Format: ~p", [TX#tx.format], Indent + 1).

format_target(TX, Indent) ->
    format_line("Target: ~s", [
        case TX#tx.target of
            <<>> -> "[NONE]";
            Target -> hb_util:id(Target)
        end
    ], Indent + 1).

format_anchor(TX, Indent) ->
    format_line("Anchor: ~s", [
        case TX#tx.anchor of
            ?DEFAULT_ANCHOR -> "[NONE]";
            Anchor -> hb_util:encode(Anchor)
        end
    ], Indent + 1).

format_quantity(TX, Indent) ->
    format_line("Quantity: ~p", [TX#tx.quantity], Indent + 1).

format_reward(TX, Indent) ->
    format_line("Reward: ~p", [TX#tx.reward], Indent + 1).

format_data_root(TX, Indent) ->
    format_line("Data Root: ~s", [
        case TX#tx.data_root of
            ?DEFAULT_DATA_ROOT -> "[NONE]";
            DataRoot -> hb_util:encode(DataRoot)
        end
    ], Indent + 1).

format_binary(Bin) ->
    lists:flatten(
        io_lib:format(
            "~p",
            [
                binary:part(
                    Bin,
                    0,
                    case byte_size(Bin) of
                        X when X < ?BIN_PRINT -> X;
                        _ -> ?BIN_PRINT
                    end
                )
            ]
        )
    ).

format_line(Str, Indent) -> format_line(Str, "", Indent).
format_line(RawStr, Fmt, Ind) ->
    io_lib:format(
        [$\s || _ <- lists:seq(1, Ind * ?INDENT_SPACES)] ++
            lists:flatten(RawStr) ++ "\n",
        Fmt
    ).

verify(#tx{ format = ans104 } = TX) ->
    ar_bundles:verify_item(TX);
verify(TX) ->
    ar_tx:verify(TX).

id(#tx{ format = ans104 } = TX, Type) ->
    ar_bundles:id(TX, Type);
id(TX, Type) ->
    ar_tx:id(TX, Type).
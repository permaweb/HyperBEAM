%%% @doc Shared token account helpers.
%%% These helpers define token balance account semantics: external IDs are
%%% validated as raw binaries, then canonicalized only when mapped onto balance
%%% trie keys.
-module(lib_token).

-export([validate_address/3, account_key/1]).

%% @doc Validate an address before it is used as a token balance account.
%% Validation checks the raw external ID and its canonical balance key form so
%% mixed-case input cannot collide with AO/trie control keys after
%% canonicalization.
validate_address(Address, CustomList, Opts) when is_binary(Address), is_list(CustomList) ->
    AccountKey = account_key(Address),
    CanonicalCustomKeys = [account_key(Key) || Key <- CustomList, is_binary(Key)],
    case byte_size(Address) of
        0 -> {error, <<"Address cannot be empty.">>};
        N when N > 128 -> {error, <<"Address is too long.">>};
        _ ->
            TrieReservedKeys = trie_reserved_keys(Opts),
            maybe
                true ?= (AccountKey =/= <<"path">>)
                    orelse {error, <<"Address uses the reserved path key.">>},
                true ?= (not is_device_key(AccountKey, Opts))
                    orelse {error, <<"Address uses a reserved device key.">>},
                true ?= (not is_reserved_trie_key(Address, TrieReservedKeys))
                    orelse {error, <<"Address uses a reserved trie internal key.">>},
                true ?= (not is_reserved_trie_key(AccountKey, TrieReservedKeys))
                    orelse {error, <<"Address uses a reserved trie internal key.">>},
                true ?= (not is_reserved_custom_key(Address, CustomList))
                    orelse {error, <<"Address is a reserved custom key.">>},
                true ?= (not is_reserved_custom_key(AccountKey, CanonicalCustomKeys))
                    orelse {error, <<"Address is a reserved custom key.">>},
                true ?= valid_address_chars(Address)
                    orelse {error, <<"Address contains unsupported characters.">>}
            end
    end;
validate_address(_, _, _) ->
    {error, <<"Address must be a binary.">>}.

%% @doc Canonical token balance trie key for an external account ID.
account_key(Address) ->
    hb_util:to_lower(hb_ao:normalize_key(Address)).

is_device_key(Key, Opts) ->
    lists:any(
        fun(Device) ->
            case hb_device:message_to_fun(
                #{<<"device">> => Device}, Key, Opts
            ) of
                {ok, _, _} -> true;
                {add_key, _, _} -> false
            end
        end,
        [<<"message@1.0">>, <<"trie@1.0">>]
    ).

%% Trie metadata keys are not exposed as device operations.
trie_reserved_keys(Opts) ->
    {ok, Trie} = hb_device_load:reference(<<"trie@1.0">>, Opts),
    maps:get(reserved, Trie:info(), []).

is_reserved_trie_key(Key, ReservedKeys) ->
    lists:member(Key, ReservedKeys).

is_reserved_custom_key(Key, List) when is_binary(Key), is_list(List) ->
    lists:member(Key, List);
is_reserved_custom_key(_, _) ->
    false.

%% @doc Return true when every byte is in the supported account alphabet.
valid_address_chars(<<>>) ->
    true;
valid_address_chars(<<Char, Rest/binary>>) when
        Char >= $A, Char =< $Z;
        Char >= $a, Char =< $z;
        Char >= $0, Char =< $9;
        Char =:= $_;
        Char =:= $- ->
    valid_address_chars(Rest);
valid_address_chars(_) ->
    false.
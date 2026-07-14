%%% @doc Shared token account helpers.
%%% These helpers define token balance account semantics: external IDs are
%%% validated as raw binaries, then canonicalized only when mapped onto balance
%%% trie keys.
-module(lib_token).

-export([validate_address/2, validate_address/3, account_key/1]).

%% @doc Built-in reserved keys for address validation.
-define(AO_RESERVED_ADDRESS_KEYS,
    [
        <<"path">>,
        <<"get">>,
        <<"set">>,
        <<"remove">>,
        <<"verify">>,
        <<"keys">>,
        <<"id">>,
        <<"commit">>,
        <<"committed">>,
        <<"committers">>,
        <<"index">>,
        <<"info">>,
        <<"set_path">>,
        <<"reserved_keys">>,
        <<"is_reserved_key">>,
        <<"dedup">>,
        <<"dedup-subject">>
    ]
).

%% @doc Validate an address before it is used as a token balance account.
%% Validation checks the raw external ID and its canonical balance key form so
%% mixed-case input cannot collide with AO/trie control keys after
%% canonicalization.
validate_address(Address, CustomList) ->
    validate_address(Address, CustomList, #{}).

validate_address(Address, CustomList, Opts) when is_binary(Address), is_list(CustomList) ->
    ReservedKeys = ?AO_RESERVED_ADDRESS_KEYS ++ CustomList,
    AccountKey = account_key(Address),
    CanonicalReservedKeys = [account_key(Key) || Key <- ReservedKeys, is_binary(Key)],
    case byte_size(Address) of
        0 -> {error, <<"Address cannot be empty.">>};
        N when N > 128 -> {error, <<"Address is too long.">>};
        _ ->
            TrieReservedKeys = trie_reserved_keys(Opts),
            maybe
                true ?= (not is_reserved_trie_key(Address, TrieReservedKeys))
                    orelse {error, <<"Address uses a reserved trie internal key.">>},
                true ?= (not is_reserved_trie_key(AccountKey, TrieReservedKeys))
                    orelse {error, <<"Address uses a reserved trie internal key.">>},
                true ?= (not is_reserved_custom_key(Address, ReservedKeys))
                    orelse {error, <<"Address is a reserved ao/custom key">>},
                true ?= (not is_reserved_custom_key(AccountKey, CanonicalReservedKeys))
                    orelse {error, <<"Address is a reserved ao/custom key">>},
                true ?= valid_address_chars(Address)
                    orelse {error, <<"Address contains unsupported characters.">>}
            end
    end;
validate_address(_, _, _) ->
    {error, <<"Address must be a binary.">>}.

%% @doc Canonical token balance trie key for an external account ID.
account_key(Address) when is_binary(Address) ->
    hb_util:to_lower(Address).

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
%%%-------------------------------------------------------------------
%%% @doc lapee_hashpath — minimal AO-Core hashpath primitive.
%%%
%%% Each step extends a running merkle chain:
%%%   tip_0   = seed
%%%   tip_n+1 = SHA-256(tip_n || SHA-256(canonical(name || value)))
%%%
%%% The event records the (name, value, value_hash, prev_tip, new_tip)
%%% tuple so a verifier can replay the chain without running the
%%% original code.
%%%-------------------------------------------------------------------
-module(lapee_hashpath).

-export([new/1, extend/3, tip/1, to_json/1]).

-record(hp, {seed :: binary(), events = [] :: [map()], tip :: binary()}).

new(Seed) when is_binary(Seed), byte_size(Seed) =:= 32 ->
    #hp{seed = Seed, tip = Seed}.

extend(#hp{events = Evs, tip = Prev} = HP, Name, Value)
  when is_binary(Name) ->
    %% Match reference Python ao_core.HashPath.extend exactly:
    %%   value_hash  = SHA-256(canonical_bytes(value))
    %%   name_hash   = SHA-256(utf8(name))
    %%   event_hash  = SHA-256(name_hash || value_hash)
    %%   new_tip     = SHA-256(prev_tip || event_hash)
    ValueBytes = canonical(Value),
    ValueHash = crypto:hash(sha256, ValueBytes),
    NameHash = crypto:hash(sha256, Name),
    EventHash = crypto:hash(sha256, <<NameHash/binary, ValueHash/binary>>),
    NewTip = crypto:hash(sha256, <<Prev/binary, EventHash/binary>>),
    Event = #{
        name => Name,
        value => Value,
        value_hash => hex(ValueHash),
        prev_tip => hex(Prev),
        new_tip => hex(NewTip)
    },
    HP#hp{events = Evs ++ [Event], tip = NewTip}.

tip(#hp{tip = T}) -> T.

to_json(#hp{seed = S, events = Evs, tip = T}) ->
    #{
        seed => hex(S),
        tip => hex(T),
        events => Evs
    }.

canonical(M) when is_map(M) ->
    %% Deterministic JSON encoding: sort keys recursively.
    iolist_to_binary(json_encode_sorted(M));
canonical(B) when is_binary(B) -> B;
canonical(I) when is_integer(I) -> integer_to_binary(I);
canonical(L) when is_list(L) -> iolist_to_binary(json_encode_sorted(L)).

json_encode_sorted(V) ->
    json:encode(V, fun sort_encoder/2).

sort_encoder(Map, Encode) when is_map(Map) ->
    Sorted = lists:sort(maps:to_list(Map)),
    json:encode_key_value_list(Sorted, Encode);
sort_encoder(Atom, _) when is_atom(Atom) ->
    <<$", (atom_to_binary(Atom))/binary, $">>;
sort_encoder(Other, Enc) ->
    json:encode_value(Other, Enc).

hex(B) -> binary:encode_hex(B, lowercase).

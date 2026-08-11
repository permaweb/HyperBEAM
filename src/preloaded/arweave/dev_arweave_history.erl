%%% @doc An AO-Core interface to Arweave's two carried histories: the reward
%%% history and the block-time history, each a persistent linked list of
%%% immutable entries.
%%%
%%% A history is identified by its newest entry, which is the message this
%%% device's keys take as their base. `lib_arweave_history' owns the
%%% representation and the rules that bound a history's length; this device is
%%% the surface over it.
%%%
%%% Four keys, which are the four things anything does to a history.
%%% `~arweave@2.9/bootstrap' hands the bytes a peer served to `from-binary/3',
%%% which is where a fetched history becomes a stored one, and `to-binary/3'
%%% renders one back into the exact wire form the vendored decoders read.
%%% `take/3' reads a history back as the newest-first list every consensus rule
%%% consumes, following one link per entry it returns, so the newest few entries
%%% of a 64,850-entry history cost the few and not the 64,850. `push/3' extends
%%% one by a single element.
%%%
%%% `push/3' does not decide what the next element is, and cannot: both elements
%%% are derived from a validated block by the vendored rule that owns them, and
%%% `~arweave-block@2.9/apply' is what runs those rules. What this key adds is
%%% the ability to build and extend a history without going through a block at
%%% all, which is what makes the representation testable on its own terms.
-module(dev_arweave_history).
-implements(<<"arweave-history@2.9">>).
-device_libraries([lib_arweave_history]).
-export([info/1, take/3, push/3, from_binary/3, to_binary/3]).
-include("include/hb.hrl").

%% @doc Export only the history operations, leaving message manipulation to
%% `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Return the newest `count' entries of the history, newest first, and how
%% many the history holds. `count' defaults to the whole history.
take(Base, Req, Opts) ->
    Length = lib_arweave_history:length(Base, Opts),
    Count = hb_util:int(get_first(<<"count">>, Base, Req, Length, Opts)),
    {ok,
        #{
            <<"length">> => Length,
            <<"entries">> =>
                lib_arweave_history:entries(Base, min(Count, Length), Opts)
        }
    }.

%% @doc Extend a history with one value and return its new head.
%%
%% The base is the history's newest entry, or a message naming only the kind
%% when the history is empty. The value's own fields are read from the request
%% alone: an entry carries exactly the field names of the value being appended,
%% so falling back to the base would silently append the entry already there.
%%
%% `height' is what bounds the history's length, and is the height of the block
%% the element belongs to.
push(Base, Req, Opts) ->
    Kind = required(<<"kind">>, Base, Req, Opts),
    Height = hb_util:int(required(<<"height">>, Base, Req, Opts)),
    case lib_arweave_history:from_message(Kind, Req, Opts) of
        {ok, Value} ->
            {ok,
                lib_arweave_history:append(
                    Kind, Value, Height, head(Base, Opts), Opts)
            };
        {error, Message} ->
            {error, error_message(Message,
                <<"A history is either a reward history or a block-time "
                    "history.">>)}
    end.

%% @doc The history a push extends: the base when it is an entry, and nothing
%% when it merely names the kind. An entry is what carries a `length', because
%% every entry records the length of the history ending at it.
head(Base, Opts) ->
    case hb_maps:is_key(<<"length">>, Base, Opts) of
        true -> Base;
        false -> []
    end.

%% @doc Build a history from the binary form `/reward_history/<BH>' and
%% `/block_time_history/<BH>' serve, returning its newest entry.
%%
%% `height' is required rather than defaulted: it is what decides how far back
%% the consensus rules read, and a history built to the wrong length would be
%% wrong in a way no later check names.
from_binary(Base, Req, Opts) ->
    Kind = required(<<"kind">>, Base, Req, Opts),
    Height = hb_util:int(required(<<"height">>, Base, Req, Opts)),
    Body = body(Base, Req, Opts),
    case lib_arweave_history:from_binary(Kind, Body, Height, Opts) of
        {ok, Head} ->
            {ok, Head};
        {error, Message} ->
            {error, error_message(Message,
                <<"The body is not a whole number of history entries.">>)}
    end.

%% @doc Serialize a history into the binary form a peer serves it in, which is
%% the form `from-binary/3' reads and the one the vendored decoders parse. The
%% round trip is exact, so a history built from a peer's bytes renders back to
%% the same bytes.
to_binary(Base, _Req, Opts) ->
    {ok, #{ <<"body">> => lib_arweave_history:to_binary(Base, Opts) }}.

%%% Internal functions.

%% @doc Read the wire document a codec key operates on.
body(Base, Req, Opts) ->
    get_first(<<"body">>, Base, Req, <<>>, Opts).

%% @doc Read a field from the request, falling back to the base message.
%%
%% These are data fields, not keys to resolve, so they are read with
%% `hb_maps:get/4' rather than `hb_ao:get/4': this device names a key after a
%% field its own entries carry, and resolving one against a message that names
%% this device would dispatch back into the device.
get_first(Key, Base, Req, Default, Opts) ->
    case hb_maps:get(Key, Req, not_found, Opts) of
        not_found -> hb_maps:get(Key, Base, Default, Opts);
        Value -> Value
    end.

%% @doc Read a key that has no meaningful default.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({missing_key, Key});
        Value -> Value
    end.

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.

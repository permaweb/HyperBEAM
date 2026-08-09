%%% @doc An AO-Core interface to Arweave's two carried histories: the reward
%%% history and the block-time history, each a persistent linked list of
%%% immutable entries.
%%%
%%% A history is identified by its newest entry, which is the message this
%%% device's keys take as their base. `lib_arweave_history' owns the
%%% representation and the rules that bound a history's length; this device is
%%% the surface over it.
%%%
%%% Two keys, because two callers need them. `~arweave@2.9/bootstrap' hands the
%%% bytes a peer served to `from-binary/3', which is where a fetched history
%%% becomes a stored one. `take/3' reads a history back, following one link per
%%% entry it returns, so the newest few entries of a 64,850-entry history cost
%%% the few and not the 64,850.
%%%
%%% Extending a history belongs to `~arweave-block@2.9/apply', which is the only
%%% thing entitled to decide what the next element is: both elements are derived
%%% from a validated block by the vendored rule that owns them, so there is no
%%% element for a caller to supply and no key here that takes one.
-module(dev_arweave_history).
-implements(<<"arweave-history@2.9">>).
-device_libraries([lib_arweave_history]).
-export([info/1, take/3, from_binary/3]).
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

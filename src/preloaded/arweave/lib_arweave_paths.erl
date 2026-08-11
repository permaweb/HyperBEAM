%%% @doc The store layout of the Arweave consensus cache.
%%%
%%% Four durable names, all under this device's own path namespace. The
%%% namespace is what makes them mean something: everything filed here was
%%% written by `~arweave@2.9' after checking it, so `validated' can answer from
%%% it and a walk of `previous' links stays inside the chain this node
%%% validated. `~arweave@2.9/block/...' -- singular, and a different prefix --
%%% is the gateway cache, which holds peer claims nothing has checked.
%%%
%%% They live in one module because two devices build them. A block's `previous'
%%% link is written by `~arweave-block@2.9' and followed by `~arweave@2.9', so a
%%% path spelled out in each would be a path that can drift, and drift here is a
%%% chain that cannot be walked.
%%%
%%% Every name is built from a value that arrived from a peer or a request, and
%%% `hb_path:to_binary/1' does not collapse `..': `hb_store_fs' walks the
%%% components and hands the traversal to the OS. So each builder refuses a
%%% value that could be resolved as a path rather than passing it on. The
%%% callers that take one from a request check it first -- see
%%% `dev_arweave_sync:block_hash/1' -- and these are the chokepoints behind
%%% them.
-module(lib_arweave_paths).
-export([tip/0, block/1, placement/1, settled/1, accounts_anchor/0]).
-include("include/hb.hrl").

-define(PREFIX, <<"~arweave@2.9">>).

%% @doc The selected head of the chain.
tip() ->
    hb_path:to_binary([?PREFIX, <<"tip">>]).

%% @doc A validated block, by its Arweave block hash. Publication links this
%% last, so a block that reads back here is one whose local indexes are
%% complete.
block(Hash) ->
    hb_path:to_binary([?PREFIX, <<"blocks">>, safe(Hash)]).

%% @doc The current placement of a transaction, by its identifier. A
%% reorganisation replaces this; it deletes nothing.
placement(ID) ->
    hb_path:to_binary([?PREFIX, <<"placements">>, safe(ID)]).

%% @doc The marker recording that a block's transactions have been announced on
%% the settled-transaction hook.
settled(Hash) ->
    hb_path:to_binary([?PREFIX, <<"settled">>, safe(Hash)]).

%% @doc The account tree a bootstrap fetched, and the block that vouches for it.
accounts_anchor() ->
    hb_path:to_binary([?PREFIX, <<"accounts-anchor">>]).

%%% Internal functions.

%% @doc Refuse a value that could be resolved as a path rather than treated as
%% a name. Every identifier this subsystem files under is base64url and carries
%% none of these.
safe(Value) when is_binary(Value) ->
    case binary:match(Value, [<<"/">>, <<"..">>, <<0>>]) of
        nomatch -> Value;
        _ -> throw({unsafe_arweave_path, Value})
    end;
safe(Value) ->
    throw({unsafe_arweave_path, Value}).

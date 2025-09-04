%%% @doc A device for orchestrating indexing of messages from foreign sources
%%% into a HyperBEAM node's caches.
%%% 
%%% Supported sources of messages are as follows:
%%% - A remote Arweave GraphQL endpoint.
%%% - A remote Arweave node.
%%% Each source is implemented as a separate engine, with `dev_copycat_[ENGINE]'
%%% as the module name.
-module(dev_copycat).
-export([graphql/3, arweave/3]).

%% @doc Fetch data from a GraphQL endpoint for replication. See 
%% `dev_copycat_graphql' for implementation details.
graphql(Base, Request, Opts) ->
    dev_copycat_graphql:graphql(Base, Request, Opts).

%% @doc Fetch data from an Arweave node for replication. See `dev_copycat_arweave'
%% for implementation details.
arweave(Base, Request, Opts) ->
    dev_copycat_arweave:arweave(Base, Request, Opts).
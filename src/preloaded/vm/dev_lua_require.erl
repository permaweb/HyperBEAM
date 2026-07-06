%%% @doc A minimal, disk-free replacement for Lua's `require' in the
%%% `lua@5.3a' sandbox.
%%%
%%% Luerl's native `require' falls back to loading modules from the host
%%% filesystem (via `package.path'/`package.searchers', which call
%%% `luerl_comp:file'). That is an escape vector for untrusted code: emptying
%%% `package.path' does not close it, because `package.path' is a mutable Lua
%%% field and the searcher reads it afresh on each call.
%%%
%%% This installs a `require' that mirrors what Luerl does internally --
%%% returning a cached module from `package.loaded', otherwise running its
%%% `package.preload' loader and caching the result -- but never touches the
%%% disk. It pairs with sandboxing `package.searchers' and `package.searchpath'
%%% so the native filesystem searcher cannot be invoked directly either.
-module(dev_lua_require).
-export([install/2]).
-include("include/hb.hrl").

%% @doc Replace the native `require' in the given Lua state with the disk-free
%% implementation, returning the updated state.
install(State, Opts) ->
    {ok, _, State1} = luerl:do_dec(require_src(), State),
    ?event(debug_lua, installed_sandboxed_require, Opts),
    State1.

%% @doc The Lua source of the sandboxed `require'. Resolves modules only from
%% `package.loaded' and `package.preload', exactly as Luerl's `do_require'
%% does before it reaches the filesystem searcher.
require_src() ->
    <<
        """
        function require(name)
            local cached = package.loaded[name]
            if cached ~= nil then return cached end
            local loader = package.preload[name]
            if loader == nil then
                error("module '" .. tostring(name) .. "' not found")
            end
            local mod = loader(name)
            if mod == nil then mod = true end
            package.loaded[name] = mod
            return mod
        end
        """
    >>.

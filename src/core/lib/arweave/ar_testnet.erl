-module(ar_testnet).

-export([is_testnet/0, height_testnet_fork/0,
		locked_rewards_blocks/1, reward_history_blocks/1, target_block_time/1,
		legacy_reward_history_blocks/1]).

-include("include/ar.hrl").
-include("include/ar_pricing.hrl").

%% VENDOR: upstream wraps every function below in -ifdef(TESTNET) and carries a
%% parallel set of ?TESTNET_* constants selecting the testnet values. HyperBEAM
%% only ever validates mainnet, so the testnet branches and the ?TESTNET_*
%% defines are resolved away here and only the mainnet (-else.) clauses are
%% kept. Every exported signature is unchanged, so callers are untouched.

is_testnet() -> false.

height_testnet_fork() ->
	infinity.

%% VENDOR: upstream first consults application:get_env(arweave,
%% locked_rewards_blocks) and falls back to locked_rewards_blocks2/1. HyperBEAM
%% has no arweave application environment, so the mainnet constant is returned
%% unconditionally and the helper is folded in.
locked_rewards_blocks(_Height) ->
	?LOCKED_REWARDS_BLOCKS.

reward_history_blocks(_Height) ->
	?REWARD_HISTORY_BLOCKS.

legacy_reward_history_blocks(_Height) ->
	?LEGACY_REWARD_HISTORY_BLOCKS.

target_block_time(_Height) ->
	?TARGET_BLOCK_TIME.

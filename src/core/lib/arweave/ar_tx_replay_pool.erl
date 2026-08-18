%%% @doc Validate a block's transactions against its consensus context.
-module(ar_tx_replay_pool).

-export([verify_block_txs/1]).

-include("include/ar.hrl").

%% VENDOR: upstream's #tx.last_tx is spelled #tx.anchor in HyperBEAM's ar.hrl.
%% The field is the same one - the previous transaction of the same wallet or a
%% recent block hash - so every upstream TX#tx.last_tx below reads TX#tx.anchor.

%% @doc Verify the transactions are valid for the block taken into account
%% the given current difficulty and height, the previous blocks' wallet list,
%% and recent weave transactions.
verify_block_txs(
			{TXs, Rate, PricePerGiBMinute, KryderPlusRateMultiplier, Denomination, Height,
			 RedenominationHeight, Timestamp, Wallets, BlockAnchors, RecentTXMap}) ->
	verify_block_txs(TXs,
			{Rate, PricePerGiBMinute, KryderPlusRateMultiplier, Denomination,
			 Height, RedenominationHeight, Timestamp, Wallets, BlockAnchors, RecentTXMap,
			 maps:new(), 0, 0}).

verify_block_txs([], _Args) ->
	valid;
verify_block_txs([TX | TXs],
			{Rate, PricePerGiBMinute, KryderPlusRateMultiplier, Denomination, Height,
			 RedenominationHeight, Timestamp, Wallets, BlockAnchors, RecentTXMap,
			 Mempool, C, Size}) when
		is_record(TX, tx),
		is_map(Wallets),
		is_list(BlockAnchors),
		is_map(RecentTXMap),
		is_map(Mempool) ->
	case verify_tx2({TX, Rate, PricePerGiBMinute, KryderPlusRateMultiplier, Denomination,
			Height, RedenominationHeight, Timestamp, Wallets, BlockAnchors, RecentTXMap,
			Mempool, verify_signature}) of
		valid ->
			NewMempool = maps:put(TX#tx.id, no_tx, Mempool),
			NewWallets = ar_node_utils:apply_tx(Wallets, Denomination, TX),
			NewSize =
				case TX of
					#tx{ format = 1 } ->
						Size + TX#tx.data_size;
					_ ->
						Size
				end,
			NewCount = C + 1,
			AboveFork1_8 = Height >= ar_fork:height_1_8(),
			CountExceedsLimit = NewCount > ?BLOCK_TX_COUNT_LIMIT,
			SizeExceedsLimit = NewSize > ?BLOCK_TX_DATA_SIZE_LIMIT,
			case {AboveFork1_8, CountExceedsLimit, SizeExceedsLimit} of
				{true, true, _} ->
					invalid;
				{true, _, true} ->
					invalid;
				_ ->
					verify_block_txs(TXs,
							{Rate, PricePerGiBMinute, KryderPlusRateMultiplier,
							 Denomination, Height, RedenominationHeight, Timestamp, NewWallets,
							 BlockAnchors, RecentTXMap, NewMempool, NewCount, NewSize})
			end;
		{invalid, _} ->
			invalid
	end.

%%%===================================================================
%%% Private functions.
%%%===================================================================

verify_tx2(Args) ->
	{TX, Rate, PricePerGiBMinute, KryderPlusRateMultiplier, Denomination, Height,
			RedenominationHeight, Timestamp, FloatingWallets, BlockAnchors, RecentTXMap,
			Mempool, VerifySignature} = Args,

	case ar_tx:verify(TX, {Rate, PricePerGiBMinute, KryderPlusRateMultiplier, Denomination,
			RedenominationHeight, Height, FloatingWallets, Timestamp}, VerifySignature) of
		true ->
			verify_anchor(TX, Height, FloatingWallets, BlockAnchors, RecentTXMap, Mempool);
		false ->
			{invalid, tx_verification_failed}
	end.

verify_anchor(TX, Height, FloatingWallets, BlockAnchors, RecentTXMap, Mempool) when
		is_record(TX, tx),
		is_map(FloatingWallets),
		is_list(BlockAnchors),
		is_map(RecentTXMap),
		is_map(Mempool) ->
	ShouldContinue = case ar_fork:height_1_8() of
		H when Height >= H ->
			%% Only verify after fork 1.8 otherwise it causes a soft fork
			%% since current nodes can accept blocks with a chain of last_tx
			%% references. The check would still fail on edge pre 1.8 since
			%% TX is validated against a previous blocks' wallet list then.
			case maps:is_key(TX#tx.anchor, Mempool) of
				true ->
					{invalid, last_tx_in_mempool};
				false ->
					continue
			end;
		_ ->
			continue
	end,
	case ShouldContinue of
		continue ->
			verify_last_tx(TX, FloatingWallets, BlockAnchors, RecentTXMap, Mempool);
		{invalid, Reason} ->
			{invalid, Reason}
	end.

verify_last_tx(TX, FloatingWallets, BlockAnchors, RecentTXMap, Mempool) ->
	case ar_tx:check_last_tx(FloatingWallets, TX) of
		true ->
			valid;
		false ->
			verify_block_anchor(TX, BlockAnchors, RecentTXMap, Mempool)
	end.

verify_block_anchor(TX, BlockAnchors, RecentTXMap, Mempool) ->
	case lists:member(TX#tx.anchor, BlockAnchors) of
		false ->
			{invalid, tx_bad_anchor};
		true ->
			verify_tx_in_weave(TX, RecentTXMap, Mempool)
	end.

verify_tx_in_weave(TX, RecentTXMap, Mempool) ->
	case maps:is_key(TX#tx.id, RecentTXMap) of
		true ->
			{invalid, tx_already_in_weave};
		false ->
			verify_tx_in_mempool(TX, Mempool)
	end.

verify_tx_in_mempool(TX, Mempool) ->
	case maps:is_key(TX#tx.id, Mempool) of
		true ->
			{invalid, tx_already_in_mempool};
		false ->
			valid
	end.

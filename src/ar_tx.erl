%%% @doc The module with utilities for transaction creation, signing, and verification.
-module(ar_tx).

-export([new/4, new/5, sign/2, verify/1, verify_tx_id/2]).
-export([json_struct_to_tx/1, tx_to_json_struct/1]).

-include("include/ar.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================


%% @doc Create a new transaction.
new(Dest, Reward, Qty, Last) ->
    #tx{
      id = crypto:strong_rand_bytes(32),
      last_tx = Last,
      quantity = Qty,
      target = Dest,
      data = <<>>,
      data_size = 0,
      reward = Reward
     }.


new(Dest, Reward, Qty, Last, SigType) ->
    #tx{
      id = crypto:strong_rand_bytes(32),
      last_tx = Last,
      quantity = Qty,
      target = Dest,
      data = <<>>,
      data_size = 0,
      reward = Reward,
      signature_type = SigType
     }.


%% @doc Cryptographically sign (claim ownership of) a transaction.
sign(TX, {PrivKey, {KeyType, Owner}}) ->
    NewTX = TX#tx{owner = Owner, signature_type = KeyType},
    Sig = ar_wallet:sign(PrivKey, signature_data_segment(NewTX)),
    ID = crypto:hash(sha256, <<Sig/binary>>),
    NewTX#tx{id = ID, signature = Sig}.


%% @doc Verify whether a transaction is valid.
verify(TX) ->
    do_verify(TX, verify_signature).


%% @doc Verify the given transaction actually has the given identifier.
verify_tx_id(ExpectedID, #tx{id = ID} = TX) ->
    ExpectedID == ID andalso verify_signature(TX, verify_signature) andalso verify_hash(TX).


%%%===================================================================
%%% Private functions.
%%%===================================================================


%% @doc Generate the data segment to be signed for a given TX.
signature_data_segment(TX) ->
    List = [<<(integer_to_binary(TX#tx.format))/binary>>,
            <<(TX#tx.owner)/binary>>,
            <<(TX#tx.target)/binary>>,
            <<(list_to_binary(integer_to_list(TX#tx.quantity)))/binary>>,
            <<(list_to_binary(integer_to_list(TX#tx.reward)))/binary>>,
            <<(TX#tx.last_tx)/binary>>,
            <<(integer_to_binary(TX#tx.data_size))/binary>>,
            <<(TX#tx.data_root)/binary>>],
    ar_deep_hash:hash(List).


%% @doc Verify the transaction's signature.
verify_signature(TX = #tx{signature_type = SigType}, verify_signature) ->
    SignatureDataSegment = signature_data_segment(TX),
    ar_wallet:verify({SigType, TX#tx.owner}, SignatureDataSegment, TX#tx.signature).


%% @doc Verify that the transaction's ID is a hash of its signature.
verify_hash(#tx{signature = Sig, id = ID}) ->
    ID == crypto:hash(sha256, <<Sig/binary>>).


%% @doc Verify transaction.
do_verify(TX, VerifySignature) ->
    From = ar_wallet:to_address(TX#tx.owner, TX#tx.signature_type),
    Checks = [{"quantity_negative", TX#tx.quantity >= 0},
              {"same_owner_as_target", (From =/= TX#tx.target)},
              {"tx_id_not_valid", verify_hash(TX)},
              {"tx_signature_not_valid", verify_signature(TX, VerifySignature)},
              {"tx_data_size_negative", TX#tx.data_size >= 0},
              {"tx_data_size_data_root_mismatch", (TX#tx.data_size == 0) == (TX#tx.data_root == <<>>)}],
    collect_validation_results(TX#tx.id, Checks).


collect_validation_results(_TXID, Checks) ->
    KeepFailed = fun({_, true}) -> false;
                    ({ErrorCode, false}) -> {true, ErrorCode}
                 end,
    case lists:filtermap(KeepFailed, Checks) of
        [] -> true;
        _ -> false
    end.


json_struct_to_tx(TXStruct) ->
    Tags =
        case hb_util:find_value(<<"tags">>, TXStruct) of
            undefined ->
                [];
            Xs ->
                Xs
        end,
    Data = hb_util:decode(hb_util:find_value(<<"data">>, TXStruct)),
    Format =
        case hb_util:find_value(<<"format">>, TXStruct) of
            undefined ->
                1;
            N when is_integer(N) ->
                N;
            N when is_binary(N) ->
                binary_to_integer(N)
        end,
    Denomination =
        case hb_util:find_value(<<"denomination">>, TXStruct) of
            undefined ->
                0;
            EncodedDenomination ->
                MaybeDenomination = binary_to_integer(EncodedDenomination),
                true = MaybeDenomination > 0,
                MaybeDenomination
        end,
    TXID = hb_util:decode(hb_util:find_value(<<"id">>, TXStruct)),
    32 = byte_size(TXID),
    #tx{
      format = Format,
      id = TXID,
      last_tx = hb_util:decode(hb_util:find_value(<<"last_tx">>, TXStruct)),
      owner = hb_util:decode(hb_util:find_value(<<"owner">>, TXStruct)),
      tags = [ {hb_util:decode(Name), hb_util:decode(Value)}
               %% Only the elements matching this pattern are included in the list.
               || {[{<<"name">>, Name}, {<<"value">>, Value}]} <- Tags ],
      target = hb_util:find_value(<<"target">>, TXStruct),
      quantity = binary_to_integer(hb_util:find_value(<<"quantity">>, TXStruct)),
      data = Data,
      reward = binary_to_integer(hb_util:find_value(<<"reward">>, TXStruct)),
      signature = hb_util:decode(hb_util:find_value(<<"signature">>, TXStruct)),
      data_size = binary_to_integer(hb_util:find_value(<<"data_size">>, TXStruct)),
      data_root =
          case hb_util:find_value(<<"data_root">>, TXStruct) of
              undefined -> <<>>;
              DR -> hb_util:decode(DR)
          end,
      denomination = Denomination
     }.


tx_to_json_struct(#tx{
                    id = ID,
                    format = Format,
                    last_tx = Last,
                    owner = Owner,
                    tags = Tags,
                    target = Target,
                    quantity = Quantity,
                    data = Data,
                    reward = Reward,
                    signature = Sig,
                    data_size = DataSize,
                    data_root = DataRoot,
                    denomination = Denomination
                   }) ->
    Fields = [{format,
               case Format of
                   undefined ->
                       1;
                   _ ->
                       Format
               end},
              {id, hb_util:encode(ID)},
              {last_tx, hb_util:encode(Last)},
              {owner, hb_util:encode(Owner)},
              {tags,
               lists:map(
                 fun({Name, Value}) ->
                         {[{name, hb_util:encode(Name)},
                           {value, hb_util:encode(Value)}]}
                 end,
                 Tags)},
              {target, hb_util:encode(Target)},
              {quantity, integer_to_binary(Quantity)},
              {data, hb_util:encode(Data)},
              {data_size, integer_to_binary(DataSize)},
              {data_tree, []},
              {data_root, hb_util:encode(DataRoot)},
              {reward, integer_to_binary(Reward)},
              {signature, hb_util:encode(Sig)}],
    Fields2 =
        case Denomination > 0 of
            true ->
                Fields ++ [{denomination, integer_to_binary(Denomination)}];
            false ->
                Fields
        end,
    hb_maps:from_list(Fields2).

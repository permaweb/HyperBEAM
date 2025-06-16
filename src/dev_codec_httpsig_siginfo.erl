%% @doc A module for converting between commitments and their encoded `signature'
%% and `signature-input' keys.
-module(dev_codec_httpsig_siginfo).
-export([commitments_to_siginfo/3, siginfo_to_commitments/3]).
-export([committed_keys_to_siginfo/1, to_siginfo_keys/3, from_siginfo_keys/3]).
-export([add_derived_specifiers/1, remove_derived_specifiers/1]).
-export([commitment_to_sig_name/1]).
-include("include/hb.hrl").

%%% A list of components that are `derived' in the context of RFC-9421 from the
%%% request message.
-define(DERIVED_COMPONENTS, [<<"method">>,
                             <<"target-uri">>,
                             <<"authority">>,
                             <<"scheme">>,
                             <<"request-target">>,
                             <<"path">>,
                             <<"query">>,
                             <<"query-param">>
         % <<"status">> % Some libraries does not support it
        ]).


%% @doc Generate a `signature' and `signature-input' key pair from a commitment
%% map.
commitments_to_siginfo(_Msg, Comms, _Opts) when ?IS_EMPTY_MESSAGE(Comms) ->
    #{};
commitments_to_siginfo(Msg, Comms, Opts) ->
    % Generate a SF item for each commitment's signature and signature-input.
    {Sigs, SigInputs} =
        maps:fold(
          fun(_CommID, Commitment, {Sigs, SigInputs}) ->
                  {ok, SigNameRaw, SFSig, SFSigInput} =
                      commitment_to_sf_siginfo(Msg, Commitment, Opts),
                  SigName = <<"sig-", SigNameRaw/binary>>,
                  {Sigs#{SigName => SFSig},
                   SigInputs#{SigName => SFSigInput}}
          end,
          {#{}, #{}},
          Comms),
    #{
      <<"signature">> =>
          hb_util:bin(hb_structured_fields:dictionary(Sigs)),
      <<"signature-input">> =>
          hb_util:bin(hb_structured_fields:dictionary(SigInputs))
     }.


%% @doc Generate a `signature' and `signature-input' key pair from a given
%% commitment.
commitment_to_sf_siginfo(Msg, Commitment, Opts) ->
    % Generate the `alg' key from the commitment.
    Alg = commitment_to_alg(Commitment, Opts),
    % Find the public key from the commitment, which we will use as the
    % `keyid' in the `signature-input' keys.
    KeyID = maps:get(<<"keyid">>, Commitment, <<>>),
    % Extract the signature from the commitment.
    Signature = hb_util:decode(maps:get(<<"signature">>, Commitment)),
    % Extract the keys present in the commitment.
    CommittedKeys = to_siginfo_keys(Msg, Commitment, Opts),
    ?event({normalized_for_enc, CommittedKeys, {commitment, Commitment}}),
    % Extract the hashpath, used as a tag, from the commitment.
    Tag = maps:get(<<"tag">>, Commitment, undefined),
    % Extract other permissible values, if present.
    Nonce = maps:get(<<"nonce">>, Commitment, undefined),
    Created = maps:get(<<"created">>, Commitment, undefined),
    Expires = maps:get(<<"expires">>, Commitment, undefined),
    % Generate the name of the signature.
    SigName = hb_util:to_lower(hb_util:human_id(crypto:hash(sha256, Signature))),
    % Generate the signature input and signature structured-fields. These can
    % then be placed into a dictionary with other commitments and transformed
    % into their binary representations.
    SFSig = {item, {binary, Signature}, []},
    AdditionalParams = get_additional_params(Commitment),
    Params =
        lists:filter(
          fun({_Key, undefined}) ->
                  false;
             ({_Key, {_, Val}}) ->
                  Val =/= undefined
          end,
          [{<<"alg">>, {string, Alg}},
           {<<"keyid">>, {string, KeyID}},
           {<<"tag">>, {string, Tag}},
           {<<"created">>, Created},
           {<<"expires">>, Expires},
           {<<"nonce">>, {string, Nonce}}] ++ AdditionalParams),
    SFSigInput =
        {list,
         [ {item, {string, Key}, []}
           || Key <- CommittedKeys ],
         Params},
    ?event(
      {sig_input,
       {string,
        hb_util:bin(
          hb_structured_fields:dictionary(
            #{<<"sig">> => SFSigInput}))}}),
    {ok, SigName, SFSig, SFSigInput}.


get_additional_params(Commitment) ->
    AdditionalParams =
        sets:to_list(
          sets:subtract(
            sets:from_list(maps:keys(Commitment)),
            sets:from_list(
              [<<"alg">>,
               <<"keyid">>,
               <<"tag">>,
               <<"created">>,
               <<"expires">>,
               <<"nonce">>,
               <<"committed">>,
               <<"signature">>,
               <<"type">>,
               <<"commitment-device">>,
               <<"committer">>]))),
    lists:map(fun(Param) ->
                      ParamValue = maps:get(Param, Commitment),
                      case ParamValue of
                          Val when is_binary(Val) ->
                              {Param, {string, Val}};
                          Val when is_list(Val) ->
                              {Param, {string, list_to_binary(lists:join(<<", ">>, Val))}};
                          Val when is_map(Val) ->
                              Map = nested_map_to_string(Val),
                              {Param, {string, list_to_binary(lists:join(<<", ">>, Map))}}
                      end
              end,
              AdditionalParams).


nested_map_to_string(Map) ->
    lists:map(fun(I) ->
                      case maps:get(I, Map) of
                          Val when is_map(Val) ->
                              Name = maps:get(<<"name">>, Val),
                              Value = maps:get(<<"value">>, Val),
                              <<I/binary, ":", Name/binary, ":", Value/binary>>;
                          Val ->
                              Val
                      end
              end,
              maps:keys(Map)).


%% @doc Take a message with a `signature' and `signature-input' key pair and
%% return a map of commitments.
siginfo_to_commitments(Msg = #{<<"signature">> := <<"sig-", SFSigBin/binary>>, <<"signature-input">> := <<"sig-", SFSigInputBin/binary>>},
                       BodyKeys,
                       Opts) ->
    % Parse the signature and signature-input structured-fields.
    SFSigs = hb_structured_fields:parse_dictionary(SFSigBin),
    SFSigsInputs = hb_structured_fields:parse_dictionary(SFSigInputBin),
    % Group parsed signature inputs and signatures into tuple pairs by their
    % name.
    CommitmentSFs =
        [ {SFSig, element(2, lists:keyfind(SFSigName, 1, SFSigsInputs))}
          || {SFSigName, SFSig} <- SFSigs ],
    % Convert each tuple into a commitment and its ID.
    CommitmentMessages =
        lists:map(
          fun({SFSig, SFSigInput}) ->
                  {ok, ID, Commitment} =
                      sf_siginfo_to_commitment(
                        Msg,
                        BodyKeys,
                        SFSig,
                        SFSigInput,
                        Opts),
                  {ID, Commitment}
          end,
          CommitmentSFs),
    % Convert the list of commitments into a map.
    maps:from_list(CommitmentMessages);
siginfo_to_commitments(_Msg, _BodyKeys, _Opts) ->
    % If the message does not contain a `signature' or `signature-input' key,
    % we return an empty map.
    #{}.


%% @doc Take a signature and signature-input as parsed structured-fields and
%% return a commitment.
sf_siginfo_to_commitment(Msg, BodyKeys, SFSig, SFSigInput, Opts) ->
    % Extract the signature and signature-input from the structured-fields.
    {item, {binary, Sig}, []} = SFSig,
    {list, SigInput, ParamsKV} = SFSigInput,
    % Generate a commitment message from the signature-input parameters.
    Commitment1 =
        maps:from_list(
          lists:map(
            fun({Key, {binary, Bin}}) -> {Key, hb_util:encode(Bin)};
               ({Key, BareItem}) ->
                    Item =
                        case hb_structured_fields:from_bare_item(BareItem) of
                            Res when is_binary(Res) ->
                                decoding_nested_map_binary(Res);
                            Res ->
                                Res
                        end,
                    {Key, Item}
            end,
            ParamsKV)),
    % Generate the `commitment-device' key and optionally, its `type' key from
    % the `alg' key.
    CommitmentDeviceKeys = commitment_to_device_specifiers(Commitment1, Opts),
    % Merge the commitment parameters with the commitment device, removing the
    % `alg' key.
    Commitment2 =
        maps:merge(
          CommitmentDeviceKeys,
          maps:remove(<<"alg">>, Commitment1)),
    % Generate the committed keys by parsing the signature-input list.
    RawCommittedKeys =
        [ Key
          || {item, {string, Key}, []} <- SigInput ],
    CommittedKeys = from_siginfo_keys(Msg, BodyKeys, RawCommittedKeys),
    % Merge and cleanup the output.
    % 1. Decode the `keyid` (typically a public key) to its raw byte form.
    % 2. Decode the `signature` to its raw byte form.
    % 3. Filter undefined keys.
    % 4. Generate the ID for the commitment from the signature. We use a SHA2-256
    %    hash of the signature, unless the signature is 32 bytes, in which case we
    %    use the signature directly as the ID.
    % 5. If the `keyid' is a public key (determined by length >= 32 bytes), set
    %    the `committer' to its hash.
    Commitment3 =
        Commitment2#{
          <<"signature">> => hb_util:encode(Sig),
          <<"committed">> => CommittedKeys
         },
    Commitment5 =
        case maps:get(<<"keyid">>, Commitment3) of
            DecKeyID when byte_size(DecKeyID) =< 43 ->
                Commitment3;
            DecPubKey ->
                Commitment3#{
                  <<"committer">> =>
                      hb_util:human_id(
                        crypto:hash(sha256, hb_util:decode(DecPubKey)))
                 }
        end,
    ID =
        if
            byte_size(Sig) == 32 -> hb_util:human_id(Sig);
            true -> hb_util:human_id(crypto:hash(sha256, Sig))
        end,
    % Return the commitment and calculated ID.
    {ok, ID, Commitment5}.


decoding_nested_map_binary(Bin) ->
    MapBinary =
        lists:foldl(
          fun(X, Acc) ->
                  case binary:split(X, <<":">>, [global]) of
                      [ID, Key, Value] ->
                          Acc#{
                            ID => #{<<"name">> => Key, <<"value">> => Value}
                           };
                      _ ->
                          X
                  end
          end,
          #{},
          binary:split(Bin, <<", ">>, [global])),
    case MapBinary of
        Res when is_map(Res) ->
            Res;
        Res ->
            Res
    end.


%% @doc Normalize a list of AO-Core keys to their equivalents in `httpsig@1.0'
%% format. This involves:
%% - If the HTTPSig message given has an `ao-body-key' key and the committed keys
%%   list contains it, we replace it in the list with the `body' key and add the
%%   `ao-body-key' key.
%% - If the list contains a `body' key, we replace it with the `content-digest'
%%   key.
%% - Otherwise, we return the list unchanged.
to_siginfo_keys(Msg, Commitment, Opts) ->
    {ok, _EncMsg, EncComm, _} =
        dev_codec_httpsig:normalize_for_encoding(Msg, Commitment, Opts),
    maps:get(<<"committed">>, EncComm).


%% @doc Normalize a list of `httpsig@1.0' keys to their equivalents in AO-Core
%% format. There are three stages:
%% 1. Remove the @ prefix from the component identifiers, if present.
%% 2. Replace `content-digest' with the body keys, if present.
%% 3. Replace the `body' key again with the value of the `ao-body-key' key, if
%%    present. This is possible because the keys derived from the body often
%%    contain the `body' key itself.
%% 4. If the `content-type' starts with `multipart/', we remove it.
from_siginfo_keys(HTTPEncMsg, BodyKeys, SigInfoCommitted) ->
    % 1. Remove specifiers from the list.
    BaseCommitted = remove_derived_specifiers(SigInfoCommitted),
    % 2. Replace the `content-digest' key with the `body' key, if present.
    WithBody =
        hb_util:list_replace(BaseCommitted, <<"content-digest">>, BodyKeys),
    % 3. Replace the `body' key again with the value of the `ao-body-key' key,
    %    if present.
    ?event(
      {from_siginfo_keys,
       {body_keys, BodyKeys},
       {raw_committed, SigInfoCommitted},
       {with_body, {explicit, WithBody}}}),
    ListWithoutBodyKey =
        case lists:member(<<"ao-body-key">>, WithBody) of
            true ->
                WithOrigBodyKey =
                    hb_util:list_replace(
                      WithBody,
                      <<"body">>,
                      maps:get(<<"ao-body-key">>, HTTPEncMsg)),
                ?event(
                  {with_orig_body_key, WithOrigBodyKey}),
                WithOrigBodyKey -- [<<"ao-body-key">>];
            false ->
                WithBody
        end,
    % 4. If the `content-type' starts with `multipart/', we remove it.
    ListWithoutContentType =
        case maps:get(<<"content-type">>, HTTPEncMsg, undefined) of
            <<"multipart/", _/binary>> ->
                hb_util:list_replace(ListWithoutBodyKey, <<"content-type">>, []);
            _ ->
                ListWithoutBodyKey
        end,
    Final =
        hb_ao:normalize_keys(
          lists:map(
            fun hb_link:remove_link_specifier/1,
            ListWithoutContentType)),
    ?event({from_siginfo_keys, {list, Final}}),
    Final.


%% @doc Convert committed keys to their siginfo format. This involves removing
%% the `body' key from the committed keys, if present, and replacing it with
%% the `content-digest' key.
committed_keys_to_siginfo(Msg) when is_map(Msg) ->
    committed_keys_to_siginfo(hb_util:message_to_ordered_list(Msg));
committed_keys_to_siginfo([]) -> [];
committed_keys_to_siginfo([<<"body">> | Rest]) ->
    [<<"content-digest">> | Rest];
committed_keys_to_siginfo([Key | Rest]) ->
    [Key | committed_keys_to_siginfo(Rest)].


%% @doc Convert an `alg` to a commitment device. If the `alg' has the form of
%% a device specifier (`x@y.z...[/type]'), return the device. Otherwise, we
%% assume that the `alg' is a `type' of the `httpsig@1.0' algorithm.
%% `type' is an optional key that allows for subtyping of the algorithm. When
%% provided, in the `alg' it is parsed and returned as the `type' key in the
%% commitment message.
commitment_to_device_specifiers(Commitment, Opts) when is_map(Commitment) ->
    commitment_to_device_specifiers(maps:get(<<"alg">>, Commitment), Opts);
commitment_to_device_specifiers(Alg, _Opts) ->
    case binary:split(Alg, <<"@">>) of
        [Type] ->
            % The `alg' is not a device specifier, so we assume that it is a
            % type of the `httpsig@1.0' algorithm.
            #{
              <<"commitment-device">> => <<"httpsig@1.0">>,
              <<"type">> => Type
             };
        [DevName, Specifiers] ->
            % The `alg' is a device specifier. We determine if a type is present
            % by splitting on the `/` character.
            case binary:split(Specifiers, <<"/">>) of
                [_Version] ->
                    % The `alg' is a device specifier without a type.
                    #{
                      <<"commitment-device">> => Alg
                     };
                [Version, Type] ->
                    % The `alg' is a device specifier with a type.
                    #{
                      <<"commitment-device">> =>
                          <<DevName/binary, "@", Version/binary>>,
                      <<"type">> => Type
                     }
            end
    end.


%% @doc Calculate an `alg' string from a commitment message, using its
%% `commitment-device' and optionally, its `type' key.
commitment_to_alg(#{<<"commitment-device">> := <<"httpsig@1.0">>, <<"type">> := Type}, _Opts) ->
    Type;
commitment_to_alg(Commitment, _Opts) ->
    Type =
        case maps:get(<<"type">>, Commitment, undefined) of
            undefined -> <<>>;
            TypeSpecifier -> <<"/", TypeSpecifier/binary>>
        end,
    CommitmentDevice = maps:get(<<"commitment-device">>, Commitment),
    <<CommitmentDevice/binary, Type/binary>>.


%% @doc Generate a signature name from a commitment. The commitment message is
%% not expected to be complete: Only the `commitment-device`, and the
%% `committer' or `keyid' keys are required.
commitment_to_sig_name(Commitment) ->
    BaseStr =
        case maps:get(<<"committer">>, Commitment, undefined) of
            undefined -> maps:get(<<"keyid">>, Commitment);
            Committer ->
                <<(hb_util:to_hex(binary:part(hb_util:native_id(Committer), 1, 8)))/binary>>
        end,
    DeviceStr =
        binary:replace(
          maps:get(
            <<"commitment-device">>,
            Commitment),
          <<"@">>,
          <<"-">>),
    <<DeviceStr/binary, ".", BaseStr/binary>>.


%% @doc Normalize key parameters to ensure their names are correct for inclusion
%% in the `signature-input' and associated keys.
add_derived_specifiers(ComponentIdentifiers) ->
    % Remove the @ prefix from the component identifiers, if present.
    Stripped =
        lists:map(
          fun(<<"@", Key/binary>>) -> Key; (Key) -> Key end,
          ComponentIdentifiers),
    % Add the @ prefix to the component identifiers, if they are derived.
    lists:flatten(
      lists:map(
        fun(Key) ->
                case lists:member(Key, ?DERIVED_COMPONENTS) of
                    true -> <<"@", Key/binary>>;
                    false -> Key
                end
        end,
        Stripped)).


%% @doc Remove derived specifiers from a list of component identifiers.
remove_derived_specifiers(ComponentIdentifiers) ->
    lists:map(
      fun(<<"@", Key/binary>>) ->
              Key;
         (Key) ->
              Key
      end,
      ComponentIdentifiers).

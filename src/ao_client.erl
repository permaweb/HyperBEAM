-module(ao_client).
%% Arweave node API
-export([arweave_timestamp/0]).
%% Arweave bundling and data access API
-export([upload/1, download/1]).
%% Scheduling Unit API
-export([get_assignments/1, get_assignments/2, get_assignments/3]).
-export([schedule/1, assign/1, register_su/1, register_su/2]).
-export([cron/1, cron/2, cron/3, cron_cursor/1]).
%% Compute Unit API
-export([compute/2]).
%% Messaging Unit API
-export([push/1, push/2]).

-include("include/ao.hrl").

%%% Arweave node API

arweave_timestamp() ->
	{ok, {{_, 200, _}, _, Body}} = httpc:request(ao:get(gateway) ++ "/block/current"),
	{Fields} = jiffy:decode(Body),
	{_, Timestamp} = lists:keyfind(<<"timestamp">>, 1, Fields),
	{_, Hash} = lists:keyfind(<<"indep_hash">>, 1, Fields),
	{_, Height} = lists:keyfind(<<"height">>, 1, Fields),
	{Timestamp, Height, Hash}.

%%% Bundling and data access API

download(ID) ->
	% TODO: Need to recreate full data items, not just data...
	case httpc:request(ao:get(gateway) ++ "/" ++ ID) of
		{ok, {{_, 200, _}, _, Body}} -> #tx{data = Body};
		_Rest -> throw({id_get_failed, ID})
	end.

upload(Item) ->
	case
		httpc:request(
			post,
			{ao:get(bundler) ++ "/tx", [], "application/octet-stream", ar_bundles:serialize(Item)},
			[],
			[]
		)
	of
		{ok, {{_, 200, _}, _, Body}} ->
			?c(upload_success),
			{ok, jiffy:decode(Body, [return_maps])};
		Response ->
			?c(upload_error),
			{error, bundler_http_error, Response}
	end.

%%% Scheduling Unit API
schedule(Msg) ->
	ao_http:post(
		maps:get(schedule, ao:get(nodes)),
		"/",
		Msg
	).

assign(_ID) ->
	?c({not_implemented, assignments}).

register_su(Location) ->
	register_su(Location, ao:get(key_location)).
register_su(Location, WalletLoc) when is_list(WalletLoc) ->
	register_su(Location, ar_wallet:load_keyfile(WalletLoc));
register_su(Location, Wallet) ->
	TX = #tx{
		tags = [
			{"Data-Protocol", "ao"},
			{"Variant", "ao.TN.1"},
			{"Type", "Scheduler-Location"},
			{"Url", Location},
			{"Time-To-Live", integer_to_list(ao:get(scheduler_location_ttl))}
		]
	},
	ao_client:upload(ar_bundles:sign_item(TX, Wallet)).

get_assignments(ProcID) ->
	get_assignments(ProcID, 0, undefined).
get_assignments(ProcID, From) ->
	get_assignments(ProcID, From, undefined).
get_assignments(ProcID, From, To) ->
	{ok, #tx{data = Data}} =
		ao_http:get(
			maps:get(schedule, ao:get(nodes)),
			"/?Action=Schedule&Process=" ++
				binary_to_list(ar_util:id(ProcID)) ++
				case From of
					undefined ->
						"";
					_ ->
						"&from=" ++
							case is_integer(From) of
								true -> integer_to_list(From);
								false -> binary_to_list(From)
							end
				end ++
				case To of
					undefined ->
						"";
					_ ->
						"&to=" ++
							case is_integer(To) of
								true -> integer_to_list(To);
								false -> binary_to_list(To)
							end
				end
		),
	extract_assignments(From, To, Data).

extract_assignments(_, _, Assignments) when map_size(Assignments) == 0 ->
	[];
extract_assignments(From, To, Assignments) ->
	?c({extracting_assignments, From, To}),
	KeyID = list_to_binary(integer_to_list(From)),
	case maps:is_key(KeyID, Assignments) of
		true ->
			[
				maps:get(KeyID, Assignments)
				| extract_assignments(From + 1, To, maps:remove(KeyID, Assignments))
			];
		false ->
			?c({no_assignment_for_key, KeyID}),
			[]
	end.

compute(ProcID, Slot) when is_binary(ProcID) ->
	compute(binary_to_list(ar_util:id(ProcID)), Slot);
compute(ProcID, Slot) when is_integer(Slot) ->
	compute(ProcID, integer_to_list(Slot));
compute(ProcID, Slot) when is_binary(Slot) ->
	compute(ProcID, binary_to_list(Slot));
compute(ProcID, Slot) when is_list(Slot) ->
	ao_http:get(
		maps:get(compute, ao:get(nodes)),
		"/?Process=" ++ ProcID ++ "&Slot=" ++ Slot
	);
compute(Assignment, Msg) when is_record(Assignment, tx) andalso is_record(Msg, tx) ->
	ao_http:post(
		maps:get(compute, ao:get(nodes)),
		"/",
		ar_bundles:normalize(#{
			<<"Message">> => Msg,
			<<"Assignment">> => Assignment
		})
	).

%%% MU API functions

push(Item) -> push(Item, none).
push(Item, TracingAtom) when is_atom(TracingAtom) ->
	push(Item, atom_to_list(TracingAtom));
push(Item, Tracing) ->
	?c({calling_remote_push, ar_util:id(Item#tx.id)}),
	ao_http:post(
		maps:get(message, ao:get(nodes)),
		"/?trace=" ++ Tracing,
		Item
	).

%%% CU API functions

cron(ProcID) ->
	cron(ProcID, cron_cursor(ProcID)).
cron(ProcID, Cursor) ->
	cron(ProcID, Cursor, ao:get(default_page_limit)).
cron(ProcID, Cursor, Limit) when is_binary(ProcID) ->
	cron(binary_to_list(ar_util:id(ProcID)), Cursor, Limit);
cron(ProcID, undefined, RawLimit) ->
	cron(ProcID, cron_cursor(ProcID), RawLimit);
cron(ProcID, Cursor, Limit) ->
	case
		httpc:request(
			maps:get(compute, ao:get(nodes)) ++
				"/cron/" ++
				ProcID ++
				"?cursor=" ++
				binary_to_list(Cursor) ++
				"&limit=" ++
				integer_to_list(Limit)
		)
	of
		{ok, {{_, 200, _}, _, Body}} ->
			try parse_result_set(Body) of
				{HasNextPage, Results} ->
					{ok, HasNextPage, Results, (lists:last(Results))#result.cursor}
			catch
				_:_ ->
					{error, cu_invalid_cron_response, Body}
			end;
		Response ->
			{error, cu_http_error, Response}
	end.

cron_cursor(ProcID) ->
	case httpc:request(maps:get(compute, ao:get(nodes)) ++ "/cron/" ++ ProcID ++ "?sort=DESC&limit=1") of
		{ok, {{_, 200, _}, _, Body}} ->
			{_, Res} = parse_result_set(Body),
			case Res of
				[] -> undefined;
				[Result] -> Result#result.cursor
			end;
		Response ->
			{error, cu_http_error, Response}
	end.

parse_result_set(Body) ->
	{JSONStruct} = jiffy:decode(Body),
	{_, {PageInfoStruct}} = lists:keyfind(<<"pageInfo">>, 1, JSONStruct),
	{_, HasNextPage} = lists:keyfind(<<"hasNextPage">>, 1, PageInfoStruct),
	{_, EdgesStruct} = lists:keyfind(<<"edges">>, 1, JSONStruct),
	{HasNextPage, lists:map(fun json_struct_to_result/1, EdgesStruct)}.

%% Parse a CU result into a #result record. If the result is in the form of a
%% stream, then the cursor is returned in the #result record as well.
json_struct_to_result(NodeStruct) ->
	json_struct_to_result(NodeStruct, #result{}).
json_struct_to_result({NodeStruct}, Res) ->
	json_struct_to_result(NodeStruct, Res);
json_struct_to_result(Struct, Res) ->
	case lists:keyfind(<<"node">>, 1, Struct) of
		false ->
			Res#result{
				messages = lists:map(
					fun ar_bundles:json_struct_to_item/1,
					ar_util:find_value(<<"Messages">>, Struct, [])
				),
				assignments = ar_util:find_value(<<"Assignments">>, Struct, []),
				spawns = lists:map(
					fun ar_bundles:json_struct_to_item/1,
					ar_util:find_value(<<"Spawns">>, Struct, [])
				),
				output = ar_util:find_value(<<"Output">>, Struct, [])
			};
		{_, {NodeStruct}} ->
			json_struct_to_result(
				NodeStruct,
				Res#result{
					cursor = ar_util:find_value(<<"cursor">>, Struct, undefined)
				}
			)
	end.

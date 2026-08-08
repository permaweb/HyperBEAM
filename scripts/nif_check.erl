-module(nif_check).
-export([main/1]).

-define(KEY, <<"default arweave 2.5 pack key">>).
-define(FAST, 0).
-define(LIGHT, 1).

-define(SUB_CHUNK_COUNT, 32).
-define(SUB_CHUNK_SIZE, 8192).
-define(LANE_COUNT, 4).
-define(DEPTH, 3).
-define(PROGRAM_COUNT, 6).
-define(SCRATCHPAD_SIZE, 2097152).

%% Upstream ar_mine_randomx_tests.erl lines 8-12.
-define(ENCODED_RX512_HASH, <<"NcXUtn7gA42QoM8MtaS-vgVy8gJ21EE2YxV18mHndmM">>).
-define(ENCODED_RX4096_HASH, <<"HqbpuoVNu8u4l4slkwnP3fvX9Q-mgjFH-3LgCyhMPPk">>).
-define(ENCODED_NONCE, <<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>).
-define(ENCODED_SEGMENT,
	<<"7XM3fgTCAY2GFpDjPZxlw4yw5cv8jNzZSZawywZGQ6_Ca-JDy2nX_MC2vjrIoDGp">>).

main([Phase]) ->
	run(list_to_atom(Phase)),
	init:stop().

%%%===================================================================
%%% Phases
%%%===================================================================

run(rx512) ->
	Input = hash_input(),
	Expected = decode(?ENCODED_RX512_HASH),
	rx_variant(rx512, fun ar_rx512_nif:rx512_init_nif/5,
		fun ar_rx512_nif:rx512_info_nif/1,
		fun ar_rx512_nif:rx512_hash_nif/5, Input, Expected);

run(rx4096) ->
	Input = hash_input(),
	Expected = decode(?ENCODED_RX4096_HASH),
	rx_variant(rx4096, fun ar_rx4096_nif:rx4096_init_nif/5,
		fun ar_rx4096_nif:rx4096_info_nif/1,
		fun ar_rx4096_nif:rx4096_hash_nif/5, Input, Expected);

run(fixtures512) ->
	%% Legacy spora_2_6 chunk packing/unpacking against upstream's fixtures
	%% (ar_mine_randomx_tests:test_regression/6, 8 rounds, JIT 0 and 1).
	{Key, Unpacked} = fixture_inputs(),
	Packed = fixture("packed.spora26.bin"),
	{ok, Light} = ar_rx512_nif:rx512_init_nif(?KEY, ?LIGHT, 0, 0, 0),
	{ok, Fast} = ar_rx512_nif:rx512_init_nif(?KEY, ?FAST, 0, 0,
		erlang:system_info(dirty_cpu_schedulers_online)),
	[begin
		{EncUs, {ok, P}} = timer:tc(fun() ->
			ar_rx512_nif:rx512_encrypt_chunk_nif(St, Key, Unpacked, 8, JIT, 0, 0) end),
		{DecUs, {ok, U}} = timer:tc(fun() ->
			ar_rx512_nif:rx512_decrypt_chunk_nif(St, Key, Packed, byte_size(Packed), 8,
				JIT, 0, 0) end),
		io:format("rx512 ~s jit=~B  encrypt MATCH: ~p (~.1f ms)  decrypt MATCH: ~p (~.1f ms)~n",
			[Mode, JIT, P =:= Packed, EncUs / 1000, U =:= Unpacked, DecUs / 1000])
	end || {Mode, St} <- [{"light", Light}, {"fast ", Fast}], JIT <- [0, 1]],
	ok;

run(fixtures4096) ->
	%% Composite (2.7) chunk packing/unpacking against upstream's fixtures.
	{Key, Unpacked} = fixture_inputs(),
	{ok, Light} = ar_rx4096_nif:rx4096_init_nif(?KEY, ?LIGHT, 0, 0, 0),
	{ok, Fast} = ar_rx4096_nif:rx4096_init_nif(?KEY, ?FAST, 0, 0,
		erlang:system_info(dirty_cpu_schedulers_online)),
	[begin
		Packed = fixture("packed.composite." ++ integer_to_list(Iters) ++ ".bin"),
		{ok, P} = ar_rx4096_nif:rx4096_encrypt_composite_chunk_nif(St, Key, Unpacked,
			JIT, 0, 0, 8, Iters, 32),
		{ok, U} = ar_rx4096_nif:rx4096_decrypt_composite_chunk_nif(St, Key, Packed,
			byte_size(Packed), JIT, 0, 0, 8, Iters, 32),
		io:format("rx4096 ~s jit=~B iters=~B  encrypt MATCH: ~p  decrypt MATCH: ~p~n",
			[Mode, JIT, Iters, P =:= Packed, U =:= Unpacked])
	end || {Mode, St} <- [{"light", Light}, {"fast ", Fast}], JIT <- [0, 1], Iters <- [1, 2]],
	ok;

run(rxsquared) ->
	Base = rss(),
	{FastInitUs, {ok, FastState}} =
		timer:tc(fun() ->
			ar_rxsquared_nif:rxsquared_init_nif(?KEY, ?FAST, 0, 0,
				erlang:system_info(dirty_cpu_schedulers_online))
		end),
	FastRss = rss(),
	io:format("rxsquared fast init: ~.3f s, rss delta ~.1f MiB~n",
		[FastInitUs / 1000000, (FastRss - Base) / 1024]),
	io:format("rxsquared_info_nif(fast) = ~p (expected {ok,{rxsquared,fast,34047604,2097152}})~n",
		[ar_rxsquared_nif:rxsquared_info_nif(FastState)]),
	entropy_vectors(FastState, "fast"),
	ok;

run(rxsquared_light) ->
	Base = rss(),
	{LightInitUs, {ok, LightState}} =
		timer:tc(fun() -> ar_rxsquared_nif:rxsquared_init_nif(?KEY, ?LIGHT, 0, 0, 0) end),
	io:format("rxsquared light init: ~.3f s, rss delta ~.1f MiB~n",
		[LightInitUs / 1000000, (rss() - Base) / 1024]),
	io:format("rxsquared_info_nif(light) = ~p (expected {ok,{rxsquared,light,0,2097152}})~n",
		[ar_rxsquared_nif:rxsquared_info_nif(LightState)]),
	entropy_vectors(LightState, "light"),
	ok;

run(vdf_selftest) ->
	%% Compute 2 checkpoints of 10 iterations then verify them.
	Salt = <<1:256>>,
	Seed = <<2:256>>,
	{ok, Out, Checkpoints} = ar_vdf_nif:vdf_sha2_nif(Salt, Seed, 1, 0, 10),
	io:format("vdf_sha2_nif out=~s checkpoints=~p bytes~n",
		[b64(Out), byte_size(Checkpoints)]),
	R = ar_vdf_nif:vdf_parallel_sha_verify_with_reset_nif(Salt, Seed, 1, 0, 10,
		Checkpoints, Out, <<0:256>>, <<0:256>>, 4),
	io:format("verify (correct) = ~p~n", [R]),
	Bad = ar_vdf_nif:vdf_parallel_sha_verify_with_reset_nif(Salt, Seed, 1, 0, 10,
		Checkpoints, <<0:256>>, <<0:256>>, <<0:256>>, 4),
	io:format("verify (tampered) = ~p~n", [Bad]),
	ok;

run(vdf_mainnet) ->
	File = os:getenv("VDF_BLOCK"),
	{ok, Bin} = file:read_file(File),
	#{<<"nonce_limiter_info">> := NL} = decode_json(Bin),
	Output = decode(maps:get(<<"output">>, NL)),
	StepNumber = int(maps:get(<<"global_step_number">>, NL)),
	Difficulty = int(maps:get(<<"vdf_difficulty">>, NL)),
	Seed = maps:get(<<"seed">>, NL),
	%% `checkpoints' holds one output per step in this block's range,
	%% newest-first; element 2 is therefore the output at StepNumber - 1,
	%% which is what ar_nonce_limiter passes as PrevOutput.
	[C0, C1 | _] = [decode(C) || C <- maps:get(<<"checkpoints">>, NL)],
	Output = C0,
	PrevOutput = C1,
	%% Entropy reset points are multiples of ?NONCE_LIMITER_RESET_FREQUENCY
	%% (1200). `maybe_add_entropy' is a no-op unless one falls in
	%% (StepNumber - 1, StepNumber], i.e. unless StepNumber is a multiple.
	false = (StepNumber rem 1200) =:= 0,
	%% On the wire the checkpoint list is newest-first; the NIF wants ascending.
	Ascending = lists:reverse([decode(C) || C <- maps:get(<<"last_step_checkpoints">>, NL)]),
	25 = length(Ascending),
	%% ar_vdf:verify2/8: StartSalt = step_number_to_salt_number(StepNumber - 1).
	StartSalt = (StepNumber - 1 - 1) * 25 + 1,
	%% ar_vdf:verify2/8 with ResetStepNumber = 0 -> ResetSalt = -49, which is
	%% out of range for the step being verified, so the reset path is dead.
	ResetSalt = (0 - 1 - 1) * 25 + 1,
	Buffer = iolist_to_binary(Ascending),
	<<RestSteps:(24 * 32)/binary, LastStep:32/binary>> = Buffer,
	LastStep = Output,
	io:format("step=~p vdf_difficulty=~p seed=~s~n", [StepNumber, Difficulty, Seed]),
	io:format("prev_output(step-1) = ~s~n", [b64(PrevOutput)]),
	io:format("output(step)        = ~s~n", [b64(Output)]),
	{Us, R} = timer:tc(fun() ->
		ar_vdf_nif:vdf_parallel_sha_verify_with_reset_nif(<<StartSalt:256>>, PrevOutput,
			24, 0, Difficulty, RestSteps, LastStep, <<ResetSalt:256>>,
			crypto:strong_rand_bytes(32),
			erlang:system_info(dirty_cpu_schedulers_online))
	end),
	io:format("verify(real block) = ~p in ~.3f s~n", [element(1, R), Us / 1000000]),
	%% Negative control: corrupt the 24th checkpoint.
	<<Head:(23 * 32)/binary, _:32/binary>> = RestSteps,
	BadR = ar_vdf_nif:vdf_parallel_sha_verify_with_reset_nif(<<StartSalt:256>>, PrevOutput,
		24, 0, Difficulty, <<Head/binary, 0:256>>, LastStep, <<ResetSalt:256>>,
		crypto:strong_rand_bytes(32), 4),
	io:format("negative control (corrupted checkpoint) = ~p~n", [element(1, BadR)]),
	%% Negative control: wrong prev output.
	BadR2 = ar_vdf_nif:vdf_parallel_sha_verify_with_reset_nif(<<StartSalt:256>>, <<0:256>>,
		24, 0, Difficulty, RestSteps, LastStep, <<ResetSalt:256>>,
		crypto:strong_rand_bytes(32), 4),
	io:format("negative control (wrong prev output)    = ~p~n", [element(1, BadR2)]),
	ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

rx_variant(Name, Init, Info, Hash, Input, Expected) ->
	Base = rss(),
	{LightUs, {ok, Light}} = timer:tc(fun() -> Init(?KEY, ?LIGHT, 0, 0, 0) end),
	LightRss = rss(),
	io:format("~p light init: ~.3f s, rss delta ~.1f MiB~n",
		[Name, LightUs / 1000000, (LightRss - Base) / 1024]),
	io:format("~p info(light) = ~p~n", [Name, Info(Light)]),
	LightHash = time_hash(Name, "light", Hash, Light, Input),
	{FastUs, {ok, Fast}} =
		timer:tc(fun() ->
			Init(?KEY, ?FAST, 0, 0, erlang:system_info(dirty_cpu_schedulers_online))
		end),
	FastRss = rss(),
	io:format("~p fast init: ~.3f s, rss delta ~.1f MiB~n",
		[Name, FastUs / 1000000, (FastRss - LightRss) / 1024]),
	io:format("~p info(fast) = ~p~n", [Name, Info(Fast)]),
	FastHash = time_hash(Name, "fast", Hash, Fast, Input),
	io:format("~p rss after hashing: total ~.1f MiB (was ~.1f MiB at start)~n",
		[Name, rss() / 1024, Base / 1024]),
	io:format("expected  = ~s~n", [b64(Expected)]),
	io:format("light     = ~s  MATCH: ~p~n", [b64(LightHash), LightHash =:= Expected]),
	io:format("fast      = ~s  MATCH: ~p~n", [b64(FastHash), FastHash =:= Expected]),
	ok.

time_hash(Name, Mode, Hash, State, Input) ->
	{ok, H} = Hash(State, Input, 0, 0, 0),
	lists:foreach(
		fun(JIT) ->
			{ok, H} = Hash(State, Input, JIT, 0, 0),
			N = 20,
			{Us, _} = timer:tc(fun() ->
				lists:foreach(fun(_) -> Hash(State, Input, JIT, 0, 0) end, lists:seq(1, N))
			end),
			io:format("~p ~s hash jit=~B: ~.3f ms/call (~B calls)~n",
				[Name, Mode, JIT, Us / 1000 / N, N])
		end,
		[0, 1]),
	H.

entropy_vectors(State, Mode) ->
	Expected1 = <<56,199,231,119,170,151,220,154,45,204,70,193,80,68,
		46,50,136,31,35,102,141,77,19,66,191,127,97,183,230,119,243,151>>,
	Expected2 = <<206,47,133,111,139,20,31,64,185,33,107,29,14,10,252,
		76,201,75,203,186,131,32,20,45,34,125,76,248,64,90,220,196>>,
	ExpectedPacked = <<15,46,184,11,124,31,150,77,199,107,221,0,136,154,61,
		146,193,198,126,52,19,7,211,28,121,108,176,15,124,33,48,99>>,
	{Us1, {ok, E1}} = timer:tc(fun() -> entropy(State, <<1>>, 0) end),
	io:format("rsp_fused_entropy_nif (~s) jit=0: ~.3f s/call, ~p bytes~n",
		[Mode, Us1 / 1000000, byte_size(E1)]),
	{Us2, {ok, E2}} = timer:tc(fun() -> entropy(State, <<2>>, 0) end),
	io:format("rsp_fused_entropy_nif (~s) jit=0, 2nd call: ~.3f s/call~n",
		[Mode, Us2 / 1000000]),
	{Us3, {ok, E1J}} = timer:tc(fun() -> entropy(State, <<1>>, 1) end),
	io:format("rsp_fused_entropy_nif (~s) jit=1: ~.3f s/call, jit==nojit: ~p~n",
		[Mode, Us3 / 1000000, E1J =:= E1]),
	{Us4, _} = timer:tc(fun() -> entropy(State, <<1>>, 1) end),
	io:format("rsp_fused_entropy_nif (~s) jit=1, 2nd call: ~.3f s/call~n",
		[Mode, Us4 / 1000000]),
	H1 = crypto:hash(sha256, E1),
	H2 = crypto:hash(sha256, E2),
	io:format("entropy(<<1>>) sha256 computed = ~w~n", [H1]),
	io:format("entropy(<<1>>) sha256 expected = ~w~n", [Expected1]),
	io:format("MATCH: ~p~n", [H1 =:= Expected1]),
	io:format("entropy(<<2>>) sha256 computed = ~w~n", [H2]),
	io:format("entropy(<<2>>) sha256 expected = ~w~n", [Expected2]),
	io:format("MATCH: ~p~n", [H2 =:= Expected2]),
	%% Packed sub-chunk vector: XOR sub-chunk 1 of entropy(<<1>>) with 0xFF*8192.
	SubChunk = <<255:(8 * ?SUB_CHUNK_SIZE)>>,
	Part = binary:part(E1, 1 * ?SUB_CHUNK_SIZE, ?SUB_CHUNK_SIZE),
	Packed = crypto:exor(SubChunk, Part),
	HP = crypto:hash(sha256, Packed),
	io:format("packed sub-chunk sha256 computed = ~w~n", [HP]),
	io:format("packed sub-chunk sha256 expected = ~w~n", [ExpectedPacked]),
	io:format("MATCH: ~p~n", [HP =:= ExpectedPacked]),
	ok.

entropy(State, Key, JIT) ->
	ar_rxsquared_nif:rsp_fused_entropy_nif(State, ?SUB_CHUNK_COUNT, ?SUB_CHUNK_SIZE,
		?LANE_COUNT, ?DEPTH, JIT, 0, 0, ?PROGRAM_COUNT, Key).

fixture_inputs() ->
	{fixture("key.bin"), fixture("unpacked.bin")}.

fixture(Name) ->
	{ok, Bin} = file:read_file(filename:join(os:getenv("AR_FIXTURES"), Name)),
	Bin.

hash_input() ->
	<<(decode(?ENCODED_NONCE))/binary, (decode(?ENCODED_SEGMENT))/binary>>.

decode(Bin) ->
	Padded =
		case byte_size(Bin) rem 4 of
			0 -> Bin;
			2 -> <<Bin/binary, "==">>;
			3 -> <<Bin/binary, "=">>
		end,
	base64:decode(binary:replace(binary:replace(Padded, <<"-">>, <<"+">>, [global]),
		<<"_">>, <<"/">>, [global])).

b64(Bin) ->
	binary:replace(binary:replace(
		binary:replace(base64:encode(Bin), <<"=">>, <<>>, [global]),
		<<"+">>, <<"-">>, [global]), <<"/">>, <<"_">>, [global]).

%% Resident set size of this OS process, in KiB.
rss() ->
	Out = os:cmd("ps -o rss= -p " ++ os:getpid()),
	list_to_integer(string:trim(Out)).

decode_json(Bin) ->
	json:decode(Bin).

int(N) when is_integer(N) -> N;
int(Bin) when is_binary(Bin) -> binary_to_integer(Bin).

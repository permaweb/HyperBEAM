%%% @doc A virtual filesystem device.
%%% Implements a file-system-as-map structure, which is traversible externally.
%%% Each file is a binary and each directory is an AO-Core message.
%%% Additionally, this module adds a series of WASI-preview-1 compatible
%%% functions for accessing the filesystem as imported functions by WASM
%%% modules.
-module(dev_wasi).
-export([init/3, compute/1, stdout/1]).
-export([path_open/3, fd_write/3, fd_read/3, clock_time_get/3]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(INIT_VFS,
        #{
          <<"dev">> => #{
                         <<"stdin">> => <<>>,
                         <<"stdout">> => <<>>,
                         <<"stderr">> => <<>>
                        }
         }).

-define(INIT_FDS,
        #{
          <<"0">> => #{
                       <<"filename">> => <<"/dev/stdin">>,
                       <<"offset">> => 0
                      },
          <<"1">> => #{
                       <<"filename">> => <<"/dev/stdout">>,
                       <<"offset">> => 0
                      },
          <<"2">> => #{
                       <<"filename">> => <<"/dev/stderr">>,
                       <<"offset">> => 0
                      }
         }).


%% @doc On-boot, initialize the virtual file system with:
%% - Empty stdio files
%% - WASI-preview-1 compatible functions for accessing the filesystem
%% - File descriptors for those files.
init(M1, _M2, Opts) ->
    ?event(running_init),
    MsgWithLib =
        hb_ao:set(
          M1,
          #{
            <<"wasm/stdlib/wasi_snapshot_preview1">> =>
                #{<<"device">> => <<"WASI@1.0">>}
           },
          Opts),
    MsgWithFDs =
        hb_ao:set(
          MsgWithLib,
          <<"file-descriptors">>,
          ?INIT_FDS,
          Opts),
    CompleteMsg =
        hb_ao:set(
          MsgWithFDs,
          <<"vfs">>,
          ?INIT_VFS,
          Opts),
    {ok, CompleteMsg}.


compute(Msg1) ->
    {ok, Msg1}.


%% @doc Return the stdout buffer from a state message.
stdout(M) ->
    hb_ao:get(<<"vfs/dev/stdout">>, M).


%% @doc Adds a file descriptor to the state message.
%path_open(M, Instance, [FDPtr, LookupFlag, PathPtr|_]) ->
path_open(Msg1, Msg2, Opts) ->
    FDs = hb_ao:get(<<"file-descriptors">>, Msg1, Opts),
    Instance = hb_private:get(<<"instance">>, Msg1, Opts),
    [FDPtr, LookupFlag, PathPtr | _] = hb_ao:get(<<"args">>, Msg2, Opts),
    ?event({path_open, FDPtr, LookupFlag, PathPtr}),
    Path = hb_beamr_io:read_string(Instance, PathPtr),
    ?event({path_open, Path}),
    FD = #{
           <<"index">> := Index
          } =
        case hb_ao:get(<<"vfs/", Path/binary>>, Msg1, Opts) of
            not_found ->
                #{
                  <<"index">> => length(hb_ao:keys(FDs)) + 1,
                  <<"filename">> => Path,
                  <<"offset">> => 0
                 };
            F -> F
        end,
    {ok,
     #{
       <<"state">> =>
           hb_ao:set(
             Msg1,
             <<"vfs/", Path/binary>>,
             FD),
       <<"results">> => [0, Index]
      }}.


%% @doc WASM stdlib implementation of `fd_write', using the WASI-p1 standard
%% interface.
fd_write(Msg1, Msg2, Opts) ->
    State = hb_ao:get(<<"state">>, Msg1, Opts),
    Instance = hb_private:get(<<"wasm/instance">>, State, Opts),
    [FD, Ptr, Vecs, RetPtr | _] = hb_ao:get(<<"args">>, Msg2, Opts),
    ?event({fd_write, {fd, FD}, {ptr, Ptr}, {vecs, Vecs}, {retptr, RetPtr}}),
    Signature = hb_ao:get(<<"func-sig">>, Msg2, Opts),
    ?event({signature, Signature}),
    fd_write(State, Instance, [FD, Ptr, Vecs, RetPtr], 0, Opts).


fd_write(S, Instance, [_, _Ptr, 0, RetPtr], BytesWritten, _Opts) ->
    hb_beamr_io:write(
      Instance,
      RetPtr,
      <<BytesWritten:64/little-unsigned-integer>>),
    {ok, #{<<"state">> => S, <<"results">> => [0]}};
fd_write(S, Instance, [FDnum, Ptr, Vecs, RetPtr], BytesWritten, Opts) ->
    FDNumStr = integer_to_binary(FDnum),
    FD = hb_ao:get(<<"file-descriptors/", FDNumStr/binary>>, S, Opts),
    Filename = hb_ao:get(<<"filename">>, FD, Opts),
    StartOffset = hb_ao:get(<<"offset">>, FD, Opts),
    {VecPtr, Len} = parse_iovec(Instance, Ptr),
    {ok, Data} = hb_beamr_io:read(Instance, VecPtr, Len),
    Before =
        binary:part(
          OrigData = hb_ao:get(<<"data">>, FD, Opts),
          0,
          StartOffset),
    After =
        binary:part(OrigData, StartOffset, byte_size(OrigData) - StartOffset),
    S1 =
        hb_ao:set(
          S,
          <<"file-descriptors/", FDNumStr/binary, "/offset">>,
          StartOffset + byte_size(Data),
          Opts),
    S2 =
        hb_ao:set(
          S1,
          <<"vfs/", Filename/binary>>,
          <<Before/binary, Data/binary, After/binary>>,
          Opts),
    fd_write(
      S2,
      Instance,
      [FD, Ptr + 16, Vecs - 1, RetPtr],
      BytesWritten + byte_size(Data),
      Opts).


%% @doc Read from a file using the WASI-p1 standard interface.
fd_read(Msg1, Msg2, Opts) ->
    State = hb_ao:get(<<"state">>, Msg1, Opts),
    Instance = hb_private:get(<<"wasm/instance">>, State, Opts),
    [FD, VecsPtr, NumVecs, RetPtr | _] = hb_ao:get(<<"args">>, Msg2, Opts),
    Signature = hb_ao:get(<<"func-sig">>, Msg2, Opts),
    ?event({signature, Signature}),
    fd_read(State, Instance, [FD, VecsPtr, NumVecs, RetPtr], 0, Opts).


fd_read(S, Instance, [FD, _VecsPtr, 0, RetPtr], BytesRead, _Opts) ->
    ?event({{completed_read, FD, BytesRead}}),
    hb_beamr_io:write(Instance,
                      RetPtr,
                      <<BytesRead:64/little-unsigned-integer>>),
    {ok, #{<<"state">> => S, <<"results">> => [0]}};
fd_read(S, Instance, [FDNum, VecsPtr, NumVecs, RetPtr], BytesRead, Opts) ->
    ?event({fd_read, FDNum, VecsPtr, NumVecs, RetPtr}),
    % Parse the request
    FDNumStr = integer_to_binary(FDNum),
    Filename =
        hb_ao:get(
          <<"file-descriptors/", FDNumStr/binary, "/filename">>, S, Opts),
    {VecPtr, Len} = parse_iovec(Instance, VecsPtr),
    % Read the bytes from the file
    Data = hb_ao:get(<<"vfs/", Filename/binary>>, S, Opts),
    Offset =
        hb_ao:get(
          <<"file-descriptors/", FDNumStr/binary, "/offset">>, S, Opts),
    ReadSize = min(Len, byte_size(Data) - Offset),
    Bin = binary:part(Data, Offset, ReadSize),
    % Write the bytes to the WASM Instance
    ok = hb_beamr_io:write(Instance, VecPtr, Bin),
    fd_read(
      hb_ao:set(
        S,
        <<"file-descriptors/", FDNumStr/binary, "/offset">>,
        Offset + ReadSize,
        Opts),
      Instance,
      [FDNum, VecsPtr + 16, NumVecs - 1, RetPtr],
      BytesRead + ReadSize,
      Opts).


%% @doc Parse an iovec in WASI-preview-1 format.
parse_iovec(Instance, Ptr) ->
    {ok, VecStruct} = hb_beamr_io:read(Instance, Ptr, 16),
    <<BinPtr:64/little-unsigned-integer,
      Len:64/little-unsigned-integer>> = VecStruct,
    {BinPtr, Len}.


%%% Misc WASI-preview-1 handlers.
clock_time_get(Msg1, _Msg2, Opts) ->
    ?event({clock_time_get, {returning, 1}}),
    State = hb_ao:get(<<"state">>, Msg1, Opts),
    {ok, #{<<"state">> => State, <<"results">> => [1]}}.


%%% Tests


init() ->
    application:ensure_all_started(hb).


generate_wasi_stack(File, Func, Params) ->
    init(),
    Msg0 = dev_wasm:cache_wasm_image(File),
    Msg1 = Msg0#{
             <<"device">> => <<"stack@1.0">>,
             <<"device-stack">> => [<<"WASI@1.0">>, <<"WASM-64@1.0">>],
             <<"output-prefixes">> => [<<"wasm">>, <<"wasm">>],
             <<"stack-keys">> => [<<"init">>, <<"compute">>],
             <<"function">> => Func,
             <<"params">> => Params
            },
    {ok, Msg2} = hb_ao:resolve(Msg1, <<"init">>, #{}),
    Msg2.


vfs_is_serializable_test() ->
    StackMsg = generate_wasi_stack("test/test-print.wasm", <<"hello">>, []),
    VFSMsg = hb_ao:get(<<"vfs">>, StackMsg),
    VFSMsg2 =
        hb_message:minimize(
          hb_message:convert(
            hb_message:convert(VFSMsg, <<"httpsig@1.0">>, #{}),
            <<"structured@1.0">>,
            <<"httpsig@1.0">>,
            #{})),
    ?assert(hb_message:match(VFSMsg, VFSMsg2)).


wasi_stack_is_serializable_test() ->
    Msg = generate_wasi_stack("test/test-print.wasm", <<"hello">>, []),
    HTTPSigMsg = hb_message:convert(Msg, <<"httpsig@1.0">>, #{}),
    Msg2 = hb_message:convert(HTTPSigMsg, <<"structured@1.0">>, <<"httpsig@1.0">>, #{}),
    ?assert(hb_message:match(Msg, Msg2)).


basic_aos_exec_test() ->
    Init = generate_wasi_stack("test/aos-2-pure-xs.wasm", <<"handle">>, []),
    Msg = gen_test_aos_msg("return 1 + 1"),
    Env = gen_test_env(),
    Instance = hb_private:get(<<"wasm/instance">>, Init, #{}),
    {ok, Ptr1} = hb_beamr_io:malloc(Instance, byte_size(Msg)),
    ?assertNotEqual(0, Ptr1),
    hb_beamr_io:write(Instance, Ptr1, Msg),
    {ok, Ptr2} = hb_beamr_io:malloc(Instance, byte_size(Env)),
    ?assertNotEqual(0, Ptr2),
    hb_beamr_io:write(Instance, Ptr2, Env),
    % Read the strings to validate they are correctly passed
    {ok, MsgBin} = hb_beamr_io:read(Instance, Ptr1, byte_size(Msg)),
    {ok, EnvBin} = hb_beamr_io:read(Instance, Ptr2, byte_size(Env)),
    ?assertEqual(Env, EnvBin),
    ?assertEqual(Msg, MsgBin),
    Ready = Init#{<<"parameters">> => [Ptr1, Ptr2]},
    {ok, StateRes} = hb_ao:resolve(Ready, <<"compute">>, #{}),
    [Ptr] = hb_ao:get(<<"results/wasm/output">>, StateRes),
    {ok, Output} = hb_beamr_io:read_string(Instance, Ptr),
    ?event({got_output, Output}),
    #{<<"response">> := #{<<"Output">> := #{<<"data">> := Data}}} =
        hb_json:decode(Output),
    ?assertEqual(<<"2">>, Data).


%%% Test Helpers
gen_test_env() ->
    <<"{\"Process\":{\"Id\":\"AOS\",\"Owner\":\"FOOBAR\",\"Tags\":[{\"name\":\"Name\",\"value\":\"Thomas\"}, {\"name\":\"Authority\",\"value\":\"FOOBAR\"}]}}\0">>.


gen_test_aos_msg(Command) ->
    <<"{\"From\":\"FOOBAR\",\"Block-Height\":\"1\",\"Target\":\"AOS\",\"Owner\":\"FOOBAR\",\"Id\":\"1\",\"Module\":\"W\",\"Tags\":[{\"name\":\"Action\",\"value\":\"Eval\"}],\"Data\":\"", (list_to_binary(Command))/binary, "\"}\0">>.

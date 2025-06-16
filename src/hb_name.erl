%%% @doc An abstraction for name registration/deregistration in HyperBEAM.
%%% Its motivation is to provide a way to register names that are not necessarily
%%% atoms, but can be any term (for example: hashpaths or `process@1.0' IDs).
%%% An important characteristic of these functions is that they are atomic:
%%% There can only ever be one registrant for a given name at a time.
-module(hb_name).
-export([start/0, register/1, register/2, unregister/1, lookup/1, all/0]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").
-define(NAME_TABLE, hb_name_registry).


%% Initialize ETS table for non-atom registrations
start() ->
    try ets:info(?NAME_TABLE) of
        undefined -> start_ets();
        _ -> ok
    catch
        error:badarg -> start_ets()
    end.


%% Start the ETS table.
start_ets() ->
    ets:new(?NAME_TABLE,
            [named_table,
             public,
             {keypos, 1},
             {write_concurrency, true},  % Safe as key-writes are atomic.
             {read_concurrency, true}]),
    ok.


%%% @doc Register a name. If the name is already registered, the registration
%%% will fail. The name can be any Erlang term.
register(Name) ->
    start(),
    ?MODULE:register(Name, self()).


register(Name, Pid) when is_atom(Name) ->
    try erlang:register(Name, Pid) of
        true -> ok
    catch
        error:badarg -> error  % Name already registered
    end;
register(Name, Pid) ->
    start(),
    case ets:insert_new(?NAME_TABLE, {Name, Pid}) of
        true -> ok;
        false -> error
    end.


%%% @doc Unregister a name.
unregister(Name) when is_atom(Name) ->
    catch erlang:unregister(Name),
    ets:delete(?NAME_TABLE, Name),  % Cleanup if atom was in ETS
    ok;
unregister(Name) ->
    start(),
    ets:delete(?NAME_TABLE, Name),
    ok.


%%% @doc Lookup a name -> PID.
lookup(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined ->
            % Check ETS for atom-based names
            start(),
            ets_lookup(Name);
        Pid -> Pid
    end;
lookup(Name) ->
    start(),
    ets_lookup(Name).


ets_lookup(Name) ->
    case ets:lookup(?NAME_TABLE, Name) of
        [{Name, Pid}] ->
            case is_process_alive(Pid) of
                true -> Pid;
                false ->
                    ets:delete(?NAME_TABLE, Name),
                    undefined
            end;
        [] -> undefined
    end.


%% @doc List the names in the registry.
all() ->
    Registered =
        ets:tab2list(?NAME_TABLE) ++
        lists:filtermap(
          fun(Name) ->
                  case whereis(Name) of
                      undefined -> false;
                      Pid -> {true, {Name, Pid}}
                  end
          end,
          erlang:registered()),
    lists:filter(
      fun({_, Pid}) -> is_process_alive(Pid) end,
      Registered).


%%% Tests

-define(CONCURRENT_REGISTRATIONS, 10).


basic_test(Term) ->
    ?assertEqual(ok, hb_name:register(Term)),
    ?assertEqual(self(), hb_name:lookup(Term)),
    ?assertEqual(error, hb_name:register(Term)),
    hb_name:unregister(Term),
    ?assertEqual(undefined, hb_name:lookup(Term)).


atom_test() ->
    basic_test(atom).


term_test() ->
    basic_test({term, os:timestamp()}).


concurrency_test() ->
    Name = {concurrent_test, os:timestamp()},
    SuccessCount = length([ R || R <- spawn_test_workers(Name), R =:= ok ]),
    ?assertEqual(1, SuccessCount),
    ?assert(is_pid(hb_name:lookup(Name))),
    hb_name:unregister(Name).


spawn_test_workers(Name) ->
    Self = self(),
    Names =
        [ case Name of
              random -> {random_name, rand:uniform(1000000)};
              _ -> Name
          end
          || _ <- lists:seq(1, ?CONCURRENT_REGISTRATIONS) ],
    Pids =
        [ spawn(
            fun() ->
                    Result = hb_name:register(ProcName),
                    Self ! {result, self(), Result},
                    % Stay alive to prevent cleanup for a period.
                    timer:sleep(500)
            end)
          || ProcName <- Names ],
    [ receive {result, Pid, Res} -> Res after 100 -> timeout end
      || Pid <- Pids ].


dead_process_test() ->
    Name = {dead_process_test, os:timestamp()},
    {Pid, Ref} = spawn_monitor(fun() -> hb_name:register(Name), ok end),
    receive {'DOWN', Ref, process, Pid, _} -> ok end,
    ?assertEqual(undefined, hb_name:lookup(Name)).


cleanup_test() ->
    {setup,
     fun() ->
             Name = {cleanup_test, os:timestamp()},
             {Pid, Ref} = spawn_monitor(fun() -> timer:sleep(1000) end),
             ?assertEqual(ok, hb_name:register(Name, Pid)),
             {Name, Pid, Ref}
     end,
     fun({Name, _, _}) ->
             hb_name:unregister(Name)
     end,
     fun({Name, Pid, Ref}) ->
             {"Auto-cleanup on process death",
              fun() ->
                      exit(Pid, kill),
                      receive {'DOWN', Ref, process, Pid, _} -> ok end,
                      ?assertEqual(undefined, wait_for_cleanup(Name, 10))
              end}
     end}.


wait_for_cleanup(Name, Retries) ->
    case Retries > 0 of
        true ->
            case hb_name:lookup(Name) of
                undefined -> undefined;
                _ ->
                    timer:sleep(100),
                    wait_for_cleanup(Name, Retries - 1)
            end;
        false -> undefined
    end.


all_test() ->
    hb_name:register(test_name, self()),
    ?assert(lists:member({test_name, self()}, hb_name:all())),
    BaseRegistered = length(hb_name:all()),
    spawn_test_workers(random),
    ?assertEqual(BaseRegistered + ?CONCURRENT_REGISTRATIONS, length(hb_name:all())),
    timer:sleep(1000),
    ?assertEqual(BaseRegistered, length(hb_name:all())).

-include("include/ar.hrl").

%%% Functional macros that pass the current module and line number to the
%%% underlying function.
-define(event(X), hb:event(X, ?MODULE, ?LINE)).
-define(debug_wait(T), hb:debug_wait(T, ?MODULE, ?LINE)).
-define(no_prod(X), hb:no_prod(X, ?MODULE, ?LINE)).

%%% Macro shortcuts for debugging.
%% @doc A macro for marking that you got 'here'.
-define(h(), hb:event("[Debug point reached.]", ?MODULE, ?LINE)).
%% @doc Quickly print a value in the logs. Currently uses the event
%% function, but should be moved to a debug-specific function once we
%% build out better logging infrastructure.
-define(p(X), hb:event(X, ?MODULE, ?LINE)).
% We don't include a macro for waiting, as it is destructive to UX.
% If you need to wait, it should at least be obvious to readers of
% the code what is happening.

-record(result, {
	messages = [],
	assignments = [],
	spawns = [],
	output = [],
	cursor = undefined
}).

-record(pstate, {
    id,         % The ID of the process which is represented.
    process,    % Process definition data item.
    wallet,     % The wallet we should use to sign responses.
    slot,       % The current slot we have process until.
    epoch,      % The present epoch of the process.
    vm_mod,     % VM Erlang module that should be used by this process.
                % This module must implement Mod:eval/3 and Mod:pstate/2.
    vm_opts,    % Options provided in calls to Mod:eval/3. This value may also
                % Employed to host VM extension data as necessary. At boot, it is
                % used to store the checkpoint (if set) that the process should be
                % initialized from.
    iface_mod,  % A module implementing the appropriate output strategy for the
                % process, taking data from the end of an execution run and
                % formatting it as bundles for signing and output.
    iface_opts, % Optional arguments to be passed to the output module on
                % calls to mod:format/3.
    transforms, % A list of transformations ({Mod, Opts}) to execute upon schedules
                % before execution.
    devices,    % A list of devices that may be called during execution by the
                % interface module at the request of the machine.
    queue       % A list of all of the known assignments that are left to
                % calculate for this process.
}).
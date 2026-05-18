%%% @doc Helpers for generated packaged-device module names.
-module(hb_device_name).
-export([generated/2, sanitize/1, is_generated/1, parts/1, root/1]).

-define(PREFIX, <<"_hb_device_">>).

%% @doc Build the generated module atom for a device and package hash.
generated(DeviceName, Hash) ->
    binary_to_atom(
        <<?PREFIX/binary, (sanitize(DeviceName))/binary, "_", Hash/binary>>,
        utf8
    ).

%% @doc Sanitize a device name so it can appear inside an Erlang atom.
sanitize(Name) when is_binary(Name) ->
    Lower = string:lowercase(Name),
    list_to_binary([sanitize_char(C) || <<C>> <= Lower]);
sanitize(Name) when is_list(Name) ->
    sanitize(hb_util:bin(Name));
sanitize(Name) when is_atom(Name) ->
    sanitize(atom_to_binary(Name, utf8)).

%% @doc Return a character safe for generated Erlang module atoms.
sanitize_char(C) when C >= $a, C =< $z -> C;
sanitize_char(C) when C >= $0, C =< $9 -> C;
sanitize_char(_) -> $_.

%% @doc Recognize a generated `_hb_device_*' module atom or binary.
is_generated(Atom) when is_atom(Atom) ->
    is_generated(atom_to_binary(Atom, utf8));
is_generated(<<"_hb_device_", _/binary>>) ->
    true;
is_generated(_) ->
    false.

%% @doc Decompose a generated module name into display components.
parts(Atom) when is_atom(Atom) ->
    parts(atom_to_binary(Atom, utf8));
parts(<<"_hb_device_", Rest/binary>>) ->
    [RootPart | HelperParts] = binary:split(Rest, <<"__">>, [global]),
    case binary:split(RootPart, <<"_">>, [global]) of
        Parts when length(Parts) >= 2 ->
            [Hash | RevName] = lists:reverse(Parts),
            Name =
                iolist_to_binary(
                    lists:join(<<"_">>, lists:reverse(RevName))
                ),
            case HelperParts of
                [] -> {Name, Hash};
                _ ->
                    Helper = iolist_to_binary(lists:join(<<"__">>, HelperParts)),
                    {Name, Hash, Helper}
            end;
        _ -> not_generated
    end;
parts(_) ->
    not_generated.

%% @doc Return the root generated module for a root or helper module.
root(Module) ->
    [Root | _] = binary:split(atom_to_binary(Module, utf8), <<"__">>),
    binary_to_atom(Root, utf8).

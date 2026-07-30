%%% @doc Marker types for AO-Core schema forms consumed by `hb_types'.
-module(hb_schema).

-export_type([bind/1, int/1, date/3]).

-type bind(_Name) :: term().
-type int(_Name) :: integer().
-type date(_Unit, _Bucket, _Format) :: binary().

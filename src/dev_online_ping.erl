%% Option A: flat group (most HB builds accept this)
hb_ao:resolve(
  #{<<"device">> => <<"online-ping@1.0">>, <<"group">> => <<"device:online-ping@1.0">>},
  <<"ping-once">>,
  #{<<"url">> => <<"https://httpbin.org/status/200">>}
).

%% Option B: nested persistence.group (some builds prefer this)
hb_ao:resolve(
  #{<<"device">> => <<"online-ping@1.0">>, <<"persistence">> => #{<<"group">> => <<"device:online-ping@1.0">>}},
  <<"ping-once">>,
  #{<<"url">> => <<"https://httpbin.org/status/200">>}
).

%%% @doc A device that renders a REPL-like interface for AO-Core via HTML.
-module(dev_hyperbuddy).
-export([info/1, describe/3, format/3, page/3, page_data/3, return_file/2, return_error/2]).
-export([metrics/3, events/3]).
-export([cookbook/0, throw/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-type ao_message() :: map().
-type key_path() :: binary().
-type hyperbuddy_request() ::
    #{
        target => binary(),
        format => binary() | [binary()],
        truncate_keys => non_neg_integer() | infinity,
        method => binary()
    }.
-type page_section() :: #{ title => binary(), path => binary(), body => binary() }.
-type page_response() ::
    #{
        content_type => binary(),
        body => binary()
    }.
-type describe_response() ::
    #{
        name => binary(),
        module => binary(),
        module_doc => binary(),
        cookbook => [map()],
        keys => [map()],
        types => map(),
        metadata => map()
    }.

-spec info(ao_message()) -> map().

%% @doc Export an explicit list of files via http. Filenames added to the
%% `hyperbuddy_serve' key of the node message will be served as static files.
%% Each filename must point to a path relative to the HyperBEAM instance's
%% build subdirectory as follows: `priv/html/hyperbuddy@1.0'.
info(Opts) ->
    ServedRoutes = hb_maps:get(hyperbuddy_serve, Opts, #{}, Opts),
    #{
        default => fun serve/4,
        serve => ServedRoutes#{
            % Default message viewer page:
            <<"index">> => <<"index.html">>,
            <<"bundle.js">> => <<"bundle.js">>,
            <<"fonts.css">> => <<"fonts.css">>,
            <<"font-dm-sans-italic.ttf">> => <<"font-dm-sans-italic.ttf">>,
            <<"font-dm-sans-variable.ttf">> => <<"font-dm-sans-variable.ttf">>,
            <<"font-geist-mono-variable.ttf">> => <<"font-geist-mono-variable.ttf">>,
            % Error pages:
            <<"404.html">> => <<"404.html">>,
            <<"500.html">> => <<"500.html">>,
            <<"styles.css">> => <<"styles.css">>,
            <<"script.js">> => <<"script.js">>
        },
        excludes => [<<"return_file">>]
    }.

-spec cookbook() -> [page_section()].

%% @doc Return example paths for the HyperBuddy device itself.
cookbook() ->
    [
        #{
            <<"title">> => <<"Inspect a device page">>,
            <<"path">> => <<"page">>,
            <<"body">> =>
                <<"Render the HyperBuddy page shell for the current subject.">>
        },
        #{
            <<"title">> => <<"Describe a target subject">>,
            <<"path">> => <<"describe?target=/~meta@1.0/build">>,
            <<"body">> =>
                <<"Return a normalized description for a target AO-Core path.">>
        },
        #{
            <<"title">> => <<"Pretty-print the environment">>,
            <<"path">> => <<"format?format=all">>,
            <<"body">> =>
                <<"Render the base, request, and node messages as formatted text.">>
        },
        #{
            <<"title">> => <<"Fetch browser-friendly page data">>,
            <<"path">> => <<"page-data?target=/~meta@1.0/build">>,
            <<"body">> =>
                <<"Return one JSON document with all of the current subject's documentation data.">>
        },
        #{
            <<"title">> => <<"Read event counters">>,
            <<"path">> => <<"events">>,
            <<"body">> => <<"Return the current node event counters.">>
        }
    ].

-spec metrics(ao_message(), hyperbuddy_request(), ao_message()) ->
    {ok, ao_message()}.

%% @doc The main HTML page for the REPL device.
metrics(_, Req, Opts) ->
    case hb_opts:get(prometheus, not hb_features:test(), Opts) of
        true ->
            {_, HeaderList, Body} =
            prometheus_http_impl:reply(
                #{path => true,
                headers => 
                    fun(Name, Default) ->
                        hb_ao:get(Name, Req, Default, Opts)
                    end,
                registry => prometheus_registry:exists(<<"default">>),
                standalone => false}
            ),
            RawHeaderMap =
                hb_maps:from_list(
                    prometheus_cowboy:to_cowboy_headers(HeaderList)
                ),
            Headers =
                hb_maps:map(
                    fun(_, Value) -> hb_util:bin(Value) end,
                    RawHeaderMap,
					Opts
                ),
            {ok, Headers#{ <<"body">> => Body }};
        false ->
            {ok, #{ <<"body">> => <<"Prometheus metrics disabled.">> }}
    end.

-spec events(ao_message(), ao_message(), ao_message()) -> {ok, ao_message()}.

%% @doc Return the current event counters as a message.
events(_, _Req, _Opts) ->
    {ok, hb_event:counters()}.

-spec describe(ao_message(), hyperbuddy_request(), ao_message()) ->
    {ok, describe_response()} | {error, term()}.

%% @doc Describe a target AO-Core path using the normalized HyperBuddy schema.
describe(_Base, Req, Opts) ->
    case hb_ao:get(<<"target">>, Req, Opts) of
        not_found ->
            {error, <<"No target path supplied.">>};
        Target ->
            case subject_from_target(hb_util:bin(Target), Opts) of
                {ok, Subject} ->
                    hb_introspect:describe(Subject, Opts);
                Error ->
                    Error
            end
    end.

-spec page(ao_message(), ao_message(), ao_message()) -> {ok, page_response()}.

%% @doc Render an interactive documentation page for the current subject.
page(_Base, _Req, _Opts) ->
    {ok,
        #{
            <<"content-type">> => <<"text/html; charset=utf-8">>,
            <<"body">> => page_shell()
        }
    }.

-spec page_data(ao_message(), hyperbuddy_request(), ao_message()) ->
    {ok, page_response()} | {error, term()}.

%% @doc Return one browser-friendly JSON payload for the current target subject.
page_data(_Base, Req, Opts) ->
    case hb_ao:get(<<"target">>, Req, Opts) of
        not_found ->
            {error, <<"No target path supplied.">>};
        Target ->
            case subject_from_target(hb_util:bin(Target), Opts) of
                {ok, Subject} ->
                    case hb_introspect:describe(Subject, Opts) of
                        {ok, Description} ->
                            {ok,
                                #{
                                    <<"content-type">> => <<"application/json; charset=utf-8">>,
                                    <<"body">> => hb_json:encode(Description)
                                }
                            };
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end.

-spec format(ao_message(), hyperbuddy_request(), ao_message()) -> {ok, ao_message()}.

%% @doc Employ HyperBEAM's internal pretty printer to format a message.
%% 
%% The request and node message can also be printed if desired by changing the
%% `format` key in the `format` call. This can be achieved easily using the
%% default key semantics:
%% ```
%% GET /.../~hyperbuddy@1.0/format=request
%% ```
%% Or a list of environment components:
%% ```
%% GET /.../~hyperbuddy@1.0/format+list=request,node
%% ```
%% Valid components are `base`, `request`, and `node`. The string `all` can also
%% be used to quickly include all of the components.
%% 
%% The `truncate-keys` key can also be used to truncate the number of keys
%% printed for each component. The default value is `infinity` (print all keys).
%% ```
%% GET /.../~hyperbuddy@1.0/format=request?truncate-keys=20
%% ```
format(Base, Req, Opts) ->
    % Find the scope of the environment that should be printed.
    Scope =
        lists:map(
            fun hb_util:bin/1,
            case hb_maps:get(<<"format">>, Req, <<"base">>, Opts) of
                <<"all">> -> [<<"base">>, <<"request">>, <<"node">>];
                Messages when is_list(Messages) -> Messages;
                SingleScope -> [SingleScope]
            end
        ),
    ?event(debug_format, {using_scope, Scope}),
    CombinedMsg =
        hb_maps:with(
            Scope,
            #{
                <<"base">> => maps:without([<<"device">>], hb_private:reset(Base)),
                <<"request">> => maps:without([<<"path">>], hb_private:reset(Req)),
                <<"node">> => hb_private:reset(Opts)
            },
            Opts
        ),
    MsgBeforeLoad =
        if map_size(CombinedMsg) == 1 ->
            hb_maps:get(hd(maps:keys(CombinedMsg)), CombinedMsg, #{}, Opts);
        true ->
            CombinedMsg
        end,
    MsgLoaded = hb_cache:ensure_all_loaded(MsgBeforeLoad, Opts),
    TruncateKeys =
        hb_maps:get(
            <<"truncate-keys">>,
            Req,
            hb_opts:get(debug_print_truncate, infinity, Opts),
            Opts
        ),
    ?event(debug_format, {using_truncation, TruncateKeys}),
    {ok,
        #{
            <<"body">> =>
                hb_util:bin(
                    hb_format:message(
                        MsgLoaded,
                        Opts#{
                            linkify_mode => discard,
                            cache_control => [<<"no-cache">>, <<"no-store">>],
                            debug_print_truncate => TruncateKeys
                        }
                    )
                )
        }
    }.

-spec throw(ao_message(), ao_message(), ao_message()) -> no_return() | {error, binary()}.

%% @doc Test key for validating the behavior of the `500` HTTP response.
throw(_Msg, _Req, Opts) ->
    case hb_opts:get(mode, prod, Opts) of
        prod -> {error, <<"Forced-throw unavailable in `prod` mode.">>};
        debug -> throw({intentional_error, Opts})
    end.

page_shell() ->
    iolist_to_binary(
        [
            <<"<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">">>,
            <<"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">">>,
            <<"<title>HyperBuddy</title><style>">>,
            page_css(),
            <<"</style></head><body>">>,
            <<"<main class=\"page\">">>,
            <<"<section class=\"hero\">">>,
            <<"<p class=\"eyebrow\">HyperBuddy</p>">>,
            <<"<h1 id=\"device-name\">Loading device...</h1>">>,
            <<"<p id=\"device-module\" class=\"module\"></p>">>,
            <<"<p id=\"device-doc\" class=\"lede\">Resolving the current subject and building its AO-Core reference page.</p>">>,
            <<"<div class=\"actions\">">>,
            <<"<a id=\"raw-describe\" class=\"button\" href=\"#\">Raw describe</a>">>,
            <<"<a id=\"raw-target\" class=\"button ghost\" href=\"#\">Open target</a>">>,
            <<"</div></section>">>,
            <<"<section class=\"panel\"><h2>Cookbook</h2><div id=\"cookbook\" class=\"grid\"></div></section>">>,
            <<"<section class=\"panel\"><h2>Keys</h2><div id=\"keys\" class=\"stack\"></div></section>">>,
            <<"<section class=\"panel\"><h2>Named Types</h2><div id=\"types\" class=\"stack\"></div></section>">>,
            <<"<section class=\"panel\"><details><summary>Metadata</summary><pre id=\"metadata\"></pre></details></section>">>,
            <<"</main><script>">>,
            page_script(),
            <<"</script></body></html>">>
        ]
    ).

page_css() ->
    <<"
    :root {
      --bg: #f5f1e8;
      --paper: #fffdf8;
      --ink: #10203a;
      --muted: #5c6a82;
      --line: #d7d1c2;
      --accent: #1e66ff;
      --accent-soft: #e7efff;
      --mono: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      --sans: 'DM Sans', ui-sans-serif, system-ui, sans-serif;
      color-scheme: light;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      font-family: var(--sans);
      background:
        radial-gradient(circle at top left, rgba(30,102,255,0.12), transparent 28rem),
        linear-gradient(180deg, #f8f4ec 0%, var(--bg) 100%);
      color: var(--ink);
    }
    .page { max-width: 1100px; margin: 0 auto; padding: 40px 24px 64px; }
    .hero, .panel {
      background: rgba(255, 253, 248, 0.9);
      border: 1px solid var(--line);
      border-radius: 24px;
      box-shadow: 0 18px 48px rgba(16, 32, 58, 0.08);
      backdrop-filter: blur(10px);
    }
    .hero { padding: 28px 30px; margin-bottom: 24px; }
    .panel { padding: 22px 24px; margin-bottom: 18px; }
    .eyebrow {
      margin: 0 0 8px;
      text-transform: uppercase;
      letter-spacing: 0.16em;
      font-size: 0.76rem;
      color: var(--muted);
      font-weight: 700;
    }
    h1, h2, h3 { margin: 0 0 12px; }
    h1 { font-size: clamp(2.4rem, 7vw, 4.5rem); line-height: 0.95; letter-spacing: -0.05em; }
    h2 { font-size: 1.4rem; letter-spacing: -0.03em; }
    h3 { font-size: 1rem; }
    .module, .lede, .muted { color: var(--muted); }
    .module { font-family: var(--mono); font-size: 0.92rem; }
    .actions { display: flex; gap: 12px; flex-wrap: wrap; margin-top: 20px; }
    .button {
      display: inline-flex;
      align-items: center;
      gap: 8px;
      padding: 11px 16px;
      border-radius: 999px;
      background: var(--accent);
      color: white;
      text-decoration: none;
      font-weight: 700;
    }
    .button.ghost {
      background: var(--accent-soft);
      color: var(--accent);
    }
    .grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
      gap: 14px;
    }
    .stack { display: grid; gap: 14px; }
    .card {
      border: 1px solid var(--line);
      border-radius: 18px;
      padding: 16px;
      background: var(--paper);
    }
    .pill {
      display: inline-block;
      border-radius: 999px;
      background: var(--accent-soft);
      color: var(--accent);
      padding: 4px 10px;
      font-family: var(--mono);
      font-size: 0.82rem;
      margin-bottom: 10px;
    }
    .path {
      display: inline-block;
      font-family: var(--mono);
      font-size: 0.86rem;
      background: #eef2f8;
      border-radius: 10px;
      padding: 6px 10px;
      margin: 8px 0 0;
      color: var(--ink);
      text-decoration: none;
    }
    .run {
      margin-top: 12px;
      display: inline-flex;
      color: var(--accent);
      text-decoration: none;
      font-weight: 700;
    }
    .type-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
      gap: 10px;
      margin-top: 12px;
    }
    .type-box {
      border: 1px solid var(--line);
      border-radius: 12px;
      padding: 10px 12px;
      background: #fbfaf6;
    }
    .type-box strong {
      display: block;
      margin-bottom: 6px;
      font-size: 0.78rem;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      color: var(--muted);
    }
    pre {
      overflow: auto;
      margin: 0;
      white-space: pre-wrap;
      word-break: break-word;
      font-family: var(--mono);
      font-size: 0.86rem;
      line-height: 1.45;
    }
    .empty { color: var(--muted); font-style: italic; }
    @media (max-width: 640px) {
      .page { padding: 20px 14px 40px; }
      .hero, .panel { border-radius: 18px; }
      .hero { padding: 22px 20px; }
      .panel { padding: 18px; }
    }">>.

page_script() ->
    <<"
    (() => {
      const suffix = '/~hyperbuddy@1.0/page';
      const pathname = window.location.pathname;
      const subject = pathname.endsWith(suffix) ? (pathname.slice(0, -suffix.length) || '/') : pathname;
      const pageDataUrl = new URL(window.location.href);
      pageDataUrl.pathname = pathname.replace(/\\/page$/, '/page-data');
      pageDataUrl.search = '';
      pageDataUrl.searchParams.set('target', subject);
      const describeUrl = new URL(window.location.href);
      describeUrl.pathname = pathname.replace(/\\/page$/, '/describe');
      describeUrl.search = '';
      describeUrl.searchParams.set('target', subject);

      const escapeHtml = (value) => String(value ?? '')
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/\"/g, '&quot;');
      const json = (value) => escapeHtml(JSON.stringify(value, null, 2));
      const unwrap = (value) => {
        if (value && typeof value === 'object' && value['ao-result'] === 'body' && value.body && typeof value.body === 'object') {
          return value.body;
        }
        if (value && typeof value === 'object' && value.body && typeof value.body === 'object' && !value.name && !value.keys) {
          return value.body;
        }
        return value;
      };
      const pathHref = (path) => {
        const cleanSubject = subject.replace(/\\/$/, '') || '/';
        const cleanPath = String(path || '').replace(/^\\/+/, '');
        return cleanPath ? cleanSubject + '/' + cleanPath : cleanSubject;
      };

      function renderCookbook(entries) {
        const root = document.getElementById('cookbook');
        if (!entries || !entries.length) {
          root.innerHTML = '<p class=\"empty\">No examples were declared for this device.</p>';
          return;
        }
        root.innerHTML = entries.map((entry) => `
          <article class=\"card\">
            <span class=\"pill\">${escapeHtml(entry.path || '')}</span>
            <h3>${escapeHtml(entry.title || entry.path || 'Untitled')}</h3>
            <p class=\"muted\">${escapeHtml(entry.body || '')}</p>
            <a class=\"path\" href=\"${pathHref(entry.path)}\">/${escapeHtml(entry.path || '')}</a>
            <a class=\"run\" href=\"${pathHref(entry.path)}\">Run example</a>
            ${entry.expected ? `<details><summary>Expected</summary><pre>${json(entry.expected)}</pre></details>` : ''}
          </article>
        `).join('');
      }

      function renderKeys(entries) {
        const root = document.getElementById('keys');
        if (!entries || !entries.length) {
          root.innerHTML = '<p class=\"empty\">No public keys were discovered.</p>';
          return;
        }
        root.innerHTML = entries.map((entry) => `
          <article class=\"card\">
            <span class=\"pill\">/${escapeHtml(entry.key || '')}</span>
            <h3>${escapeHtml(entry.key || '')}</h3>
            <p class=\"muted\">${escapeHtml(entry.doc || 'No function documentation is available yet.')}</p>
            <div class=\"type-grid\">
              <div class=\"type-box\"><strong>Base</strong><code>${escapeHtml(entry.base || 'any()')}</code></div>
              <div class=\"type-box\"><strong>Request</strong><code>${escapeHtml(entry.request || 'any()')}</code></div>
              <div class=\"type-box\"><strong>Return</strong><code>${escapeHtml(entry.return || 'any()')}</code></div>
            </div>
            <a class=\"run\" href=\"${pathHref(entry.key)}\">Resolve key</a>
            ${entry.signatures && entry.signatures.length ? `<details><summary>Signatures</summary><pre>${json(entry.signatures)}</pre></details>` : ''}
            ${entry.types && Object.keys(entry.types).length ? `<details><summary>Structured schema</summary><pre>${json(entry.types)}</pre></details>` : ''}
          </article>
        `).join('');
      }

      function renderTypes(types) {
        const root = document.getElementById('types');
        const entries = Object.entries(types || {});
        if (!entries.length) {
          root.innerHTML = '<p class=\"empty\">No named type aliases were extracted.</p>';
          return;
        }
        root.innerHTML = entries.map(([name, value]) => `
          <article class=\"card\">
            <span class=\"pill\">${escapeHtml(name)}</span>
            <pre>${json(value)}</pre>
          </article>
        `).join('');
      }

      async function load() {
        document.getElementById('raw-target').href = subject;
        document.getElementById('raw-describe').href = describeUrl.toString();
        try {
          const response = await fetch(pageDataUrl.toString(), { headers: { 'Accept': 'application/json' } });
          if (!response.ok) throw new Error(`HTTP ${response.status}`);
          const data = unwrap(await response.json());
          document.getElementById('device-name').textContent = data.name || 'Unknown device';
          document.getElementById('device-module').textContent = data.module || '';
          document.getElementById('device-doc').textContent = data['module-doc'] || 'No module documentation is available yet.';
          document.getElementById('metadata').textContent = JSON.stringify(data.metadata || {}, null, 2);
          renderCookbook(data.cookbook || []);
          renderKeys(data.keys || []);
          renderTypes(data.types || {});
        } catch (error) {
          document.getElementById('device-name').textContent = 'Failed to load device';
          document.getElementById('device-doc').textContent = String(error);
          document.getElementById('cookbook').innerHTML = '<p class=\"empty\">The target description could not be loaded.</p>';
          document.getElementById('keys').innerHTML = '';
          document.getElementById('types').innerHTML = '';
        }
      }

      load();
    })();
    ">>.

-spec subject_from_target(key_path(), ao_message()) ->
    {ok, ao_message()} | {error, term()}.

%% @doc Infer the message/device that a HyperBuddy page should describe from a
%% target AO-Core path without executing that target path.
subject_from_target(Target, Opts) ->
    Parsed = hb_singleton:from(#{ <<"path">> => Target }, Opts),
    case last_cast_device(Parsed) of
        {ok, Device} ->
            {ok, #{ <<"device">> => Device }};
        error ->
            maybe_cached_subject(Parsed, Opts)
    end.

last_cast_device(Parsed) when is_list(Parsed) ->
    lists:foldl(
        fun
            ({as, Device, _}, _Acc) -> {ok, hb_util:bin(Device)};
            (_, Acc) -> Acc
        end,
        error,
        Parsed
    );
last_cast_device(_) ->
    error.

maybe_cached_subject([Part | _], Opts) when ?IS_ID(Part) ->
    case hb_cache:read(Part, Opts) of
        {ok, Subject} when is_map(Subject) -> {ok, Subject};
        _ -> {ok, #{}}
    end;
maybe_cached_subject(_, _Opts) ->
    {ok, #{}}.

%% @doc Serve a file from the priv directory. Only serves files that are explicitly
%% listed in the `routes' field of the `info/1' return value.
serve(<<"keys">>, M1, _M2, Opts) -> dev_message:keys(M1, Opts);
serve(<<"set">>, M1, M2, Opts) -> dev_message:set(M1, M2, Opts);
serve(Key, _, _, Opts) ->
    ?event({hyperbuddy_serving, Key}),
    ServeRoutes = hb_maps:get(serve, info(Opts), #{}, Opts),
    case hb_maps:find(Key, ServeRoutes, Opts) of
        {ok, Filename} -> return_file(<<"hyperbuddy@1.0">>, Filename, #{});
        error -> {error, not_found}
    end.

%% @doc Read a file from disk and serve it as a static HTML page.
return_file(Device, Name) ->
    return_file(Device, Name, #{}).
return_file(Device, Name, Template) ->
    Base = hb_util:bin(code:priv_dir(hb)),
    Filename = <<Base/binary, "/html/", Device/binary, "/", Name/binary >>,
    ?event({hyperbuddy_serving, Filename}),
    case file:read_file(Filename) of
        {ok, RawBody} ->
            Body = apply_template(RawBody, Template),
            {ok, #{
                <<"body">> => Body,
                <<"content-type">> =>
                    case filename:extension(Filename) of
                        <<".html">> -> <<"text/html">>;
                        <<".js">> -> <<"text/javascript">>;
                        <<".css">> -> <<"text/css">>;
                        <<".png">> -> <<"image/png">>;
                        <<".ico">> -> <<"image/x-icon">>;
                        <<".ttf">> -> <<"font/ttf">>;
                        <<".json">> -> <<"application/json">>;
                        _ -> <<"text/plain">>
                    end
                }
            };
        {error, _} ->
            {error, not_found}
    end.

%% @doc Return an error page, with the `{{error}}` template variable replaced.
return_error(Error, Opts) when not is_map(Error) ->
    return_error(#{ <<"body">> => Error }, Opts);
return_error(ErrorMsg, Opts) ->
    return_file(
        <<"hyperbuddy@1.0">>,
        <<"500.html">>,
        #{ <<"error">> => hb_format:error(ErrorMsg, Opts) }
    ).

%% @doc Apply a template to a body.
apply_template(Body, Template) when is_map(Template) ->
    apply_template(Body, maps:to_list(Template));
apply_template(Body, []) ->
    Body;
apply_template(Body, [{Key, Value} | Rest]) ->
    ?event(debug_apply_template, {key, Key, value, Value}),
    apply_template(
        re:replace(
            Body,
            <<"\\{\\{", Key/binary, "\\}\\}">>,
            hb_util:bin(Value),
            [global, {return, binary}]
        ),
        Rest
    ).

%%% Tests

return_templated_file_test() ->
    {ok, #{ <<"body">> := Body }} =
        return_file(
            <<"hyperbuddy@1.0">>,
            <<"500.html">>,
            #{
                <<"error">> => <<"This is an error message.">>
            }
        ),
    ?assertNotEqual(
        binary:match(Body, <<"This is an error message.">>),
        nomatch
    ).

return_custom_json_test() ->
    Base = hb_util:bin(code:priv_dir(hb)),
    Filename = <<Base/binary, "/html/hyperbuddy@1.0/test.json">>,
    ok = file:write_file(Filename, <<"{\"status\":\"ok\"}">>),
    try
        ?assertMatch(
            {ok,
                #{
                    <<"body">> := JSONBin,
                    <<"content-type">> := <<"application/json">>
                }
            } when byte_size(JSONBin) > 0,
            hb_ao:resolve(
                #{
                    <<"device">> => <<"hyperbuddy@1.0">>
                },
                <<"custom.json">>,
                #{
                    hyperbuddy_serve => #{
                        <<"custom.json">> => <<"test.json">>
                    }
                }
            )
        )
    after
        file:delete(Filename)
    end.

page_data_test() ->
    {ok,
        #{
            <<"content-type">> := <<"application/json; charset=utf-8">>,
            <<"body">> := Body
        }
    } =
        page_data(
            #{},
            #{ <<"target">> => <<"/~meta@1.0/build">> },
            #{}
        ),
    Decoded = hb_json:decode(Body),
    ?assertMatch(
        #{
            <<"name">> := <<"meta@1.0">>,
            <<"module">> := <<"dev_meta">>,
            <<"cookbook">> := [_ | _],
            <<"keys">> := [_ | _]
        },
        Decoded
    ).

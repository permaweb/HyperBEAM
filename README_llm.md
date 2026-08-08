# hb-llm-device — `llm@1.0` for HyperBEAM

OpenAI-compatible LLM proxy for HyperBEAM. Use local Ollama, vLLM, or `llama.cpp` — or a remote vLLM like `spark-1b7b.local:8888` — from any AO process or via HTTP. Bypasses `dev_relay` localhost block by calling `hb_http` directly.

- `chat` → `POST /v1/chat/completions` (supports `stream=true` → SSE `text/event-stream`)
- `generate` → `chat` + extracts `choices[0].message.content` → `content` (+ `raw`)
- `embed`/`embeddings` → `POST /v1/embeddings` → `embedding`/`embeddings`/`raw`
- `qwen3.6` alias → `unsloth/Qwen3.6-35B-A3B-NVFP4` (Spark), `stream`, `endpoint`/`embed-endpoint`/`llm-endpoint` overrides

Default endpoint `http://spark-1b7b.local:8888/v1/chat/completions` (Spark `qwen3.6`), default model `unsloth/Qwen3.6-35B-A3B-NVFP4`. Override per-request — also works with local Ollama `http://localhost:11434`.

## Why not just `dev_relay@1.0`?

`dev_relay` blocks `localhost`/`127.0.0.1`/private IPs by default (`relay-block-internal=true`, `hb_hostname:is_public`). This device calls `hb_http:request` directly, so `http://localhost:11434` and `http://spark-1b7b.local:8888` work without `HB_RELAY_BLOCK_INTERNAL=false`. If you *do* want relay, `HB_RELAY_BLOCK_INTERNAL=false rebar3 shell`.

## Install

```bash
# Option A: copy into HyperBEAM (preloaded)
cp src/preloaded/llm/dev_llm.erl /path/to/hyperbeam/src/preloaded/llm/
cp src/preloaded/llm/llm_sidecar.lua /path/to/hyperbeam/src/preloaded/llm/
rebar3 compile
rebar3 device preload  # rebuilds _build/preloaded-store, index -> llm@1.0

# Option B: standalone repo as dep
# in your rebar.config: {deps, [{hb_llm_device, {git, "https://github.com/twilson63/hb-llm-device.git", {branch, "main"}}}]}
```

## Run a local AI with Ollama

```bash
# Install https://ollama.com
curl -fsSL https://ollama.com/install.sh | sh

# Start server ( :11434 )
ollama serve &
# Pull models
ollama pull llama3.2          # 3B default for Ollama
ollama pull qwen3.6:27b-coding-nvfp4  # or use Spark's qwen3.6 (35B) via spark-1b7b.local:8888
ollama pull nomic-embed-text  # for embeddings
ollama list  # should show llama3.2, qwen3.6, nomic-embed-text, glm-5.2:cloud etc.

# Verify Ollama API
curl http://localhost:11434/v1/models
curl http://localhost:11434/v1/chat/completions -H "Content-Type: application/json" \
  -d '{"model":"llama3.2","messages":[{"role":"user","content":"Say hi in 3 words"}],"stream":false}'
```

**Alternatives:**

```bash
# vLLM OpenAI-compatible (e.g. Spark)
python -m vllm.entrypoints.openai.api_server --model unsloth/Qwen3.6-35B-A3B-NVFP4 --port 8888 --host 0.0.0.0
# llama.cpp
llama-server -m qwen3.6-35b.gguf --port 8080 --host 127.0.0.1
```

## Test

```bash
# 1. Simple prompt (GET, default Spark qwen3.6) — no endpoint needed
curl "http://localhost:8734/~llm@1.0/chat?prompt=Write+a+haiku+about+AO"

# 2. Local Ollama llama3.2 (explicit endpoint)
curl "http://localhost:8734/~llm@1.0/chat?prompt=hello&endpoint=http://localhost:11434/v1/chat/completions&model=llama3.2"
curl -X POST http://localhost:8734/~llm@1.0/chat -H "content-type: application/json" \
  -d '{"messages":[{"role":"user","content":"What is HyperBEAM?"}],"model":"llama3.2","endpoint":"http://localhost:11434/v1/chat/completions"}'

# 3. Spark qwen3.6 via alias (default)
curl "http://localhost:8734/~llm@1.0/chat?prompt=hello&model=qwen3.6"
# or explicit full:
curl "http://localhost:8734/~llm@1.0/chat?prompt=hello&endpoint=http://spark-1b7b.local:8888/v1/chat/completions&model=unsloth/Qwen3.6-35B-A3B-NVFP4"

# 4. Streaming SSE
curl "http://localhost:8734/~llm@1.0/chat?prompt=Count+1+to+3&stream=true&model=qwen3.6"  # → data: {"delta":{"content":"1"}} … [DONE]

# 5. Embeddings (Ollama local)
curl -X POST http://localhost:8734/~llm@1.0/embed -H "content-type: application/json" \
  -d '{"input":"hello world","model":"nomic-embed-text","embed-endpoint":"http://localhost:11434/v1/embeddings"}'

# 6. Via AO process (set default on process)
# hb message commit --process <PROC> '{"device":"llm@1.0","llm-endpoint":"http://localhost:11434/v1/chat/completions","model":"llama3.2"}'
# or for Spark: '{"llm-endpoint":"http://spark-1b7b.local:8888/v1/chat/completions","model":"qwen3.6"}'
```

## From AO / Lua

Load `src/preloaded/llm/llm_sidecar.lua` into your AO process:

```bash
aos <PROC> < src/preloaded/llm/llm_sidecar.lua
```

```lua
-- non-stream
local res = ao.send({
  Device = "llm@1.0",
  Action = "chat",
  Tags = { Prompt = "Explain AO in one sentence", Model = "qwen3.6" }
})
print(res.Data)

-- streaming (sidecar sends Stream-Chunk / Stream-Done)
Send({ Target = ao.id, Action = "Chat", Prompt = "Count 1 to 5", Stream = "true", Model = "qwen3.6" })

-- or direct ao.resolve (no sidecar)
local status, res = ao.resolve({device='llm@1.0', path='chat', prompt='Say hi', model='qwen3.6', endpoint='http://spark-1b7b.local:8888/v1/chat/completions'})
print(res.body)

-- explicit Ollama endpoint
local status, res = ao.resolve({device='llm@1.0', path='chat', prompt='hi', model='llama3.2', endpoint='http://localhost:11434/v1/chat/completions'})
```

## Endpoints

- `chat` / `completions` — `POST /v1/chat/completions` passthrough (`messages` or `prompt`/`data` → `messages`, `model`, `stream`)
- `generate` — same as `chat` but extracts `choices[0].message.content` → `content` for easier AO use
- `embed` / `embeddings` — `POST /v1/embeddings` → `embedding`/`embeddings`

Overrides per-request: `endpoint` (chat), `embed-endpoint` (embed), `model`, `prompt`/`messages`/`data`, `stream` (`true`/`1`), or process Base `llm-endpoint`/`llm-embed-endpoint`.

## Security

This device can `POST` to any `endpoint` you pass — including `localhost` and private LAN `spark-1b7b.local`. Do not expose a node running it to the public internet without auth. Put it behind `hb` admin (`--admin`) or a reverse proxy, or restrict `llm-endpoint` to an allowlist.

## Forge publish

```bash
rebar3 device publish --device-src src/preloaded/llm --verbose
# dry-run:
rebar3 device publish --device-src src/preloaded/llm --dry-run --verbose
# Spec: PD89PcLv_ilAtTxDOPtbKLXNq4be5eBCMXJi1BS7KsY, Impl: RFJL5SpFbLN1X3LuqYnPDw4W02n5c4l8HGu-17tB-N8 (v1)
```

Published via `httpsig@1.0`, verifiable on Arweave. Others can `hb_ao:resolve({device=><<"llm@1.0">>})` after indexing or pin spec ID.

## Tests

```bash
rebar3 eunit --module dev_llm_test          # 12 mock + unit, no network
LLM_LIVE=1 rebar3 eunit --module dev_llm_test  # hits live Spark qwen3.6 (or Ollama)
rebar3 eunit                                 # full 987
```

## License

Apache-2.0

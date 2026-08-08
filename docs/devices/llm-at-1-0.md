# Device: ~llm@1.0

## Overview

The [`~llm@1.0`](../../src/preloaded/llm/dev_llm.erl) device is an OpenAI-compatible LLM proxy for HyperBEAM. It lets any AO process or HTTP client talk to a local Ollama, vLLM, or `llama.cpp` instance (or a remote vLLM like `spark-1b7b.local:8888`) via the OpenAI `chat/completions` and `embeddings` APIs. Unlike `~relay@1.0`, it calls `hb_http:request` directly so `localhost` and private LAN hosts are **not** blocked.

Source: `src/preloaded/llm/dev_llm.erl` (`-implements(<<"llm@1.0">>)`), sidecar `src/preloaded/llm/llm_sidecar.lua`, tests `src/core/test/dev_llm_test.erl`. Published via `forge` as `llm@1.0` (Spec `2fJihfIyiV3iN7LYA0HMSRV8_pxUx3zkBoi8ZtNV7bE`, Impl `PN-CoM1TpYUa_03D9AT1zH_0zBvJH_HsMIq-hp1XCYY`, Signer `aa0b-vzWf7Sn4cFKI43P4MUcSgAdAjCH2V2IFTKZGfU`) — previous Spec `PD89PcLv_ilAtTxDOPtbKLXNq4be5eBCMXJi1BS7KsY` deprecated.

Default endpoint `http://spark-1b7b.local:8888/v1/chat/completions`, default model `unsloth/Qwen3.6-35B-A3B-NVFP4` (`qwen3.6` alias). Override per-request via `endpoint`, `model`, `llm-endpoint`.

## Core Concept: OpenAI Proxy + Streaming

The device builds an OpenAI `POST /v1/chat/completions` JSON body from `prompt`/`messages`/`data` + `model` + `stream`, posts it with `hb_http:request` to the configured endpoint, and returns the response. With `stream=true` it returns SSE `text/event-stream` (`data: {"choices":[{"delta":{"content":"..."}}]}` chunks + `data: [DONE]`), which the Lua sidecar splits into per-chunk `Send`s.

Forge publish: `rebar3 device publish --device-src src/preloaded/llm --verbose` (Spec/Impl IDs above, signer `aa0b...`).

## Key Functions (Keys)

* **`chat` / `completions`**
    * **Action:** OpenAI `POST /v1/chat/completions` passthrough. Supports `stream=true` → SSE.
    * **Inputs (from `Req` then `Base`):**
        * `prompt` | `data` | `messages`: prompt string or `messages=[{role,content}]` array (decoded if JSON binary). `prompt`/`data` → `messages=[{role=user,content}]`.
        * `model`: model ID (default `unsloth/Qwen3.6-35B-A3B-NVFP4`, aliases `qwen3.6`, `qwen3.6:27b` → full). Also `llama3.2`, `glm-5.2:cloud`, etc. when pointing at Ollama.
        * `endpoint`: full URL (default `http://spark-1b7b.local:8888/v1/chat/completions`, or `http://localhost:11434/v1/chat/completions` for Ollama). Also `llm-endpoint` on Base.
        * `stream` | `Stream`: `true`/`"true"`/`1` → `{"stream":true}` and `content-type: text/event-stream` on response.
        * `body`: raw JSON binary passthrough (merged with `stream` flag).
    * **Response:** `{ok, #{<<"body">>:=JSON, <<"status">>:=200, <<"headers">>:=#{...}, <<"raw">>:=#{...}}}`; with `stream=true`, `headers` includes `<<"content-type">>=><<"text/event-stream">>` and `body` is SSE.
    * **Example HyperPATH:**
        ```
        GET /~llm@1.0/chat?prompt=Say+hello+in+3+words&model=qwen3.6
        GET /~llm@1.0/chat?prompt=hello&endpoint=http://localhost:11434/v1/chat/completions&model=llama3.2&stream=true
        POST /~llm@1.0/chat  {"messages":[{"role":"user","content":"hi"}],"model":"qwen3.6"}
        ```

* **`generate`**
    * **Action:** Same as `chat` but extracts `choices[0].message.content` (or `delta.content`) → `<<"content">>` for easier AO use.
    * **Inputs:** Same as `chat`.
    * **Response:** `{ok, Res#{<<"content">>:=Binary, <<"raw">>:=DecodedJSON}}` plus original `body`/`status`.
    * **Example:** `GET /~llm@1.0/generate?prompt=What+is+2%2B2%3F+One+word.&model=qwen3.6` → `Four`

* **`embed` / `embeddings`**
    * **Action:** `POST /v1/embeddings`.
    * **Inputs:** `input` | `prompt` | `data` (text), `model` (default `nomic-embed-text`), `embed-endpoint` | `llm-embed-endpoint` (default `http://spark-1b7b.local:8888/v1/embeddings` or derived from `llm-endpoint` → `/embeddings`).
    * **Response:** `{ok, #{<<"embedding">>:=SingleVec, <<"embeddings">>:=ListVecs, <<"raw">>:=Decoded, <<"body">>:=JSON}}`
    * **Example:** `POST /~llm@1.0/embed {"input":"hello world","model":"nomic-embed-text"}`

* **Streaming (SSE)**
    * Set `stream=true` (or `Stream=true`) on `chat`/`generate` request. Device sends `{"model":..., "messages":..., "stream":true}` to LLM and returns `text/event-stream` SSE: `data: {"choices":[{"delta":{"content":"Hello "}}]}\n\ndata: [DONE]\n\n`. `curl` prints chunks as they arrive; Lua sidecar parses `data: [^\n]+` → `json.decode` → `delta.content` → `Send({Tags={["Stream-Chunk"]="true"}, Data=content})` per chunk + `Stream-Done`.
    * **Example stream:**
        ```
        curl "http://localhost:8734/~llm@1.0/chat?prompt=Count+1+to+3&model=qwen3.6&stream=true"
        # → data: {"id":"chatcmpl-...","choices":[{"delta":{"content":"1"}}]} ...
        ```

## Local AI with Ollama

```bash
# Install https://ollama.com
curl -fsSL https://ollama.com/install.sh | sh
ollama serve &  # :11434
ollama pull llama3.2
ollama pull nomic-embed-text
ollama pull qwen3.6:27b-coding-nvfp4  # or use Spark's qwen3.6 via spark-1b7b.local:8888
ollama list
curl http://localhost:11434/v1/models
curl http://localhost:11434/v1/chat/completions -H "Content-Type: application/json" \
  -d '{"model":"llama3.2","messages":[{"role":"user","content":"Say hi in 3 words"}],"stream":false}'
# HyperBEAM via llm device (explicit Ollama endpoint):
curl "http://localhost:8734/~llm@1.0/chat?prompt=Say+hi&model=llama3.2&endpoint=http://localhost:11434/v1/chat/completions&stream=true"
```

Alternatives: `python -m vllm.entrypoints.openai.api_server --model unsloth/Qwen3.6-35B-A3B-NVFP4 --port 8888` (Spark), `llama-server -m qwen.gguf --port 8080`.

## AO / Lua Usage

```lua
-- load sidecar once:
-- aos <PROC> < src/preloaded/llm/llm_sidecar.lua

-- non-stream
local res = ao.send({Device="llm@1.0", Action="chat", Tags={Prompt="Explain AO in one sentence", Model="qwen3.6"}})
print(res.Data) -- JSON body with choices

-- streaming (sidecar sends Stream-Chunk per SSE delta)
Send({Target=ao.id, Action="Chat", Prompt="Count 1 to 5", Stream="true", Model="qwen3.6"})

-- direct ao.resolve (no sidecar)
local status, res = ao.resolve({device='llm@1.0', path='chat', prompt='hi', model='qwen3.6', endpoint='http://spark-1b7b.local:8888/v1/chat/completions'})
print(res.body)
local status, res = ao.resolve({device='llm@1.0', path='chat', prompt='hi', model='llama3.2', endpoint='http://localhost:11434/v1/chat/completions', stream='true'})

-- embeddings (Ollama local)
local status, res = ao.resolve({device='llm@1.0', path='embed', input='hello world', model='nomic-embed-text', ['embed-endpoint']='http://localhost:11434/v1/embeddings'})
```

## Security

The device `POST`s to any `endpoint` you pass, including `localhost` and private LAN. Do not expose a node running it to the public internet without auth (`hb --admin` or reverse proxy) or an `llm-endpoint` allowlist.

## Tests

`src/core/test/dev_llm_test.erl` — 12 tests (unit `resolve_endpoint`/`build_chat_body`/`is_stream`/`extract_content`, mock `gen_tcp` `chat|generate|embed|stream` + `hb_ao:resolve` + `lua@5.3a` `ao.resolve`, live `LLM_LIVE=1` via Spark). `rebar3 eunit --module dev_llm_test` `All 12 passed`, `rebar3 eunit` `987`.

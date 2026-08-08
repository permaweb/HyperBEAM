#!/usr/bin/env bash
set -e
BASE=http://localhost:8734
AGENT=researcher
COLLECTION=agent-researcher

echo "== 1. Register skills (idempotent) =="
for f in research summarize write_essay research-write; do
  # skills.json contains all four; register via jq
  PAYLOAD=$(jq ".[] | select(.name==\"$f\")" skills.json)
  curl -s -X POST $BASE/~skills@1.0/register -H 'content-type: application/json' -d "$PAYLOAD" | jq .
done

echo "== 2. Verify harness + skills =="
curl -s $BASE/~skills@1.0/list | jq .
curl -s "$BASE/~skills@1.0/check?skill=research&tools=[\"fetch\"]" | jq .

echo "== 3. Start agent process (aOS) ==
# In another terminal: aos $AGENT < src/preloaded/agent/agent.lua
# Or via HB direct:"

echo "== 4. Run composed skill as agent researcher (uses LLM+Harness+Tools+Instructions) =="
curl -s -X POST $BASE/~skills@1.0/run -H 'content-type: application/json' -d '{
  "skill": "research-write",
  "agent_tools": ["fetch"],
  "message": "Research Microsoft Natick, Nautilus floating, Kepler/space datacenters. Write 800w essay: water vs space. Include table Power|Cooling|Latency|Repair|Cost and sources. Use identity/user/soul + memory.",
  "collection": "'$COLLECTION'",
  "model": "qwen3.6"
}' | jq -r '.output // .content' | tee essay.out.md

echo ""
echo "== 5. Direct harness equivalent (without skills indirection) =="
curl -s -X POST $BASE/~harness@1.0/handle -H 'content-type: application/json' -d '{
  "message": "Take docs and write essay: Datacenters in Water and Space (see skills.json write_essay instructions)",
  "tools": [{"type":"function","function":{"name":"fetch","description":"Fetch URL via relay","parameters":{"type":"object","properties":{"relay-path":{"type":"string"}}}}}],
  "collection": "'$COLLECTION'",
  "model": "qwen3.6"
}' | jq -r '.output' | head -n 40

echo "== Done. Output saved to essay.out.md, history in hb_store $COLLECTION, durable to agents/researcher/memory/ =="

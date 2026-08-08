# Datacenters in Water and Space

*Essay produced by agent `researcher` via `skills@1.0/research-write` → `harness@1.0/handle` → `llm@1.0` + `relay@1.0/fetch` (stubbed example output — replace with live run of `run.sh`).*

Water cools. Space radiates. Both sound exotic until you price power, repair, and latency.

## Water: Natick and Floaters

Microsoft's Project Natick (2018–2020) sank a 40-foot vessel off Orkney with 864 servers. Seawater is the heat exchanger: no chillers, PUE ~1.07 in the trial versus ~1.2 on land. Deployment was fast (90 days dock-to-power) and failure rate 1/8th of land — fewer humans, fewer bumps, stable temperature.

Nautilus and newer floating concepts extend it: barges or spar platforms with seawater loops and tide/moored power. Cooling is essentially free, but biofouling, corrosion, and cable cuts are not. Retrieval is a crane, not a keycard. You trade HVAC opex for marine ops.

## Space: Kepler, Starcloud, and Solar

Space sells infinite solar (~1.36 kW/m²) and radiative cooling to 3K. Kepler's orbital edge and Starcloud's 1–5 kW prototype racks bet that launch cost ($1.5–2.6k/kg on Falcon 9, falling) plus zero cooling beats earthbound power bills.

Physics helps: no air means no fans, just radiators. Power is continuous in sun-sync orbit, ~65% sun in LEO otherwise. But radiation hardens everything (ECC, rad-tolerant boards), and repair is impossible — you ship redundancy or you lose the rack. Latency is 10–40 ms extra per hop to ground, and bandwidth is the real tax: laser downlinks at 100+ Gbps are coming, but not cheap.

|  | Water (Natick-like) | Space (LEO) |
|---|---|---|
| **Power** | Shore + seawater cooling, ~1.07 PUE | Solar + batteries, free after launch |
| **Cooling** | Seawater loop, biofouling risk | Radiators to deep space, no moving parts |
| **Latency** | ~1 ms extra (shore cable) | 10–40 ms + ground hop |
| **Repair** | Crane in days/weeks | No repair; redundancy only |
| **Cost** | Vessel + mooring + cable | Launch $ + rad-hardening + laser link |

## Boring Tradeoff

Water wins near-term: it is just a better heat sink. Space wins only if power cost dominates and you never need a screwdriver. The harness is the same lesson: `Agent = LLM + Harness + Tools + Instructions`. Replace the tool (`fetch` via `relay`) or the skill (`research → summarize → write`), the agent still works — because the harness checks `requires_tools` against `tools.json`, not hard-coded code. Hyper holds the loop, skills hold the know-how, tools hold the access, instructions hold the memory.

**Sources:** Microsoft Natick Phase 2 report; Nautilus Data Technologies overview; Kepler Communications orbital DC concept; Starcloud demo (2025); Axiom Space radiators review.

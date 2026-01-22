import http from "k6/http";
import { SharedArray } from "k6/data";

// Toggle between sequential (true) or random (false) fetching
const SEQUENTIAL = true;

// Load IDs from file once
const ids = new SharedArray("txids", () =>
	// tx_id_reply.log
	// unique_tx_id_reply.log
	// only_unique_200_tx.log
  open("only_unique_200_tx.log").split("\n").filter(id => id.length > 0)
);

export let options = {
  vus: 20,
  duration: "3m",
};

export default function () {
  let id;
  if (SEQUENTIAL) {
    // Sequential: each VU gets a unique ID per iteration
    // VU 1 iter 0 -> id[0], VU 2 iter 0 -> id[1], ..., VU 1 iter 1 -> id[30], etc.
    const index = (__VU - 1) + (__ITER * options.vus);
    id = ids[index % ids.length];
  } else {
    // Random: pick a random ID (can be repeated)
    id = ids[Math.floor(Math.random() * ids.length)];
  }

  // make HTTP GET request
  http.get(`http://localhost:8734/${id}`, { timeout: "60s" });
}

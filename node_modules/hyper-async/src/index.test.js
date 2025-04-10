import { test } from "uvu";
import * as assert from "uvu/assert";

import Async, { of, fromPromise } from "./index.js";

const prop = (k) => (o) => o[k];
const get = (url) => fromPromise(fetch)(url);
const toJSON = (res) => fromPromise(res.json.bind(res))();

test("ok", async () => {
  const x = await of(1)
    .chain((x) => get("https://jsonplaceholder.typicode.com/posts/" + x))
    .chain(toJSON)
    .map(prop("title"))
    .toPromise();
  assert.equal(
    x,
    "sunt aut facere repellat provident occaecati excepturi optio reprehenderit"
  );
});

test("ok2", async () => {
  const x = await Async.of(1)
    .chain((x) =>
      Async.fromPromise(fetch)(
        "https://jsonplaceholder.typicode.com/posts/" + x
      )
    )
    .chain(toJSON)
    .map(prop("title"))
    .toPromise();
  assert.equal(
    x,
    "sunt aut facere repellat provident occaecati excepturi optio reprehenderit"
  );
});

test("test bimap", async () => {
  const x = await Async.of(1)
    .chain((x) => Async.Rejected(2))
    .bimap(
      (x) => x + 1,
      (x) => 2
    )
    .toPromise()
    .catch((x) => x);

  assert.equal(x, 3);
});

test("test bichain", async () => {
  const x = await Async.of(1)
    .chain((x) => Async.Rejected(2))
    .bichain((x) => Async.Rejected(x + 1), Async.Resolved)
    .toPromise()
    .catch((x) => x);

  assert.equal(x, 3);
});

test.run();

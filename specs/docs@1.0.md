# `docs@1.0` — the documentation pages of a node

- **Device name:** `docs@1.0`
- **Depends-on:** `message@1.0` (its `info` key gives the info of a device as
  a message of this device), `httpsig@1.0` (it resolves `index` upon a reply
  that has no body), `query@1.0` (the page finds recipes through its GraphQL).
- **Status:** Draft

## 1. Overview

`docs@1.0` serves the documentation of the devices of a node. A browser that
requests the `info` of a message receives a page that describes the device of
that message: its schema, its specification, its recipes and its
implementations. The node's `docs-device` option selects the device of the
info; its default is `docs@1.0`.

The device holds the whole site in its package: the page, its scripts and
styles, its fonts, and its guides. The page is the same for every device. Its
script reads the data of a device from the node, and renders it.

## 2. Concepts & terminology

- **Info:** the message that `message@1.0` gives for the key `info`. Its
  `device` is the node's `docs-device`, its `for` is the device that it
  describes, its `schema` is the schema of the device and its
  `specification` the specification of the device. A reply with
  `accept-bundle: true` holds both messages; without it, they are `ao:`
  links. A device's own `info/3` answers in its place, and a device with a
  `handler` or a `default` answers `info` itself, as it does any other key.
- **Schema:** the `-spec`s of a device's module, as `hb_types:extract/1`
  gives them: a message whose keys are the keys of the device, each with the
  list of its clauses. The keys of each message type in a clause are a list
  of `{name, presence, type}`.
- **Specification:** the signed `Device-Specification` message of the
  device's name in the node's preloaded index. Its body is the device's
  `-specification` file, or the module's `%%% @doc` block without one.
- **Recipe:** an item with the tags `type: Device-Recipe` and
  `recipe-for-device: <spec ID>`. Its body is Markdown, whose code blocks are
  requests that the page can run.
- **Package:** the files of the device's implementation archive, which the
  loader extracts when it loads the device.

## 3. Device interface

The device answers four keys. Every other key falls through to
`message@1.0`, so the keys of an info (`for`, `schema`, `specification`) are
read from it. A message of this device with an `asset-directory` key is a
directory of the package: each of its keys is a file or a directory in it.

## 4. Keys

### `index`

Returns `index.html` from the package, with
`content-type: text/html; charset=utf-8` and `cache-control: no-store`. The
bytes are the same for every device.

### `implementations`

Returns `{specification, implementations}` for the device in the `for` of the
request, or else of the base: the ID of the signature of its specification,
and every implementation ID of that specification that the node's loader can
find: in the preloaded store, in the node's stores, and from the trusted
signers on Arweave. A node with `force-signed` set signs the `hashpath` of the
reply, so the signature attests the lookup. `cache-control: no-store`.

### `devices`

Returns the node's preloaded index: each device name, with the ID of its
specification. `cache-control: no-store`.

### `asset`

Returns a file of the package. The path of the file is either the request's
`asset` key (`/~docs@1.0/asset=site.css`) or the path parts that follow
`asset` (`/~docs@1.0/asset/fonts/dm-sans-400.woff2`), so that the relative
URLs of a stylesheet resolve. Each part of the path is split at `/`, which a
URL can encode inside a part as `%2F`, and a path with an element that is
empty, `.` or `..` is refused with `not_found`. The reply has the content
type of the file's extension and `cache-control: public, max-age=3600`.

## 5. The page

The page is reached by device-first paths:

- `/~message@1.0/info?for=<device>`: the page of any device;
- `/~<device>/info`: the same, for a device that answers `info` through
  `message@1.0`;
- `/~docs@1.0`: the home of the node, with its guides and devices.

Its script, `docs.js`, reads:

1. the info of the device, as JSON with `accept-bundle: true`, which holds
   its schema and its specification. The signer of the specification is the
   `committer` of its signature;
2. the recipes for the specification, through the node's
   `POST /~query@1.0/graphql`. Recipes signed by the signer of the
   specification are listed as **Recipes**, and all others as **Community
   recipes**;
3. the implementations of the device;
4. the device list, and the assets of the page.

It renders them with the device's Markdown renderer, `markdown.js`, which any
page can load from `/~docs@1.0/asset/markdown.js`.

## 6. Security

- The runner of the page sends a recipe's requests with `credentials: 'omit'`,
  so that they do not carry the visitor's cookies.
- A community recipe is rendered without raw HTML, and the first click on one
  of its requests shows a caution that someone else made it. Only a second
  click sends the request.
- `markdown.js` renders a link only if it is relative, or its scheme is
  `http`, `https` or `mailto`.

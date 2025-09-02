var __defProp = Object.defineProperty;
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};

// src/index.common.js
import { connect as schedulerUtilsConnect, locate } from "@permaweb/ao-scheduler-utils";

// node_modules/hyper-async/dist/index.js
var Async = (fork) => ({
  fork,
  toPromise: () => new Promise((resolve, reject) => fork(reject, resolve)),
  map: (fn) => Async((rej, res) => fork(rej, (x) => res(fn(x)))),
  bimap: (f, g) => Async(
    (rej, res) => fork(
      (x) => rej(f(x)),
      (x) => res(g(x))
    )
  ),
  chain: (fn) => Async((rej, res) => fork(rej, (x) => fn(x).fork(rej, res))),
  bichain: (f, g) => Async(
    (rej, res) => fork(
      (x) => f(x).fork(rej, res),
      (x) => g(x).fork(rej, res)
    )
  ),
  fold: (f, g) => Async(
    (rej, res) => fork(
      (x) => f(x).fork(rej, res),
      (x) => g(x).fork(rej, res)
    )
  )
});
var of = (x) => Async((rej, res) => res(x));
var Resolved = (x) => Async((rej, res) => res(x));
var Rejected = (x) => Async((rej, res) => rej(x));
var fromPromise = (f) => (...args) => Async(
  (rej, res) => f(...args).then(res).catch(rej)
);

// src/client/signer.js
import { Buffer as BufferShim3 } from "buffer/index.js";
import base64url4 from "base64url";
import { httpbis } from "http-message-signatures";
import { parseItem, serializeList } from "structured-headers";

// src/lib/data-item.js
import { Buffer as BufferShim } from "buffer/index.js";
import base64url from "base64url";
import * as ArBundles from "@dha-team/arbundles";
if (!globalThis.Buffer) globalThis.Buffer = BufferShim;
var pkg = ArBundles.default ? ArBundles.default : ArBundles;
var { createData, DataItem, SIG_CONFIG } = pkg;
function createDataItemBytes(data, signer, opts) {
  const signerMeta = SIG_CONFIG[signer.type];
  if (!signerMeta) throw new Error(`Metadata for signature type ${signer.type} not found`);
  signerMeta.signatureType = signer.type;
  signerMeta.ownerLength = signerMeta.pubLength;
  signerMeta.signatureLength = signerMeta.sigLength;
  signerMeta.publicKey = signer.publicKey;
  const dataItem = createData(data, signerMeta, opts);
  return dataItem.getRaw();
}
function getSignatureData(dataItemBytes) {
  const dataItem = new DataItem(dataItemBytes);
  return dataItem.getSignatureData();
}
function verify(dataItemBytes) {
  return DataItem.verify(dataItemBytes);
}

// src/client/hb.js
import { omit, keys } from "ramda";
import base64url3 from "base64url";

// src/lib/utils.js
import {
  F,
  T,
  __,
  append,
  assoc,
  chain,
  concat,
  cond,
  defaultTo,
  equals,
  has,
  includes,
  is,
  join,
  map,
  pipe,
  propOr,
  reduce
} from "ramda";
import { ZodError, ZodIssueCode } from "zod";
var joinUrl = ({ url, path: path2 }) => {
  if (!path2) return url;
  if (path2.startsWith("/")) return joinUrl({ url, path: path2.slice(1) });
  url = new URL(url);
  url.pathname += path2;
  return url.toString();
};
function parseTags(rawTags) {
  return pipe(
    defaultTo([]),
    reduce(
      (map3, tag) => pipe(
        // [value, value, ...] || []
        propOr([], tag.name),
        // [value]
        append(tag.value),
        // { [name]: [value, value, ...] }
        assoc(tag.name, __, map3)
      )(map3),
      {}
    ),
    /**
    * If the field is only a singly list, then extract the one value.
    *
    * Otherwise, keep the value as a list.
    */
    map((values) => values.length > 1 ? values : values[0])
  )(rawTags);
}
function eqOrIncludes(val) {
  return cond([
    [is(String), equals(val)],
    [is(Array), includes(val)],
    [T, F]
  ]);
}
function errFrom(err) {
  let e;
  if (is(ZodError, err)) {
    e = new Error(mapZodErr(err));
    e.stack += err.stack;
  } else if (is(Error, err)) {
    e = err;
  } else if (has("message", err)) {
    e = new Error(err.message);
  } else if (is(String, err)) {
    e = new Error(err);
  } else {
    e = new Error("An error occurred");
  }
  return e;
}
function mapZodErr(zodErr) {
  return pipe(
    (zodErr2) => (
      /**
       * Take a ZodError and flatten it's issues into a single depth array
       */
      function gatherZodIssues(zodErr3, status, contextCode) {
        return reduce(
          (issues, issue) => pipe(
            cond([
              /**
               * These issue codes indicate nested ZodErrors, so we resursively gather those
               * See https://github.com/colinhacks/zod/blob/HEAD/ERROR_HANDLING.md#zodissuecode
               */
              [
                equals(ZodIssueCode.invalid_arguments),
                () => gatherZodIssues(issue.argumentsError, 422, "Invalid Arguments")
              ],
              [
                equals(ZodIssueCode.invalid_return_type),
                () => gatherZodIssues(issue.returnTypeError, 500, "Invalid Return")
              ],
              [
                equals(ZodIssueCode.invalid_union),
                // An array of ZodErrors, so map over and flatten them all
                () => chain((i) => gatherZodIssues(i, 400, "Invalid Union"), issue.unionErrors)
              ],
              [T, () => [{ ...issue, status, contextCode }]]
            ]),
            concat(issues)
          )(issue.code),
          [],
          zodErr3.issues
        );
      }(zodErr2, 400, "")
    ),
    /**
     * combine all zod issues into a list of { message, status }
     * summaries of each issue
     */
    (zodIssues) => reduce(
      (acc, zodIssue) => {
        const { message: message2, path: _path, contextCode: _contextCode } = zodIssue;
        const path2 = _path[1] || _path[0];
        const contextCode = _contextCode ? `${_contextCode} ` : "";
        acc.push(`${contextCode}'${path2}': ${message2}.`);
        return acc;
      },
      [],
      zodIssues
    ),
    join(" | ")
  )(zodErr);
}

// src/client/hb-encode.js
import base64url2 from "base64url";
import { Buffer as BufferShim2 } from "buffer/index.js";
if (!globalThis.Buffer) globalThis.Buffer = BufferShim2;
var MAX_HEADER_LENGTH = 4096;
async function hasNewline(value) {
  if (typeof value === "string") return value.includes("\n");
  if (value instanceof Blob) {
    value = await value.text();
    return value.includes("\n");
  }
  if (isBytes(value)) return Buffer.from(value).includes("\n");
  return false;
}
async function sha256(data) {
  return crypto.subtle.digest("SHA-256", data);
}
function isBytes(value) {
  return value instanceof ArrayBuffer || ArrayBuffer.isView(value);
}
function isPojo(value) {
  return !isBytes(value) && !Array.isArray(value) && !(value instanceof Blob) && typeof value === "object" && value !== null;
}
function hbEncodeValue(value) {
  if (isBytes(value)) {
    if (value.byteLength === 0) return hbEncodeValue("");
    return [void 0, value];
  }
  if (typeof value === "string") {
    if (value.length === 0) return [void 0, "empty-binary"];
    return [void 0, value];
  }
  if (Array.isArray(value)) {
    if (value.length === 0) return ["empty-list", void 0];
    const encoded = value.reduce(
      (acc, cur) => {
        let [type, curEncoded] = hbEncodeValue(cur);
        if (!type) type = "binary";
        acc.push(`(ao-type-${type}) ${curEncoded}`);
        return acc;
      },
      []
    );
    return ["list", encoded.join(",")];
  }
  if (typeof value === "number") {
    if (!Number.isInteger(value)) return ["float", `${value}`];
    return ["integer", `${value}`];
  }
  if (typeof value === "symbol") {
    return ["atom", value.description];
  }
  throw new Error(`Cannot encode value: ${value.toString()}`);
}
function hbEncodeLift(obj, parent = "", top = {}) {
  const [flattened, types] = Object.entries({ ...obj }).reduce((acc, [key, value]) => {
    const flatK = (parent ? `${parent}/${key}` : key).toLowerCase();
    if (value == null) return acc;
    if (Array.isArray(value) && value.some(isPojo)) {
      value = value.reduce((indexedObj, v, idx) => Object.assign(indexedObj, { [idx]: v }), {});
    }
    if (isPojo(value)) {
      hbEncodeLift(value, flatK, top);
      return acc;
    }
    const [type, encoded] = hbEncodeValue(value);
    if (encoded) {
      if (Buffer.from(encoded).byteLength > MAX_HEADER_LENGTH) {
        top[flatK] = encoded;
      } else acc[0][key] = encoded;
    }
    if (type) acc[1][key] = type;
    return acc;
  }, [{}, {}]);
  if (Object.keys(flattened).length === 0) return top;
  if (Object.keys(types).length > 0) {
    const aoTypes = Object.entries(types).map(([key, value]) => `${key.toLowerCase()}=${value}`).join(",");
    if (Buffer.from(aoTypes).byteLength > MAX_HEADER_LENGTH) {
      const flatK = parent ? `${parent}/ao-types` : "ao-types";
      top[flatK] = aoTypes;
    } else flattened["ao-types"] = aoTypes;
  }
  if (parent) top[parent] = flattened;
  else Object.assign(top, flattened);
  return top;
}
function encodePart(name, { headers, body }) {
  const parts = Object.entries(Object.fromEntries(headers)).reduce((acc, [name2, value]) => {
    acc.push(`${name2}: `, value, "\r\n");
    return acc;
  }, [`content-disposition: form-data;name="${name}"\r
`]);
  if (body) parts.push("\r\n", body);
  return new Blob(parts);
}
async function encode(obj = {}) {
  if (Object.keys(obj) === 0) return;
  const flattened = hbEncodeLift(obj);
  const bodyKeys = [];
  const headerKeys = [];
  await Promise.all(
    Object.keys(flattened).map(async (key) => {
      const value = flattened[key];
      if (isPojo(value)) {
        const subPart = await encode(value);
        if (!subPart) return;
        bodyKeys.push(key);
        flattened[key] = encodePart(key, subPart);
        return;
      }
      if (await hasNewline(value) || key.includes("/") || Buffer.from(value).byteLength > MAX_HEADER_LENGTH) {
        bodyKeys.push(key);
        flattened[key] = new Blob([
          `content-disposition: form-data;name="${key}"\r
\r
`,
          value
        ]);
        return;
      }
      headerKeys.push(key);
      flattened[key] = value;
    })
  );
  const h = new Headers();
  headerKeys.forEach((key) => h.append(key, flattened[key]));
  if (h.has("data")) {
    bodyKeys.push("data");
  }
  let body, finalContent;
  if (bodyKeys.length) {
    if (bodyKeys.length === 1) {
      body = new Blob([obj.data]);
      h.append("inline-body-key", bodyKeys[0]);
      h.delete(bodyKeys[0]);
    } else {
      const bodyParts = await Promise.all(
        bodyKeys.map((name) => {
          return flattened[name].arrayBuffer();
        })
      );
      const base = new Blob(
        bodyParts.flatMap((p, i, arr) => i < arr.length - 1 ? [p, "\r\n"] : [p])
      );
      const hash = await sha256(await base.arrayBuffer());
      const boundary = base64url2.encode(Buffer.from(hash));
      const blobParts = bodyParts.flatMap((p) => [`--${boundary}\r
`, p, "\r\n"]);
      blobParts.push(`--${boundary}--`);
      h.set("Content-Type", `multipart/form-data; boundary="${boundary}"`);
      body = new Blob(blobParts);
    }
    finalContent = await body.arrayBuffer();
    const contentDigest = await sha256(finalContent);
    const base64 = base64url2.toBase64(base64url2.encode(contentDigest));
    h.append("Content-Digest", `sha-256=:${base64}:`);
  }
  return { headers: h, body };
}

// src/logger.js
import debug from "debug";
import { tap } from "ramda";
var createLogger = (name = "@permaweb/aoconnect") => {
  const logger = debug(name);
  logger.child = (name2) => createLogger(`${logger.namespace}:${name2}`);
  logger.tap = (note, ...rest) => tap((...args) => logger(note, ...rest, ...args));
  return logger;
};
var verboseLog = (...args) => {
  if (process.env.DEBUG) {
    console.log(...args);
  }
};

// src/client/hb.js
var reqFormatCache = {};
function toSigBaseArgs({ url, method, headers, includePath = false }) {
  headers = new Headers(headers);
  return {
    /**
     * Always sign all headers, and the path,
     * and that there is a deterministic signature
     * component ordering
     *
     * TODO: removing path from signing, for now.
     */
    fields: [
      ...headers.keys(),
      ...includePath ? ["@path"] : []
    ].sort(),
    request: { url, method, headers: { ...Object.fromEntries(headers) } }
  };
}
function httpSigName(address) {
  const decoded = base64url3.toBuffer(address);
  const hexString = [...decoded.subarray(1, 9)].map((byte) => byte.toString(16).padStart(2, "0")).join("");
  return `http-sig-${hexString}`;
}
function requestWith(args) {
  const { fetch: fetch2, logger: _logger, HB_URL, signer } = args;
  let signingFormat = args["signing-format"] || args.signingFormat;
  const logger = _logger.child("request");
  return async function(fields) {
    const { path: path2, method, ...restFields } = fields;
    signingFormat = fields["signing-format"] || fields.signingFormat;
    if (!signingFormat) {
      signingFormat = reqFormatCache[fields.path] ?? "HTTP";
    }
    try {
      let fetch_req = {};
      verboseLog("SIGNING FORMAT: ", signingFormat, ". REQUEST: ", fields);
      if (signingFormat === "ANS-104") {
        const ans104Request = toANS104Request(restFields);
        verboseLog("ANS-104 REQUEST PRE-SIGNING: ", JSON.stringify(ans104Request, null, 2));
        const signedRequest = await toDataItemSigner(signer)(ans104Request.item);
        verboseLog("SIGNED ANS-104 ITEM: ", signedRequest);
        fetch_req = {
          body: signedRequest.raw,
          url: joinUrl({ url: HB_URL, path: path2 }),
          path: path2,
          method,
          headers: ans104Request.headers
        };
      } else {
        const req = await encode(restFields);
        const signingArgs = toSigBaseArgs({
          url: joinUrl({ url: HB_URL, path: path2 }),
          method,
          headers: req.headers
        });
        const signedRequest = await toHttpSigner(signer)(signingArgs);
        fetch_req = { ...signedRequest, body: req.body, path: path2, method };
      }
      verboseLog("Sending signed message to HB: %o");
      const res = await fetch2(fetch_req.url, {
        method: fetch_req.method,
        headers: fetch_req.headers,
        body: fetch_req.body,
        redirect: "follow"
      });
      verboseLog("PUSH FORMAT: ", signingFormat, ". RESPONSE:", res);
      if (res.status === 422 && signingFormat === "HTTP") {
        reqFormatCache[fields.path] = "ANS-104";
        return requestWith({ ...args, signingFormat: "ANS-104" })(fields);
      }
      if (res.status == 500) {
        verboseLog("ERROR RESPONSE: ", res);
        throw new Error(`${res.status}: ${await res.text()}`);
      }
      if (res.status === 404) {
        verboseLog("ERROR RESPONSE: ", res);
        throw new Error(`${res.status}: ${await res.text()}`);
      }
      if (res.status >= 400) {
        logger.tap("ERROR RESPONSE: ", res);
        throw new Error(`${res.status}: ${await res.text()}`);
      }
      if (res.status >= 300) {
        return res;
      }
      let body = await res.text();
      return {
        headers: res.headers,
        body
      };
    } catch (error) {
      verboseLog("ERROR RESPONSE: ", error);
      throw error;
    }
  };
}
function toANS104Request(fields) {
  verboseLog("TO ANS 104 REQUEST: ", fields);
  const dataItem = {
    target: fields.target,
    anchor: fields.anchor ?? "",
    tags: keys(
      omit(
        [
          "Target",
          "target",
          "Anchor",
          "anchor",
          "Data",
          "data",
          "data-protocol",
          "Data-Protocol",
          "variant",
          "Variant",
          "dryrun",
          "Dryrun",
          "Type",
          "type",
          "path",
          "method",
          "signingFormat",
          "signing-format"
        ],
        fields
      )
    ).map(function(key) {
      return { name: key, value: fields[key] };
    }, fields).concat([
      { name: "data-protocol", value: "ao" },
      { name: "type", value: fields.type ?? "Message" },
      { name: "variant", value: fields.variant ?? "ao.N.1" }
    ]),
    data: fields?.data || ""
  };
  verboseLog("ANS104 REQUEST: ", JSON.stringify(dataItem, null, 2));
  return { headers: {
    "Content-Type": "application/ans104",
    "codec-device": "ans104@1.0",
    "accept-bundle": "true"
  }, item: dataItem };
}

// src/client/signer.js
var { augmentHeaders, createSignatureBase, createSigningParameters, formatSignatureBase } = httpbis;
if (!globalThis.Buffer) globalThis.Buffer = BufferShim3;
var toView = (value) => {
  if (ArrayBuffer.isView(value)) value = Buffer.from(value.buffer, value.byteOffset, value.byteLength);
  else if (typeof value === "string") value = base64url4.toBuffer(value);
  else throw new Error("Unexpected type. Value must be one of Uint8Array, ArrayBuffer, or base64url-encoded string");
  return value;
};
var DATAITEM_SIGNER_KIND = "ans104";
var HTTP_SIGNER_KIND = "httpsig";
var toDataItemSigner = (signer) => {
  return async ({ data, tags, target, anchor }) => {
    let resolveUnsigned;
    let createCalled;
    const dataToSign = new Promise((resolve) => {
      resolveUnsigned = resolve;
    });
    const create = async (injected) => {
      createCalled = true;
      if (injected.passthrough) return { data, tags, target, anchor };
      const { publicKey, type = 1, alg = "rsa-v1_5-sha256" } = injected;
      const unsigned = createDataItemBytes(
        data,
        { type, publicKey: toView(publicKey) },
        { target, tags, anchor }
      );
      resolveUnsigned(unsigned);
      const deepHash = await getSignatureData(unsigned);
      return deepHash;
    };
    return signer(create, DATAITEM_SIGNER_KIND).then((res) => {
      if (!createCalled) {
        throw new Error("create() must be invoked in order to construct the data to sign");
      }
      if (typeof res === "object" && res.id && res.raw) return res;
      if (!res.signature || !res.signature) {
        throw new Error("signer must return its signature and address");
      }
      const { signature } = res;
      return dataToSign.then((unsigned) => {
        return Promise.resolve(signature).then(toView).then(async (rawSig) => {
          const signedBytes = unsigned;
          signedBytes.set(rawSig, 2);
          const isValid = await verify(signedBytes);
          if (!isValid) throw new Error("Data Item signature is not valid");
          return {
            /**
             * A data item's ID is the base64url encoded
             * SHA-256 of the signature
             */
            id: await crypto.subtle.digest("SHA-256", rawSig).then((raw) => base64url4.encode(raw)),
            raw: signedBytes
          };
        });
      });
    });
  };
};
var toHttpSigner = (signer) => {
  const params = ["alg", "keyid"].sort();
  return async ({ request, fields }) => {
    let resolveUnsigned;
    let createCalled;
    const httpSig = {};
    const dataToSign = new Promise((resolve) => {
      resolveUnsigned = resolve;
    }).then();
    const create = (injected) => {
      createCalled = true;
      let { publicKey, type = 1, alg = "rsa-pss-sha512" } = injected;
      publicKey = toView(publicKey);
      const signingParameters = createSigningParameters({
        params,
        paramValues: {
          keyid: base64url4.encode(publicKey),
          alg
        }
      });
      const signatureBase = createSignatureBase({ fields }, request);
      const signatureInput = serializeList([
        [signatureBase.map(([item]) => parseItem(item)), signingParameters]
      ]);
      signatureBase.push(['"@signature-params"', [signatureInput]]);
      const base = formatSignatureBase(signatureBase);
      httpSig.signatureInput = signatureInput;
      httpSig.signatureBase = base;
      const encoded = new TextEncoder().encode(base);
      resolveUnsigned(encoded);
      return encoded;
    };
    return signer(create, HTTP_SIGNER_KIND).then((res) => {
      if (!res.signature || !res.signature) {
        throw new Error("signer must return its signature and address");
      }
      const { signature, address } = res;
      if (!createCalled) {
        throw new Error("create() must be invoked in order to construct the data to sign");
      }
      return dataToSign.then(() => {
        return Promise.resolve(signature).then(toView).then((rawSig) => {
          const withSignature = augmentHeaders(
            request.headers,
            rawSig,
            httpSig.signatureInput,
            httpSigName(address)
          );
          const signedRequest = { ...request, headers: withSignature };
          return signedRequest;
        });
      });
    });
  };
};

// src/client/ao-mu.js
function signDataItemChain(args, logger) {
  return of(args).chain(fromPromise(
    ({ processId, data, tags, anchor, signer }) => toDataItemSigner(signer)({ data, tags, target: processId, anchor })
  )).map(logger.tap("Successfully built and signed data item"));
}
function sendDataItemChain(signedDataItem, { fetch: fetch2, MU_URL: MU_URL2, logger, verifyBeforeSend = false }) {
  let chain2 = of(signedDataItem);
  if (verifyBeforeSend) {
    chain2 = chain2.chain(fromPromise(async (item) => {
      const verified = await verify(item.raw);
      if (!verified) throw new Error("Signed data item verification failed.");
      return item;
    }));
  }
  return chain2.chain(fromPromise(
    async (item) => fetch2(MU_URL2, {
      method: "POST",
      headers: {
        "Content-Type": "application/octet-stream",
        Accept: "application/json"
      },
      redirect: "follow",
      body: item.raw
    })
  )).bichain(
    (err) => {
      if (err.name === "RedirectRequested") {
        return Rejected(err);
      } else {
        return Rejected(new Error(`Error while communicating with MU: ${JSON.stringify(err)}`));
      }
    },
    fromPromise(async (res) => {
      if (res.ok) return res.json();
      throw new Error(`${res.status}: ${await res.text()}`);
    })
  ).bimap(
    logger.tap("Error encountered when writing message via MU"),
    logger.tap("Successfully wrote message via MU")
  ).map((res) => ({ res, messageId: signedDataItem.id }));
}
function deployMessageWith({ fetch: fetch2, MU_URL: MU_URL2, logger: _logger }) {
  const logger = _logger.child("deployMessage");
  return (args) => {
    return signDataItemChain(args, logger).chain((signedDataItem) => sendDataItemChain(signedDataItem, { fetch: fetch2, MU_URL: MU_URL2, logger })).toPromise();
  };
}
function signMessageWith({ logger: _logger }) {
  const logger = _logger.child("signMessage");
  return (args) => {
    return signDataItemChain(args, logger).toPromise();
  };
}
function sendSignedMessageWith({ fetch: fetch2, MU_URL: MU_URL2, logger: _logger }) {
  const logger = _logger.child("sendSignedMessage");
  return (signedDataItem) => {
    return sendDataItemChain(signedDataItem, { fetch: fetch2, MU_URL: MU_URL2, logger, verifyBeforeSend: true }).toPromise();
  };
}
function deployProcessWith({ fetch: fetch2, MU_URL: MU_URL2, logger: _logger }) {
  const logger = _logger.child("deployProcess");
  return (args) => {
    return of(args).chain(
      fromPromise(({ data, tags, signer }) => {
        return toDataItemSigner(signer)({ data, tags });
      })
    ).map(logger.tap("Successfully built and signed data item")).chain(
      (signedDataItem) => of(signedDataItem).chain(
        fromPromise(async (signedDataItem2) => {
          return fetch2(MU_URL2, {
            method: "POST",
            headers: {
              "Content-Type": "application/octet-stream",
              Accept: "application/json"
            },
            redirect: "follow",
            body: signedDataItem2.raw
          });
        })
      ).bichain(
        (err) => Rejected(
          new Error(
            `Error while communicating with MU: ${JSON.stringify(err)}`
          )
        ),
        fromPromise(async (res) => {
          if (res.ok) return res.json();
          throw new Error(`${res.status}: ${await res.text()}`);
        })
      ).bimap(
        logger.tap("Error encountered when deploying process via MU"),
        logger.tap("Successfully deployed process via MU")
      ).map((res) => ({ res, processId: res.processId || signedDataItem.id }))
    ).toPromise();
  };
}
function deployMonitorWith({ fetch: fetch2, MU_URL: MU_URL2, logger: _logger }) {
  const logger = _logger.child("deployMonitor");
  return (args) => of(args).chain(
    fromPromise(
      ({ processId, data, tags, anchor, signer }) => toDataItemSigner(signer)({ data, tags, target: processId, anchor })
    )
  ).map(logger.tap("Successfully built and signed data item")).chain(
    (signedDataItem) => of(signedDataItem).chain(
      fromPromise(
        async (signedDataItem2) => fetch2(MU_URL2 + "/monitor/" + args.processId, {
          method: "POST",
          headers: {
            "Content-Type": "application/octet-stream",
            Accept: "application/json"
          },
          redirect: "follow",
          body: signedDataItem2.raw
        })
      )
    ).bichain(
      (err) => Rejected(
        new Error(
          `Error while communicating with MU: ${JSON.stringify(err)}`
        )
      ),
      fromPromise(async (res) => {
        if (res.ok) return { ok: true };
        throw new Error(`${res.status}: ${await res.text()}`);
      })
    ).bimap(
      logger.tap("Error encountered when subscribing to process via MU"),
      logger.tap("Successfully subscribed to process via MU")
    ).map((res) => ({ res, messageId: signedDataItem.id }))
  ).toPromise();
}
function deployUnmonitorWith({ fetch: fetch2, MU_URL: MU_URL2, logger: _logger }) {
  const logger = _logger.child("deployUnmonitor");
  return (args) => of(args).chain(
    fromPromise(
      ({ processId, data, tags, anchor, signer }) => toDataItemSigner(signer)({ data, tags, target: processId, anchor })
    )
  ).map(logger.tap("Successfully built and signed data item")).chain(
    (signedDataItem) => of(signedDataItem).chain(
      fromPromise(
        async (signedDataItem2) => fetch2(MU_URL2 + "/monitor/" + args.processId, {
          method: "DELETE",
          headers: {
            "Content-Type": "application/octet-stream",
            Accept: "application/json"
          },
          redirect: "follow",
          body: signedDataItem2.raw
        })
      )
    ).bichain(
      (err) => Rejected(
        new Error(
          `Error while communicating with MU: ${JSON.stringify(err)}`
        )
      ),
      fromPromise(async (res) => {
        if (res.ok) return { ok: true };
        throw new Error(`${res.status}: ${await res.text()}`);
      })
    ).bimap(
      logger.tap(
        "Error encountered when unsubscribing to process via MU"
      ),
      logger.tap("Successfully unsubscribed to process via MU")
    ).map((res) => ({ res, messageId: signedDataItem.id }))
  ).toPromise();
}
function deployAssignWith({ fetch: fetch2, MU_URL: MU_URL2, logger: _logger }) {
  const logger = _logger.child("deployAssign");
  return (args) => {
    return of(args).chain(
      fromPromise(
        async ({ process: process2, message: message2, baseLayer, exclude }) => fetch2(
          `${MU_URL2}?process-id=${process2}&assign=${message2}${baseLayer ? "&base-layer" : ""}${exclude ? "&exclude=" + exclude.join(",") : ""}`,
          {
            method: "POST",
            headers: {
              "Content-Type": "application/octet-stream",
              Accept: "application/json"
            }
          }
        )
      )
    ).bichain(
      (err) => Rejected(
        new Error(
          `Error while communicating with MU: ${JSON.stringify(err)}`
        )
      ),
      fromPromise(async (res) => {
        if (res.ok) return res.json();
        throw new Error(`${res.status}: ${await res.text()}`);
      })
    ).bimap(
      logger.tap("Error encountered when writing assignment via MU"),
      logger.tap("Successfully wrote assignment via MU")
    ).map((res) => ({ res, assignmentId: res.id })).toPromise();
  };
}

// src/client/ao-cu.js
function dryrunFetchWith({ fetch: fetch2, CU_URL: CU_URL2, logger }) {
  return (msg) => of(msg).map(logger.tap("posting dryrun request to CU")).chain(fromPromise((msg2) => fetch2(`${CU_URL2}/dry-run?process-id=${msg2.Target}`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    redirect: "follow",
    body: JSON.stringify(msg2)
  }).then((res) => res.json()))).toPromise();
}
function loadResultWith({ fetch: fetch2, CU_URL: CU_URL2, logger }) {
  return ({ id, processId }) => {
    return of(`${CU_URL2}/result/${id}?process-id=${processId}`).map(logger.tap("fetching message result from CU")).chain(fromPromise(
      async (url) => fetch2(url, {
        method: "GET",
        headers: {
          Accept: "application/json"
        },
        redirect: "follow"
      }).then((res) => res.json())
    )).toPromise();
  };
}
function queryResultsWith({ fetch: fetch2, CU_URL: CU_URL2, logger }) {
  return ({ process: process2, from, to, sort, limit }) => {
    const target = new URL(`${CU_URL2}/results/${process2}`);
    const params = new URLSearchParams(target.search);
    if (from) {
      params.append("from", from);
    }
    if (to) {
      params.append("to", to);
    }
    if (sort) {
      params.append("sort", sort);
    }
    if (limit) {
      params.append("limit", limit);
    }
    target.search = params;
    return of(target.toString()).map(logger.tap("fetching message result from CU")).chain(fromPromise(
      async (url) => fetch2(url, {
        method: "GET",
        headers: {
          Accept: "application/json"
        },
        redirect: "follow"
      }).then((res) => res.json())
    )).toPromise();
  };
}

// src/client/gateway.js
import { path } from "ramda";
import { z } from "zod";
function loadTransactionMetaWith({ fetch: fetch2, GRAPHQL_URL: GRAPHQL_URL2, logger }) {
  const GET_TRANSACTIONS_QUERY = `
    query GetTransactions ($transactionIds: [ID!]!) {
      transactions(ids: $transactionIds) {
        edges {
          node {
            owner {
              address
            }
            tags {
              name
              value
            }
            block {
              id
              height
              timestamp
            }
          }
        }
      }
    }`;
  const transactionConnectionSchema = z.object({
    data: z.object({
      transactions: z.object({
        edges: z.array(z.object({
          node: z.record(z.any())
        }))
      })
    })
  });
  return (id) => of(id).chain(fromPromise(
    (id2) => fetch2(GRAPHQL_URL2, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        query: GET_TRANSACTIONS_QUERY,
        variables: { transactionIds: [id2] }
      })
    }).then(async (res) => {
      if (res.ok) return res.json();
      logger('Error Encountered when querying gateway for transaction "%s"', id2);
      throw new Error(`${res.status}: ${await res.text()}`);
    }).then(transactionConnectionSchema.parse).then(path(["data", "transactions", "edges", "0", "node"]))
  )).toPromise();
}

// src/client/ao-su.js
import LruMap from "mnemonist/lru-map.js";
var getMessagesByRange = ({ fetch: fetch2, locate: locate2 }) => {
  return async ({ processId, from, to, limit }) => {
    const suUrl = (await locate2(processId)).url;
    return fetch2(`${suUrl}/${processId}?from=${from}&to=${to}&limit=${limit}`).then((res) => res.json());
  };
};
var getMessageById = ({ fetch: fetch2, locate: locate2 }) => {
  return async ({ processId, messageId }) => {
    const suUrl = (await locate2(processId)).url;
    return fetch2(`${suUrl}/${messageId}?process-id=${processId}`).then((res) => res.json());
  };
};
var getLastSlotWith = ({ fetch: fetch2, locate: locate2 }) => {
  return async ({ processId, since }) => {
    if (!since) {
      since = 10 * 60 * 1e3;
    }
    const from = Date.now() - since;
    const suUrl = (await locate2(processId)).url;
    return fetch2(`${suUrl}/${processId}?process-id=${processId}&limit=100&from=${from}`).then((res) => res.json()).then((suResult) => {
      return suResult.edges[suResult?.edges?.length - 1]?.node?.assignment?.tags?.find((t) => t.name === "Nonce")?.value;
    });
  };
};

// src/lib/messages/index.js
import { z as z2 } from "zod";
var inputSchema = z2.object({
  processId: z2.string(),
  from: z2.string(),
  to: z2.string(),
  limit: z2.string().default("1000").optional()
}).passthrough();
function messagesWith(env) {
  const messages = env.messages;
  return (fields) => {
    fields = inputSchema.parse(fields);
    return messages(fields);
  };
}

// src/lib/message-id/index.js
import { z as z3 } from "zod";
var inputSchema2 = z3.object({
  processId: z3.string(),
  messageId: z3.string()
}).passthrough();
function messageIdWith(env) {
  const getMessageId = env.getMessageId;
  return (fields) => {
    fields = inputSchema2.parse(fields);
    return getMessageId(fields);
  };
}

// src/lib/process-id/index.js
import { z as z4 } from "zod";
var inputSchema3 = z4.object({
  processId: z4.string(),
  since: z4.number().optional()
}).passthrough();
function processIdWith(env) {
  const processId = env.processId;
  return (fields) => {
    fields = inputSchema3.parse(fields);
    return processId(fields);
  };
}

// src/lib/request/index.js
import { identity, mergeRight } from "ramda";
import { z as z5 } from "zod";

// src/lib/request/multipart.js
function parseMultipartContent(content, contentType) {
  const boundaryMatch = contentType.match(/boundary="?([^";]+)"?/);
  if (!boundaryMatch) {
    throw new Error("No boundary found in Content-Type");
  }
  const boundary = boundaryMatch[1];
  const boundaryRegex = new RegExp(`--${boundary}(?:--)?(\\r\\n|\\n)`);
  const parts = content.split(boundaryRegex).filter((p) => p !== "\r\n");
  const resultMap = /* @__PURE__ */ new Map();
  for (let i = 0; i < parts.length; i += 2) {
    if (!parts[i] || parts[i].trim() === "") continue;
    const partContent = parts[i];
    const lines = partContent.split(/\r\n|\n/);
    const headers = {};
    let j = 0;
    while (j < lines.length && lines[j].trim() !== "") {
      const line = lines[j];
      const colonIndex = line.indexOf(":");
      if (colonIndex !== -1) {
        const headerName = line.substring(0, colonIndex).trim().toLowerCase();
        const headerValue = line.substring(colonIndex + 1).trim();
        headers[headerName] = headerValue;
      }
      j++;
    }
    j++;
    const bodyContent = lines.slice(j).join("\n").trim();
    const contentDisposition = headers["content-disposition"] || "";
    const nameMatch = contentDisposition.match(/name="([^"]+)"/);
    if (nameMatch) {
      const name = nameMatch[1];
      const entry = Object.assign({}, headers, { body: bodyContent });
      const fields = {};
      lines.slice(j).forEach((line) => {
        if (line.trim() === "") return;
        const colonIndex = line.indexOf(":");
        if (colonIndex !== -1) {
          const fieldName = line.substring(0, colonIndex).trim();
          const fieldValue = line.substring(colonIndex + 1).trim();
          fields[fieldName] = fieldValue;
        }
      });
      if (Object.keys(fields).length > 0) {
        entry.fields = fields;
      }
      resultMap.set(name, entry);
    }
  }
  return resultMap;
}

// src/lib/request/index.js
function requestWith2(env) {
  return (fields) => {
    return of(fields).chain(verifyInput).chain(dispatch(env)).map(logResult(env, fields)).map(transformToMap).bimap(errFrom, identity).toPromise();
  };
}
function verifyInput(args) {
  const inputSchema7 = z5.object({
    path: z5.string().min(1, { message: "path is required" }),
    method: z5.string()
  }).passthrough();
  return of(inputSchema7.parse(args));
}
function transformToMap(result2) {
  let map3 = {};
  const res = result2;
  if (res.headers.get("content-type") && res.headers.get("content-type").startsWith("multipart")) {
    map3 = mergeRight(
      map3,
      Object.fromEntries(
        parseMultipartContent(res.body, res.headers.get("content-type"))
      )
    );
  } else {
    map3.body = res.body;
  }
  res.headers.forEach((v, k) => {
    map3[k] = v;
  });
  return map3;
}
function dispatch(env) {
  return fromPromise(env.request);
}
function logResult(env, fields) {
  return (res) => {
    env.logger(
      'Received response from message sent to path "%s" with res %O',
      fields?.path ?? "/",
      res
    );
    return res;
  };
}

// src/lib/result/index.js
import { identity as identity2 } from "ramda";

// src/lib/result/verify-input.js
import { z as z6 } from "zod";
var inputSchema4 = z6.object({
  id: z6.string().min(1, { message: "message is required to be a message id" }),
  processId: z6.string().min(1, { message: "process is required to be a process id" })
});
function verifyInputWith() {
  return (ctx) => {
    return of(ctx).map(inputSchema4.parse).map(() => ctx);
  };
}

// src/dal.js
import { z as z7 } from "zod";
var tagSchema = z7.object({
  name: z7.string(),
  value: z7.any()
});
var dryrunResultSchema = z7.function().args(z7.object({
  Id: z7.string(),
  Target: z7.string(),
  Owner: z7.string(),
  Anchor: z7.string().optional(),
  Data: z7.any().default("1234"),
  Tags: z7.array(z7.object({ name: z7.string(), value: z7.string() }))
})).returns(z7.promise(z7.any()));
var loadResultSchema = z7.function().args(z7.object({
  id: z7.string().min(1, { message: "message id is required" }),
  processId: z7.string().min(1, { message: "process id is required" })
})).returns(z7.promise(z7.any()));
var queryResultsSchema = z7.function().args(z7.object({
  process: z7.string().min(1, { message: "process id is required" }),
  from: z7.string().optional(),
  to: z7.string().optional(),
  sort: z7.enum(["ASC", "DESC"]).default("ASC"),
  limit: z7.number().optional()
})).returns(z7.promise(z7.object({
  edges: z7.array(z7.object({
    cursor: z7.string(),
    node: z7.object({
      Output: z7.any().optional(),
      Messages: z7.array(z7.any()).optional(),
      Spawns: z7.array(z7.any()).optional(),
      Error: z7.any().optional()
    })
  }))
})));
var deployMessageSchema = z7.function().args(z7.object({
  processId: z7.string(),
  data: z7.any(),
  tags: z7.array(tagSchema),
  anchor: z7.string().optional(),
  signer: z7.any().nullish()
})).returns(z7.promise(
  z7.object({
    messageId: z7.string()
  }).passthrough()
));
var prepareMessageSchema = z7.function().args(z7.object({
  processId: z7.string(),
  data: z7.any(),
  tags: z7.array(tagSchema),
  anchor: z7.string().optional(),
  signer: z7.any().nullish()
})).returns(z7.promise(
  z7.object({
    id: z7.string(),
    raw: z7.any()
  }).passthrough()
));
var sendSignedMessageSchema = z7.function().args(z7.object({
  id: z7.string(),
  raw: z7.any()
})).returns(z7.promise(
  z7.object({
    messageId: z7.string()
  }).passthrough()
));
var deployProcessSchema = z7.function().args(z7.object({
  data: z7.any(),
  tags: z7.array(tagSchema),
  signer: z7.any().nullish()
})).returns(z7.promise(
  z7.object({
    processId: z7.string()
  }).passthrough()
));
var deployAssignSchema = z7.function().args(z7.object({
  process: z7.string(),
  message: z7.string(),
  baseLayer: z7.boolean().optional(),
  exclude: z7.array(z7.string()).optional()
})).returns(z7.promise(
  z7.object({
    assignmentId: z7.string()
  }).passthrough()
));
var deployMonitorSchema = deployMessageSchema;
var loadProcessMetaSchema = z7.function().args(z7.object({
  suUrl: z7.string().url(),
  processId: z7.string()
})).returns(z7.promise(
  z7.object({
    tags: z7.array(tagSchema)
  }).passthrough()
));
var locateSchedulerSchema = z7.function().args(z7.string()).returns(z7.promise(
  z7.object({
    url: z7.string()
  })
));
var validateSchedulerSchema = z7.function().args(z7.string()).returns(z7.promise(z7.boolean()));
var loadTransactionMetaSchema = z7.function().args(z7.string()).returns(z7.promise(
  z7.object({
    tags: z7.array(tagSchema)
  }).passthrough()
));
var signerSchema = z7.function();

// src/lib/result/read.js
function readWith({ loadResult }) {
  loadResult = fromPromise(loadResultSchema.implement(loadResult));
  return (ctx) => {
    return of({ id: ctx.id, processId: ctx.processId }).chain(loadResult);
  };
}

// src/lib/result/index.js
function resultWith(env) {
  const verifyInput2 = verifyInputWith(env);
  const read = readWith(env);
  return ({ message: message2, process: process2 }) => {
    return of({ id: message2, processId: process2 }).chain(verifyInput2).chain(read).map(
      env.logger.tap(
        'readResult result for message "%s": %O',
        message2
      )
    ).map((result2) => result2).bimap(errFrom, identity2).toPromise();
  };
}

// src/lib/message/index.js
import { identity as identity3 } from "ramda";

// src/lib/message/upload-message.js
import { z as z8 } from "zod";
import { __ as __2, always, assoc as assoc2, curry, defaultTo as defaultTo2, ifElse, pipe as pipe2, prop } from "ramda";
import { proto } from "@permaweb/protocol-tag-utils";
var aoProto = proto("ao");
var removeAoProtoByName = curry(aoProto.removeAllByName);
var concatAoProto = curry(aoProto.concat);
var concatUnassoc = curry(aoProto.concatUnassoc);
var tagSchema2 = z8.array(z8.object({
  name: z8.string(),
  value: z8.string()
}));
function buildTagsWith() {
  return (ctx) => {
    const variant = ctx?.tags?.find((tag) => tag.name.toLowerCase() === "variant")?.value || "ao.TN.1";
    return of(ctx.tags).map(defaultTo2([])).map(removeAoProtoByName("Variant")).map(removeAoProtoByName("Type")).map(concatAoProto([
      { name: "Variant", value: variant },
      { name: "Type", value: "Message" }
    ])).map(tagSchema2.parse).map(assoc2("tags", __2, ctx));
  };
}
function buildDataWith({ logger }) {
  return (ctx) => {
    return of(ctx).chain(ifElse(
      always(ctx.data),
      /**
       * data is provided as input, so do nothing
       */
      () => Resolved(ctx),
      /**
       * No data is provided, so replace with one space
       */
      () => Resolved(" ").map(assoc2("data", __2, ctx)).map(
        (ctx2) => pipe2(
          prop("tags"),
          concatUnassoc([{ name: "Content-Type", value: "text/plain" }]),
          assoc2("tags", __2, ctx2)
        )(ctx2)
      ).map(logger.tap('added pseudo-random string as message "data"'))
    )).map(
      (ctx2) => pipe2(
        prop("tags"),
        concatUnassoc([{ name: "SDK", value: "aoconnect" }]),
        assoc2("tags", __2, ctx2)
      )(ctx2)
    );
  };
}
function uploadMessageWith(env) {
  const buildTags = buildTagsWith(env);
  const buildData = buildDataWith(env);
  const deployMessage = deployMessageSchema.implement(env.deployMessage);
  return (ctx) => {
    return of(ctx).chain(buildTags).chain(buildData).chain(fromPromise(({ id, data, tags, anchor, signer }) => {
      return deployMessage({ processId: id, data, tags, anchor, signer: signerSchema.implement(signer || env.signer) });
    })).map((res) => assoc2("messageId", res.messageId, ctx));
  };
}
function prepareMessageWith(env) {
  const buildTags = buildTagsWith(env);
  const buildData = buildDataWith(env);
  const signMessage = prepareMessageSchema.implement(env.signMessage);
  return (ctx) => {
    return of(ctx).chain(buildTags).chain(buildData).chain(fromPromise(
      ({ id, data, tags, anchor, signer }) => signMessage({ processId: id, data, tags, anchor, signer: signerSchema.implement(signer || env.signer) })
    )).map((res) => {
      return res;
    });
  };
}
function sendSignedMessageWith2(env) {
  const sendSignedMessage = sendSignedMessageSchema.implement(env.sendSignedMessage);
  return (ctx) => {
    return of(ctx).chain(fromPromise(
      ({ id, raw }) => sendSignedMessage({ id, raw })
    )).map((res) => assoc2("messageId", res.messageId, ctx));
  };
}

// src/lib/message/index.js
function messageWith(env) {
  const uploadMessage = uploadMessageWith(env);
  return ({ process: process2, data, tags, anchor, signer }) => {
    return of({ id: process2, data, tags, anchor, signer }).chain(uploadMessage).map((ctx) => ctx.messageId).bimap(errFrom, identity3).toPromise();
  };
}
function prepareWith(env) {
  const prepareMessage = prepareMessageWith(env);
  return ({ process: process2, data, tags, anchor, signer }) => {
    return of({ id: process2, data, tags, anchor, signer }).chain(prepareMessage).map((ctx) => ctx).bimap(errFrom, identity3).toPromise();
  };
}
function signedMessageWith(env) {
  const sendSignedMessage = sendSignedMessageWith2(env);
  return ({ id, raw }) => {
    return of({ id, raw }).chain(sendSignedMessage).map((ctx) => ctx.messageId).bimap(errFrom, identity3).toPromise();
  };
}

// src/lib/spawn/index.js
import { identity as identity4 } from "ramda";

// src/lib/spawn/verify-inputs.js
import { isNotNil, prop as prop2 } from "ramda";
var checkTag = (name, pred, err) => (tags) => pred(tags[name]) ? Resolved(tags) : Rejected(`Tag '${name}': ${err}`);
function verifyModuleWith({ loadTransactionMeta, logger }) {
  loadTransactionMeta = fromPromise(loadTransactionMetaSchema.implement(loadTransactionMeta));
  return (module) => of(module).chain(loadTransactionMeta).map(prop2("tags")).map(parseTags).chain(checkTag("Data-Protocol", eqOrIncludes("ao"), "value 'ao' was not found on module")).chain(checkTag("Type", eqOrIncludes("Module"), "value 'Module' was not found on module")).chain(checkTag("Module-Format", isNotNil, "was not found on module")).chain(checkTag("Input-Encoding", isNotNil, "was not found on module")).chain(checkTag("Output-Encoding", isNotNil, "was not found on module")).bimap(
    logger.tap("Verifying module source failed: %s"),
    logger.tap("Verified module source")
  );
}
function verifySchedulerWith({ logger, validateScheduler }) {
  validateScheduler = fromPromise(validateSchedulerSchema.implement(validateScheduler));
  return (scheduler) => of(scheduler).chain(
    (scheduler2) => validateScheduler(scheduler2).chain((isValid) => isValid ? Resolved(scheduler2) : Rejected(`Valid Scheduler-Location owned by ${scheduler2} not found`))
  ).bimap(
    logger.tap("Verifying scheduler failed: %s"),
    logger.tap("Verified scheduler")
  );
}
function verifySignerWith({ logger }) {
  return (signer) => of(signer).map(logger.tap("Checking for signer")).chain((signer2) => signer2 ? Resolved(signer2) : Rejected("signer not found"));
}
function verifyInputsWith(env) {
  const logger = env.logger.child("verifyInput");
  env = { ...env, logger };
  const verifyModule = verifyModuleWith(env);
  const verifyScheduler = verifySchedulerWith(env);
  const verifySigner = verifySignerWith(env);
  return (ctx) => {
    return of(ctx).chain((ctx2) => verifyModule(ctx2.module).map(() => ctx2)).bimap(
      logger.tap("Error when verify input: %s"),
      logger.tap("Successfully verified inputs")
    );
  };
}

// src/lib/spawn/upload-process.js
import { z as z9 } from "zod";
import { __ as __3, always as always2, assoc as assoc3, curry as curry2, defaultTo as defaultTo3, ifElse as ifElse2, pipe as pipe3, prop as prop3 } from "ramda";
import { proto as proto2 } from "@permaweb/protocol-tag-utils";
var aoProto2 = proto2("ao");
var removeAoProtoByName2 = curry2(aoProto2.removeAllByName);
var concatAoProto2 = curry2(aoProto2.concat);
var concatUnassoc2 = curry2(aoProto2.concatUnassoc);
var tagsSchema = z9.array(tagSchema);
function buildTagsWith2() {
  return (ctx) => {
    const variant = ctx?.tags?.find((tag) => tag.name.toLowerCase() === "variant")?.value || "ao.TN.1";
    return of(ctx).map(prop3("tags")).map(defaultTo3([])).map(removeAoProtoByName2("Variant")).map(removeAoProtoByName2("Type")).map(removeAoProtoByName2("Module")).map(removeAoProtoByName2("Scheduler")).map(concatAoProto2([
      { name: "Variant", value: variant },
      { name: "Type", value: "Process" },
      { name: "Module", value: ctx.module },
      { name: "Scheduler", value: ctx.scheduler },
      { name: "Timestamp", value: Date.now().toString() }
    ])).map(tagsSchema.parse).map(assoc3("tags", __3, ctx));
  };
}
function buildDataWith2({ logger }) {
  return (ctx) => {
    return of(ctx).chain(ifElse2(
      always2(ctx.data),
      /**
       * data is provided as input, so do nothing
       */
      () => Resolved(ctx),
      /**
       * No data is provided, so replace with one space
       */
      () => Resolved(" ").map(assoc3("data", __3, ctx)).map(
        (ctx2) => pipe3(
          prop3("tags"),
          concatUnassoc2([{ name: "Content-Type", value: "text/plain" }]),
          assoc3("tags", __3, ctx2)
        )(ctx2)
      ).map(logger.tap('added pseudo-random string as process "data"'))
    )).map(
      (ctx2) => pipe3(
        prop3("tags"),
        concatUnassoc2([{ name: "SDK", value: "aoconnect" }]),
        assoc3("tags", __3, ctx2)
      )(ctx2)
    );
  };
}
function uploadProcessWith(env) {
  const logger = env.logger.child("uploadProcess");
  env = { ...env, logger };
  const buildTags = buildTagsWith2(env);
  const buildData = buildDataWith2(env);
  const deployProcess = deployProcessSchema.implement(env.deployProcess);
  return (ctx) => {
    return of(ctx).chain(buildTags).chain(buildData).chain(fromPromise(
      ({ data, tags, signer }) => deployProcess({ data, tags, signer: signerSchema.implement(signer || env.signer) })
    )).map((res) => assoc3("processId", res.processId, ctx));
  };
}

// src/lib/spawn/index.js
function spawnWith(env) {
  const verifyInputs = verifyInputsWith(env);
  const uploadProcess = uploadProcessWith(env);
  return ({ module, scheduler, signer, tags, data }) => {
    return of({ module, scheduler, signer, tags, data }).chain(verifyInputs).chain(uploadProcess).map((ctx) => ctx.processId).bimap(errFrom, identity4).toPromise();
  };
}

// src/lib/monitor/index.js
import { identity as identity5 } from "ramda";

// src/lib/monitor/upload-monitor.js
import { assoc as assoc4 } from "ramda";
function uploadMonitorWith(env) {
  const deployMonitor = deployMonitorSchema.implement(env.deployMonitor);
  return (ctx) => {
    return of(ctx).chain(fromPromise(
      ({ id, signer }) => deployMonitor({
        processId: id,
        signer: signerSchema.implement(signer || env.signer),
        /**
         * No tags or data can be provided right now,
         *
         * so just set data to single space and set tags to an empty array
         */
        data: " ",
        tags: []
      })
    )).map((res) => assoc4("monitorId", res.messageId, ctx));
  };
}

// src/lib/monitor/index.js
function monitorWith(env) {
  const uploadMonitor = uploadMonitorWith(env);
  return ({ process: process2, signer }) => of({ id: process2, signer }).chain(uploadMonitor).map((ctx) => ctx.monitorId).bimap(errFrom, identity5).toPromise();
}

// src/lib/unmonitor/index.js
import { identity as identity6 } from "ramda";

// src/lib/unmonitor/upload-unmonitor.js
import { assoc as assoc5 } from "ramda";
function uploadUnmonitorWith(env) {
  const deployUnmonitor = deployMonitorSchema.implement(env.deployUnmonitor);
  return (ctx) => {
    return of(ctx).chain(fromPromise(
      ({ id, signer }) => deployUnmonitor({
        processId: id,
        signer: signerSchema.implement(signer || env.signer),
        /**
         * No tags or data can be provided right now,
         *
         * so just set data to single space and set tags to an empty array
         */
        data: " ",
        tags: []
      })
    )).map((res) => assoc5("monitorId", res.messageId, ctx));
  };
}

// src/lib/unmonitor/index.js
function unmonitorWith(env) {
  const uploadUnmonitor = uploadUnmonitorWith(env);
  return ({ process: process2, signer }) => of({ id: process2, signer }).chain(uploadUnmonitor).map((ctx) => ctx.monitorId).bimap(errFrom, identity6).toPromise();
}

// src/lib/results/index.js
import { identity as identity7 } from "ramda";

// src/lib/results/verify-input.js
import { z as z10 } from "zod";
var inputSchema5 = z10.object({
  process: z10.string().min(1, { message: "process identifier is required" }),
  from: z10.string().optional(),
  to: z10.string().optional(),
  sort: z10.enum(["ASC", "DESC"]).default("ASC"),
  limit: z10.number().optional()
});
function verifyInputWith2() {
  return (ctx) => {
    return of(ctx).map(inputSchema5.parse).map(() => ctx);
  };
}

// src/lib/results/query.js
function queryWith({ queryResults }) {
  queryResults = fromPromise(queryResultsSchema.implement(queryResults));
  return (ctx) => {
    return of({ process: ctx.process, from: ctx.from, to: ctx.to, sort: ctx.sort, limit: ctx.limit }).chain(queryResults);
  };
}

// src/lib/results/index.js
function resultsWith(env) {
  const verifyInput2 = verifyInputWith2(env);
  const query = queryWith(env);
  return ({ process: process2, from, to, sort, limit }) => {
    return of({ process: process2, from, to, sort, limit }).chain(verifyInput2).chain(query).map(
      env.logger.tap(
        'readResults result for message "%s": %O',
        process2
      )
    ).map((result2) => result2).bimap(errFrom, identity7).toPromise();
  };
}

// src/lib/dryrun/verify-input.js
import { z as z11 } from "zod";
var inputSchema6 = z11.object({
  Id: z11.string(),
  Target: z11.string(),
  Owner: z11.string(),
  Anchor: z11.string().optional(),
  Data: z11.any().default("1234"),
  Tags: z11.array(z11.object({ name: z11.string(), value: z11.string() }))
});
function verifyInputWith3() {
  return (msg) => {
    return of(msg).map(inputSchema6.parse).map((m) => {
      m.Tags = m.Tags.concat([
        { name: "Data-Protocol", value: "ao" },
        { name: "Type", value: "Message" },
        { name: "Variant", value: "ao.TN.1" }
      ]);
      return m;
    });
  };
}

// src/lib/dryrun/run.js
function runWith({ dryrunFetch }) {
  return fromPromise(dryrunResultSchema.implement(dryrunFetch));
}

// src/lib/dryrun/index.js
function dryrunWith(env) {
  const verifyInput2 = verifyInputWith3(env);
  const dryrun2 = runWith(env);
  return (msg) => of(msg).map(convert).chain(verifyInput2).chain(dryrun2).toPromise();
}
function convert({ process: process2, data, tags, anchor, ...rest }) {
  return {
    Id: "1234",
    Owner: "1234",
    ...rest,
    Target: process2,
    Data: data || "1234",
    Tags: tags || [],
    Anchor: anchor || "0"
  };
}

// src/lib/assign/index.js
import { identity as identity8 } from "ramda";

// src/lib/assign/send-assign.js
import { assoc as assoc6 } from "ramda";
function sendAssignWith(env) {
  const deployAssign = deployAssignSchema.implement(env.deployAssign);
  return (ctx) => {
    return of(ctx).chain(fromPromise(
      ({ process: process2, message: message2, baseLayer, exclude }) => deployAssign({ process: process2, message: message2, baseLayer, exclude })
    )).map((res) => assoc6("assignmentId", res.assignmentId, ctx));
  };
}

// src/lib/assign/index.js
function assignWith(env) {
  const sendAssign = sendAssignWith(env);
  return ({ process: process2, message: message2, baseLayer, exclude }) => {
    return of({ process: process2, message: message2, baseLayer, exclude }).chain(sendAssign).map((ctx) => ctx.assignmentId).bimap(errFrom, identity8).toPromise();
  };
}

// src/lib/serializeCron/index.js
import { map as map2 } from "ramda";
function serializeCron(cron) {
  function parseInterval(interval2 = "") {
    if (typeof interval2 !== "string") throw new Error("Encountered Error serializing cron: invalid interval");
    const [value, unit] = interval2.split("-").map((s) => s.trim());
    if (!value || !unit) throw new Error("Encountered Error serializing cron: invalid interval");
    if (!parseInt(value) || parseInt(value) < 0) throw new Error("Encountered Error serializing cron: invalid interval value");
    const singularRegex = /^(millisecond|second|minute|hour|day|month|year|block)$/;
    const pluralRegex = /^(milliseconds|seconds|minutes|hours|days|months|years|blocks)$/;
    const unitSingularMatch = unit.match(singularRegex);
    const unitPluralMatch = unit.match(pluralRegex);
    if (parseInt(value) > 1 && !unitPluralMatch || parseInt(value) === 1 && !unitSingularMatch) throw new Error("Encountered Error serializing cron: invalid interval type");
    return `${value}-${unit}`;
  }
  function parseTags2(tags2 = []) {
    return map2((tag) => {
      if (!tag.name || !tag.value) throw new Error("Encountered Error serializing cron: invalid tag structure");
      if (typeof tag.name !== "string" || typeof tag.value !== "string") throw new Error("Encountered Error serializing cron: invalid interval tag types");
      return { name: `Cron-Tag-${tag.name}`, value: tag.value };
    }, tags2);
  }
  const interval = parseInterval(cron.interval);
  const tags = parseTags2(cron.tags);
  return [{ name: "Cron-Interval", value: interval }, ...tags];
}

// src/index.common.js
var DEFAULT_GATEWAY_URL = "https://arweave.net";
var DEFAULT_MU_URL = "https://mu.ao-testnet.xyz";
var DEFAULT_CU_URL = "https://cu.ao-testnet.xyz";
var DEFAULT_RELAY_URL = "http://relay.ao-hb.xyz";
var DEFAULT_RELAY_CU_URL = "http://cu.s451-comm3-main.xyz";
var DEFAULT_RELAY_MU_URL = "http://mu.s451-comm3-main.xyz";
var DEFAULT_DEVICE = "relay@1.0";
var defaultFetch = fetch;
function connectWith({ createDataItemSigner: createDataItemSigner3, createSigner: createSigner3 }) {
  const _logger = createLogger();
  function legacyMode({
    MODE,
    GRAPHQL_URL: GRAPHQL_URL2,
    GRAPHQL_MAX_RETRIES: GRAPHQL_MAX_RETRIES2,
    GRAPHQL_RETRY_BACKOFF: GRAPHQL_RETRY_BACKOFF2,
    MU_URL: MU_URL2 = DEFAULT_MU_URL,
    CU_URL: CU_URL2 = DEFAULT_CU_URL,
    fetch: fetch2 = defaultFetch,
    noLog
  }) {
    const logger = _logger.child("legacy");
    if (!noLog) logger("Mode Activated \u2139\uFE0F");
    const { validate } = schedulerUtilsConnect({
      cacheSize: 100,
      GRAPHQL_URL: GRAPHQL_URL2,
      GRAPHQL_MAX_RETRIES: GRAPHQL_MAX_RETRIES2,
      GRAPHQL_RETRY_BACKOFF: GRAPHQL_RETRY_BACKOFF2
    });
    const resultLogger = logger.child("result");
    const result2 = resultWith({
      loadResult: loadResultWith({
        fetch: fetch2,
        CU_URL: CU_URL2,
        logger: resultLogger
      }),
      logger: resultLogger
    });
    const messageLogger = logger.child("message");
    const message2 = messageWith({
      deployMessage: deployMessageWith({
        fetch: fetch2,
        MU_URL: MU_URL2,
        logger: messageLogger
      }),
      logger: messageLogger
    });
    const signMessageLogger = logger.child("signMessage");
    const signMessage = prepareWith({
      signMessage: signMessageWith({
        logger: signMessageLogger
      }),
      logger: signMessageLogger
    });
    const sendSignedMessageLogger = logger.child("sendSignedMessage");
    const sendSignedMessage = signedMessageWith({
      sendSignedMessage: sendSignedMessageWith({
        fetch: fetch2,
        MU_URL: MU_URL2,
        logger: sendSignedMessageLogger
      }),
      logger: sendSignedMessageLogger
    });
    const spawnLogger = logger.child("spawn");
    const spawn2 = spawnWith({
      loadTransactionMeta: loadTransactionMetaWith({
        fetch: fetch2,
        GRAPHQL_URL: GRAPHQL_URL2,
        logger: spawnLogger
      }),
      validateScheduler: validate,
      deployProcess: deployProcessWith({
        fetch: fetch2,
        MU_URL: MU_URL2,
        logger: spawnLogger
      }),
      logger: spawnLogger
    });
    const monitorLogger = logger.child("monitor");
    const monitor2 = monitorWith({
      deployMonitor: deployMonitorWith({
        fetch: fetch2,
        MU_URL: MU_URL2,
        logger: monitorLogger
      }),
      logger: monitorLogger
    });
    const unmonitorLogger = logger.child("unmonitor");
    const unmonitor2 = unmonitorWith({
      deployUnmonitor: deployUnmonitorWith({
        fetch: fetch2,
        MU_URL: MU_URL2,
        logger: unmonitorLogger
      }),
      logger: monitorLogger
    });
    const resultsLogger = logger.child("results");
    const results2 = resultsWith({
      queryResults: queryResultsWith({
        fetch: fetch2,
        CU_URL: CU_URL2,
        logger: resultsLogger
      }),
      logger: resultsLogger
    });
    const dryrunLogger = logger.child("dryrun");
    const dryrun2 = dryrunWith({
      dryrunFetch: dryrunFetchWith({
        fetch: fetch2,
        CU_URL: CU_URL2,
        logger: dryrunLogger
      }),
      logger: dryrunLogger
    });
    const assignLogger = logger.child("assign");
    const assign2 = assignWith({
      deployAssign: deployAssignWith({
        fetch: fetch2,
        MU_URL: MU_URL2,
        logger: assignLogger
      }),
      logger: messageLogger
    });
    const getMessageById2 = messageIdWith({
      getMessageId: getMessageById({ fetch: fetch2, locate })
    });
    const getMessages = messagesWith({
      messages: getMessagesByRange({ fetch: fetch2, locate })
    });
    const getLastSlot = processIdWith({
      processId: getLastSlotWith({
        fetch: fetch2,
        locate
      })
    });
    return {
      MODE,
      result: result2,
      results: results2,
      message: message2,
      spawn: spawn2,
      monitor: monitor2,
      unmonitor: unmonitor2,
      dryrun: dryrun2,
      assign: assign2,
      createDataItemSigner: createDataItemSigner3,
      signMessage,
      sendSignedMessage,
      getMessages,
      getLastSlot,
      getMessageById: getMessageById2
    };
  }
  function mainnetMode({
    MODE,
    signer,
    GRAPHQL_URL: GRAPHQL_URL2,
    device = DEFAULT_DEVICE,
    URL: URL2 = DEFAULT_RELAY_URL,
    MU_URL: MU_URL2 = DEFAULT_RELAY_MU_URL,
    CU_URL: CU_URL2 = DEFAULT_RELAY_CU_URL,
    fetch: fetch2 = defaultFetch
  }) {
    const logger = _logger.child("mainnet-process");
    logger("Mode Activated %s", "\u{1F432}");
    if (!signer) {
      throw new Error("mainnet mode requires providing a signer to connect()");
    }
    const mainnetDataItemSigner = signer ? () => signer : createDataItemSigner3;
    const mainnetSigner = signer ? () => signer : createSigner3;
    const getMessageById2 = messageIdWith({
      getMessageId: getMessageById({ fetch: fetch2, locate })
    });
    const getMessages = messagesWith({
      messages: getMessagesByRange({ fetch: fetch2, locate })
    });
    const getLastSlot = processIdWith({
      processId: getLastSlotWith({
        fetch: fetch2,
        locate
      })
    });
    const requestLogger = logger.child("request");
    const request = requestWith2({
      signer,
      logger: requestLogger,
      MODE,
      method: "GET",
      request: requestWith({
        fetch: defaultFetch,
        logger: requestLogger,
        HB_URL: URL2,
        signer
      })
    });
    return {
      MODE: "mainnet",
      request,
      createSigner: mainnetSigner,
      createDataItemSigner: mainnetDataItemSigner,
      getMessages,
      getLastSlot,
      getMessageById: getMessageById2
      /**
      * do we want helpers for payments?
      *
      * - getOperator
      * - getNodeBalance
      */
    };
  }
  function connect2(args = {}) {
    let { GRAPHQL_URL: GRAPHQL_URL2, GATEWAY_URL: GATEWAY_URL2 = DEFAULT_GATEWAY_URL, ...restArgs } = args;
    if (!GRAPHQL_URL2) {
      GRAPHQL_URL2 = joinUrl({ url: GATEWAY_URL2, path: "/graphql" });
    }
    const MODE = args.MODE || "legacy";
    if (MODE === "legacy") return legacyMode({ ...restArgs, GRAPHQL_URL: GRAPHQL_URL2 });
    if (MODE === "mainnet") return mainnetMode({ ...restArgs, GRAPHQL_URL: GRAPHQL_URL2 });
    throw new Error(`Unrecognized MODE: ${MODE}`);
  }
  return connect2;
}

// src/client/node/wallet.js
var wallet_exports = {};
__export(wallet_exports, {
  createDataItemSigner: () => createDataItemSigner,
  createSigner: () => createSigner
});
import { constants, createPrivateKey, createHash, createSign } from "node:crypto";
function createANS104Signer({ privateKey, publicKey, address }) {
  const signer = async (create) => {
    const deepHash = await create({
      type: 1,
      publicKey,
      alg: "rsa-v1_5-sha256"
    });
    const signature = createSign("sha256").update(deepHash).sign({ key: privateKey, padding: constants.RSA_PKCS1_PSS_PADDING });
    return { signature, address };
  };
  return signer;
}
function createHttpSigner({ publicKey, privateKey, address }) {
  const signer = async (create) => {
    const signatureBase = await create({
      type: 1,
      publicKey,
      alg: "rsa-pss-sha512"
    });
    const signature = createSign("sha512").update(signatureBase).sign({ key: privateKey, padding: constants.RSA_PKCS1_PSS_PADDING });
    return { signature, address };
  };
  return signer;
}
function createSigner(wallet) {
  const publicKey = Buffer.from(wallet.n, "base64url");
  const privateKey = createPrivateKey({ key: wallet, format: "jwk" });
  const address = createHash("sha256").update(publicKey).digest("base64url");
  const dataItemSigner = createANS104Signer({ wallet, privateKey, publicKey, address });
  const httpSigner = createHttpSigner({ wallet, publicKey, privateKey, address });
  const signer = (create, kind) => {
    if (kind === DATAITEM_SIGNER_KIND) return dataItemSigner(create);
    if (kind === HTTP_SIGNER_KIND) return httpSigner(create);
    throw new Error(`signer kind unknown "${kind}"`);
  };
  return signer;
}
var createDataItemSigner = createSigner;

// src/index.js
var GATEWAY_URL = process.env.GATEWAY_URL || void 0;
var MU_URL = process.env.MU_URL || void 0;
var CU_URL = process.env.CU_URL || void 0;
var GRAPHQL_URL = process.env.GRAPHQL_URL || void 0;
var GRAPHQL_MAX_RETRIES = process.env.GRAPHQL_MAX_RETRIES || void 0;
var GRAPHQL_RETRY_BACKOFF = process.env.GRAPHQL_RETRY_BACKOFF || void 0;
var RELAY_URL = process.env.RELAY_URL || void 0;
var AO_URL = process.env.AO_URL = void 0;
var connect = connectWith({
  createDataItemSigner: wallet_exports.createDataItemSigner,
  createSigner: wallet_exports.createSigner
});
var createDataItemSigner2 = wallet_exports.createDataItemSigner;
var createSigner2 = wallet_exports.createSigner;
var { result, results, message, spawn, monitor, unmonitor, dryrun, assign } = connect({
  MODE: "legacy",
  GATEWAY_URL,
  MU_URL,
  CU_URL,
  RELAY_URL,
  AO_URL,
  GRAPHQL_URL,
  GRAPHQL_MAX_RETRIES,
  GRAPHQL_RETRY_BACKOFF,
  noLog: true
});
export {
  assign,
  connect,
  createDataItemSigner2 as createDataItemSigner,
  createSigner2 as createSigner,
  dryrun,
  message,
  monitor,
  result,
  results,
  serializeCron,
  spawn,
  unmonitor
};

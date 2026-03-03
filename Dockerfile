FROM ubuntu:22.04 AS builder

RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    pkg-config \
    ncurses-dev \
    libssl-dev \
    sudo \
    curl \
    ca-certificates

RUN git clone https://github.com/erlang/otp.git && \
    cd otp && \
    git checkout maint-27 && \
    ./configure && \
    make -j16 && \
    sudo make install

RUN git clone https://github.com/erlang/rebar3.git && \
    cd rebar3 && \
    ./bootstrap && \
    sudo mv rebar3 /usr/local/bin/

# install node 22 (used by genesis_wasm profile)
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y nodejs && \
    node --version

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /opt

COPY . .

# compile the project with provided profiles
RUN rebar3 clean && rebar3 get-deps && rebar3 as genesis_wasm release

FROM ubuntu:22.04 AS runner

WORKDIR /opt

# Install Node 22 dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    gnupg

# node 22 is still needed for genesis_wasm profile
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y nodejs && \
    node --version

# copy the build artifacts from the builder stage
COPY --from=builder /opt/_build/ /opt/_build/

# copy the wallet file
COPY wallets/wallet1.json /opt/_build/genesis_wasm/rel/hb/hyperbeam-key.json

# Apply genesis-wasm fixes:
# Fix 1: parseInt string concatenation bug (from = '438779' + 1 = '4387791' instead of 438780)
RUN sed -i 's/if (!isColdStart) from = from + 1/if (!isColdStart) from = parseInt(`${from}`) + 1/' \
    /opt/_build/genesis_wasm/rel/hb/genesis-wasm-server/src/effects/hb/index.js
# Fix 2: Allow genesis-wasm to fetch messages from HyperBEAM scheduler when body is insufficient
#         (needed after restart to catch up from checkpoint without failing)
RUN sed -i "/if (!dryRun) throw new Error('Body is not valid: would attempt to fetch from scheduler in loadMessages')/d" \
    /opt/_build/genesis_wasm/rel/hb/genesis-wasm-server/src/effects/hb/index.js
# Fix 3: Allow loadMessageMeta to fetch from scheduler when body is missing/invalid
#         (needed for dryrun with ?to=<slot> pinning; previously always threw)
RUN node -e "
  const fs = require('fs');
  const file = '/opt/_build/genesis_wasm/rel/hb/genesis-wasm-server/src/effects/hb/index.js';
  let c = fs.readFileSync(file, 'utf8');
  c = c.replace(
    \"        throw new Error('Body is not valid: would attempt to fetch from scheduler in loadMessageMeta')\",
    '        return fetch(\`\${suUrl}/~scheduler@1.0/schedule?\${toParams({ processId, from: messageUid, to: messageUid, pageSize: 1 }).toString()}\`).then(okRes)'
  );
  fs.writeFileSync(file, c);
"

# Fix 4: Pass the correct 'before' target to findFileCheckpointBefore and
#         findRecordCheckpointBefore in findLatestProcessMemory. Previously they
#         hardcoded LATEST, causing maybeFile/maybeRecord to always find the newest
#         checkpoint and block fallthrough to Arweave records for historical dry-runs.
RUN node -e "
  const fs = require('fs');
  const file = '/opt/_build/genesis_wasm/rel/hb/genesis-wasm-server/src/effects/ao-process.js';
  let c = fs.readFileSync(file, 'utf8');
  // Fix maybeFile: destructure before from args and pass it instead of LATEST
  c = c.replace(
    'function maybeFile (args) {\n    const { processId, omitMemory } = args\n    /**\n     * Attempt to find the latest checkpoint in a file\n     */\n    return findFileCheckpointBefore({ processId, before: LATEST })',
    'function maybeFile (args) {\n    const { processId, omitMemory, before } = args\n    /**\n     * Attempt to find a file checkpoint at or before the requested slot.\n     */\n    return findFileCheckpointBefore({ processId, before })'
  );
  // Fix maybeRecord: destructure before from args and pass it instead of LATEST
  c = c.replace(
    'function maybeRecord (args) {\n    const { processId, omitMemory } = args',
    'function maybeRecord (args) {\n    const { processId, omitMemory, before } = args'
  );
  c = c.replace(
    'return findRecordCheckpointBefore({ processId, before: LATEST })',
    'return findRecordCheckpointBefore({ processId, before })'
  );
  fs.writeFileSync(file, c);
"

# Fix 5: Pass the correct 'before' target to determineLatestCheckpoint and
#         maybeCheckpointFromArweave. Previously they hardcoded LATEST, so
#         Arweave checkpoint queries would always return the newest checkpoint
#         rather than the nearest one at or before the requested evaluation slot.
RUN node -e "
  const fs = require('fs');
  const file = '/opt/_build/genesis_wasm/rel/hb/genesis-wasm-server/src/effects/ao-process.js';
  let c = fs.readFileSync(file, 'utf8');
  // Fix determineLatestCheckpoint: add 'before' param (defaults to LATEST for backward compat)
  c = c.replace(
    'function determineLatestCheckpoint (edges) {',
    'function determineLatestCheckpoint (edges, before = LATEST) {'
  );
  c = c.replace(
    \`      /**
       * Pass the LATEST flag, which configures latestCheckpointBefore
       * to only be concerned with finding the absolute latest checkpoint
       * in the list
       */
      latestCheckpointBefore(LATEST),\`,
    \`      /**
       * Use the provided \\\`before\\\` target so we only consider checkpoints
       * at or before the requested evaluation point.
       */
      latestCheckpointBefore(before),\`
  );
  // Fix maybeCheckpointFromArweave: destructure before from args and pass it
  c = c.replace(
    \`  function maybeCheckpointFromArweave (args) {
    const { processId, omitMemory } = args\`,
    \`  function maybeCheckpointFromArweave (args) {
    const { processId, omitMemory, before } = args\`
  );
  c = c.replace(
    \`          before: LATEST
        })
      })
      .map(path(['data', 'transactions', 'edges']))
      .map(determineLatestCheckpoint)\`,
    \`          before
        })
      })
      .map(path(['data', 'transactions', 'edges']))
      .map((edges) => determineLatestCheckpoint(edges, before))\`
  );
  fs.writeFileSync(file, c);
"

# Fix 6: maybeCached must also respect the 'before' target. Previously it
#         returned the in-memory cached checkpoint unconditionally, blocking
#         the fallthrough to maybeFile/maybeRecord/maybeCheckpointFromArweave
#         whenever the cached ordinate was newer than the requested target slot.
#         Use isLaterThan (strict) so a cached state at the EXACT target ordinate
#         is accepted as a valid dry-run starting point.
RUN node -e "
  const fs = require('fs');
  const file = '/opt/_build/genesis_wasm/rel/hb/genesis-wasm-server/src/effects/ao-process.js';
  let c = fs.readFileSync(file, 'utf8');
  c = c.replace(
    \`  function maybeCached (args) {
    const { processId, omitMemory } = args

    return of(processId)
      .chain((processId) => {
        const cached = cache.get(processId)

        /**
         * There is no cached memory, so keep looking
         */
        if (!cached) return Rejected(args)\`,
    \`  function maybeCached (args) {
    const { processId, omitMemory, before } = args

    return of(processId)
      .chain((processId) => {
        const cached = cache.get(processId)

        /**
         * There is no cached memory, so keep looking
         */
        if (!cached) return Rejected(args)

        /**
         * If a specific 'before' target is given, verify the cached checkpoint
         * is not STRICTLY NEWER than the target. A cached state at exactly the
         * target ordinate is a valid starting point and should be accepted.
         * Only fall through to the checkpoint search chain when the cache is
         * newer than what was requested.
         */
        if (before !== LATEST) {
          if (isLaterThan(before, cached.evaluation)) return Rejected(args)
        }\`
  );
  fs.writeFileSync(file, c);
"

# Fix 7: Paginate Arweave checkpoint queries for historical dry-runs.
#         maybeCheckpointFromArweave previously fetched only 50 checkpoints
#         sorted HEIGHT_DESC (newest first). When the target slot is far behind
#         the live state, the relevant checkpoint is beyond the first 50 results
#         and genesis-wasm falls back to a cold start. When before !== LATEST,
#         this fix paginates through Arweave checkpoints (up to 200 pages x 50
#         = 10000 checkpoints) until it finds one with nonce <= before.ordinate.
RUN node -e "
  const fs = require('fs');
  const file = '/opt/_build/genesis_wasm/rel/hb/genesis-wasm-server/src/effects/ao-process.js';
  let c = fs.readFileSync(file, 'utf8');

  // Fix 7a: Add \$after cursor param and pageInfo/cursor fields to GET_AO_PROCESS_CHECKPOINTS
  c = c.replace(
    \`  const GET_AO_PROCESS_CHECKPOINTS = \\\`
    query GetAoProcessCheckpoints(
      \\\$owners: [String!]!
      \\\$processId: String!
      \\\$limit: Int!
    ) {
      transactions(
        tags: [
          { name: \"Process\", values: [\\\$processId] }
          { name: \"Type\", values: [\"Checkpoint\"] }
          { name: \"Data-Protocol\", values: [\"ao\"] }
        ],
        owners: \\\$owners,
        first: \\\$limit,
        sort: HEIGHT_DESC
      ) {
        edges {
          node {
            id
            owner {
              address
            }
            tags {
              name
              value
            }
          }
        }
      }
    }
  \\\`\`,
    \`  const GET_AO_PROCESS_CHECKPOINTS = \\\`
    query GetAoProcessCheckpoints(
      \\\$owners: [String!]!
      \\\$processId: String!
      \\\$limit: Int!
      \\\$after: String
    ) {
      transactions(
        tags: [
          { name: \"Process\", values: [\\\$processId] }
          { name: \"Type\", values: [\"Checkpoint\"] }
          { name: \"Data-Protocol\", values: [\"ao\"] }
        ],
        owners: \\\$owners,
        first: \\\$limit,
        after: \\\$after,
        sort: HEIGHT_DESC
      ) {
        pageInfo {
          hasNextPage
        }
        edges {
          cursor
          node {
            id
            owner {
              address
            }
            tags {
              name
              value
            }
          }
        }
      }
    }
  \\\`\`
  );

  // Fix 7b: Replace single-page query in maybeCheckpointFromArweave with
  //         paginated fetch that stops once a checkpoint <= before is found
  c = c.replace(
    \`      .chain((owners) => {
        return queryCheckpoints({
          query: GET_AO_PROCESS_CHECKPOINTS,
          variables: { owners, processId, limit: 50 },
          processId,
          before
        })
      })
      .map(path(['data', 'transactions', 'edges']))
      .map((edges) => determineLatestCheckpoint(edges, before))
      .chain((latestCheckpoint) => {
        if (!latestCheckpoint) return Rejected(args)\`,
    \`      .chain((owners) => {
        if (before === LATEST) {
          return queryCheckpoints({
            query: GET_AO_PROCESS_CHECKPOINTS,
            variables: { owners, processId, limit: 50 },
            processId,
            before
          })
            .map(path(['data', 'transactions', 'edges']))
        }

        const PAGE_SIZE = 50
        const MAX_PAGES = 200
        return fromPromise(async () => {
          let cursor
          let allEdges = []
          for (let attempt = 0; attempt < MAX_PAGES; attempt++) {
            const variables = { owners, processId, limit: PAGE_SIZE }
            if (cursor) variables.after = cursor
            let res
            try {
              res = await queryGateway({ query: GET_AO_PROCESS_CHECKPOINTS, variables })
            } catch (e) {
              logger(
                'queryGateway failed on checkpoint pagination attempt %d for process \"%s\": %O',
                attempt + 1, processId, e
              )
              try {
                res = await queryCheckpointGateway({ query: GET_AO_PROCESS_CHECKPOINTS, variables })
              } catch (e2) {
                logger(
                  'queryCheckpointGateway also failed on checkpoint pagination attempt %d for process \"%s\": %O',
                  attempt + 1, processId, e2
                )
                break
              }
            }
            const txs = res?.data?.transactions
            if (!txs) break
            const edges = txs.edges || []
            allEdges = allEdges.concat(edges)
            const found = determineLatestCheckpoint(allEdges, before)
            if (found) {
              logger(
                'Found Arweave checkpoint for process \"%s\" before \"%j\" after %d page(s)',
                processId, before, attempt + 1
              )
              break
            }
            if (!txs.pageInfo?.hasNextPage || edges.length === 0) break
            cursor = edges[edges.length - 1].cursor
            if (!cursor) break
          }
          return allEdges
        })()
      })
      .map((edges) => determineLatestCheckpoint(edges, before))
      .chain((latestCheckpoint) => {
        if (!latestCheckpoint) return Rejected(args)\`
  );
  fs.writeFileSync(file, c);
"

# bin bash here to start the container
ENTRYPOINT ["/opt/_build/genesis_wasm/rel/hb/bin/hb"]

CMD ["foreground"]

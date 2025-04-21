-- load hyperAOS
require('../test/apm_modules/@apm/hyper-aos/source')
require('../test/token')

local stringify = require('.stringify')

-- helper functions
--
-- sends message to process
local function send_message(msg, opts)
  opts = opts or {}
  local base = {
    process = {
      commitments = { PROCESS = { alg = "rsa-pss-sha512", committer = "OWNER" }},
      authority = {"NETWORK1", "NETWORK2","OWNER"},
      type = "Process"
    }
  }
  local req = {
    path = "schedule",
    method = "POST",
    slot = opts.slot or 1,
    timestamp = opts.timestamp or os.time(),
    ['block-height'] = opts.height or "1000",
    body = msg
  }
  return compute(base, req)
end

local function send_process_message()
  return send_message({
    commitments = { PROCESS = { alg = "rsa-pss-sha512", committer = "OWNER" }},
    target = "TOKEN",
    authority = {"NETWORK1", "NETWORK2","OWNER" },
    type = "Process"
  }, { slot = 1})
end

function Token_test()
  return "ok"
end
-- function token_transfer_test ()
--   -- reset state
--   Inbox = {}
--   Balances = {}
--   Initialized = nil
--
--   -- send init msg
--   local base = send_process_message()
--   -- set balance
--   Balances = { address1 = "59090", address2 = "10000" }
--   base = send_message({
--     commitments = { MSG = { alg = "rsa-pss-sha512", committer = "address1" }},
--     target = "TOKEN",
--     action = "Transfer",
--     quantity = "500",
--     recipient = "address3"
--   })
--   assert(base.results.outbox["1"].action == "Debit-Notice", "should get Debit-Notice")
--   assert(base.results.outbox["2"].action == "Credit-Notice", "should get Credit-Notice")
--   assert(Balances["address3"] == "500", "address3 should have 500")
--   assert(Balances["address1"] == "58590", "address1 should be less 500")
--   return "ok"
-- end
--
-- t:add('get balances', function ()
--   -- reset state
--   Inbox = {}
--   Balances = {}
--   Initialized = nil
--
--   -- send init msg
--   local base = send_message({
--     commitments = { PROCESS = { alg = "rsa-pss-sha512", committer = "OWNER" }},
--     target = "TOKEN",
--     authority = {"NETWORK1", "NETWORK2","OWNER" },
--     type = "Process"
--   }, { slot = 1})
--   -- set balance
--   Balances = { address1 = "59090", address2 = "10000" }
--   base = send_message({
--     commitments = { MSG = { alg = "rsa-pss-sha512", committer = "address1" }},
--     target = "TOKEN",
--     action = "Balances"
--   })
--   -- _print(stringify.format(base.results.outbox["1"]))
--   assert(base.results.outbox["1"].mintedsupply == "69090", "should get balances")
--
-- end)
--
-- t:add('get info', function ()
--   -- reset state
--   Inbox = {}
--   Balances = {}
--   Initialized = nil
--
--   -- send init msg
--   local base = send_message({
--     commitments = { PROCESS = { alg = "rsa-pss-sha512", committer = "OWNER" }},
--     target = "TOKEN",
--     authority = {"NETWORK1", "NETWORK2","OWNER" },
--     type = "Process"
--   }, { slot = 1})
--   -- set balance
--   Balances = { address1 = "59090" }
--   base = send_message({
--     commitments = { MSG = { alg = "rsa-pss-sha512", committer = "address1" }},
--     target = "TOKEN",
--     action = "Info"
--   })
--
--   assert(base.results.outbox["1"].name == "aos", "should get info")
-- end)
--
-- t:add('get balance', function ()
--   -- reset state
--   Inbox = {}
--   Balances = {}
--   Initialized = nil
--
--   -- send init msg
--   local base = send_message({
--     commitments = { PROCESS = { alg = "rsa-pss-sha512", committer = "OWNER" }},
--     target = "TOKEN",
--     authority = {"NETWORK1", "NETWORK2","OWNER" },
--     type = "Process"
--   }, { slot = 1})
--   -- set balance
--   Balances = { address1 = "59090" }
--   base = send_message({
--     commitments = { MSG = { alg = "rsa-pss-sha512", committer = "address1" }},
--     target = "TOKEN",
--     action = "Balance"
--   })
--   assert(base.results.outbox["1"].data == "59090", "should get callers balance")
-- end)
--
function Token_mint_token_test()
  -- reset state
  Inbox = {}
  Balances = {}
  Initialized = nil
  -- init with process message
  local base = send_message({
    commitments = { PROCESS = { alg = "rsa-pss-sha512", committer = "OWNER" }},
    target = "PROCESS",
    authority = {"NETWORK1", "NETWORK2","OWNER" },
    type = "Process"
  }, { slot = 1 })
  assert(Inbox[1].body.type == "Process", 'added to inbox')
  -- send mint message
  base = send_message({
    target = "TOKEN",
    commitments = { MESSAGE = { alg = "rsa-pss-sha512", committer = "OWNER"}},
    action = "Mint",
    data = "Address1,1000\\nAddress2,50",
    format = "CSV"

  }, {slot = 2})
  ao.event("rakis", { "results:", base.results })
  assert(base.results.output.data == "Successfully processed mint request")
  return "ok"
end

function Token_send_simple_message_test ()
  local state = {
    process = {
      commitments = {
        PROCESS = {
          alg = "rsa-pss-sha512",
          committer = "OWNER"
        }
      }
    }
  }
  local req = {
    path = "schedule",
    method = "POST",
    body = {
      target = "1",
      data = "1 + 1",
      action = "Eval",
      commitments = {
        MSG = {
          alg = "rsa-pss-sha512",
          committer = "OWNER"
        }
      }
    }
  }
  state = compute(state, req)
  -- _print(require('.stringify').format(base.results))
  assert(state.results.output.data == "2", 'success')
  return "ok"
end

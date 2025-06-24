--- The Eval module provides a handler for evaluating Lua expressions. Returns the eval function.
-- @module eval

local stringify = require(".stringify")
local json = require('.json')
--- The eval function.
-- Handler for executing and evaluating Lua expressions.
-- After execution, the result is stringified and placed in aos.outbox.Output.
-- @function eval
-- @tparam {table} aos The aos environment object
-- @treturn {function} The handler function, which takes a message as an argument.
-- @see stringify
EvalFn = function (aos)
  return function (req)
    local msg = req.body
    -- exec expression
    local expr = msg.body and msg.body.body or msg.data or ""
    local func, err = load("return " .. expr, 'aos', 't', _G)
    local output = ""
    local e = nil
    if err then
      func, err = load(expr, 'aos', 't', _G)
    end
    if func then
      output, e = func()
    else
      aos.outbox.Error = err
      return
    end
    if e then
      aos.outbox.Error = e
      return
    end
    if HandlerPrintLogs and output then
      table.insert(HandlerPrintLogs,
        type(output) == "table"
        and stringify.format(output)
        or tostring(output)
      )
      -- print(stringify.format(HandlerPrintLogs))
    -- else
    --   -- set result in outbox.Output (Left for backwards compatibility)
    --   aos.outbox.Output = {
    --     data = type(output) == "table" 
    --       and stringify.format(output) or tostring(output),
    --     prompt = Prompt()
    --   }
    --
    end
  end
end

_G.package.loaded['.eval'] = EvalFn
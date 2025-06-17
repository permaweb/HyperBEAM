--- The Handlers library provides a flexible way to manage and execute a series of handlers based on patterns. Each handler consists of a pattern function, a handle function, and a name. This library is suitable for scenarios where different actions need to be taken based on varying input criteria. Returns the handlers table.
-- @module handlers

--- The handlers table
-- @table handlers
-- @field _version The version number of the handlers module
-- @field list The list of handlers
-- @field onceNonce The nonce for the once handlers
-- @field utils The handlers-utils module
-- @field generateResolver The generateResolver function
-- @field receive The receive function
-- @field once The once function
-- @field add The add function
-- @field append The append function
-- @field prepend The prepend function
-- @field remove The remove function
-- @field evaluate The evaluate function
local handlers = { _version = "0.0.5" }
local utils = require('test.hyper-utils')

handlers.utils = require('test.hyper-handlers-utils')
-- if update we need to keep defined handlers
if Handlers then
  handlers.list = Handlers.list or {}
else
  handlers.list = {}
end
handlers.onceNonce = 0

--- Given an array, a property name, and a value, returns the index of the object in the array that has the property with the value.
-- @lfunction findIndexByProp
-- @tparam {table[]} array The array to search through
-- @tparam {string} prop The property name to check
-- @tparam {any} value The value to check for in the property
-- @treturn {number | nil} The index of the object in the array that has the property with the value, or nil if no such object is found
local function findIndexByProp(array, prop, value)
  for index, object in ipairs(array) do
    if object[prop] == value then
      return index
    end
  end
  return nil
end

--- Given a name, a pattern, and a handle, asserts that the arguments are valid.
-- @lfunction assertAddArgs
-- @tparam {string} name The name of the handler
-- @tparam {table | function | string} pattern The pattern to check for in the message
-- @tparam {function} handle The function to call if the pattern matches
-- @tparam {number | string | nil} maxRuns The maximum number of times the handler should run, or nil if there is no limit
local function assertAddArgs(name, pattern, handle, maxRuns)
  assert(
    type(name) == 'string' and
    (type(pattern) == 'function' or type(pattern) == 'table' or type(pattern) == 'string'),
    'Invalid arguments given. Expected: \n' ..
    '\tname : string, ' ..
    '\tpattern : action : string | MsgMatch : table,\n' ..
    '\t\tfunction(msg: Message) : {-1 = break, 0 = skip, 1 = continue},\n' ..
    '\thandle(msg : Message) : void) | Resolver,\n' ..
    '\tMaxRuns? : number | "inf" | nil')
end

--- Given a resolver specification, returns a resolver function.
-- @function generateResolver
-- @tparam {table | function} resolveSpec The resolver specification
-- @treturn {function} A resolver function
function handlers.generateResolver(resolveSpec)
  return function(msg)
    -- If the resolver is a single function, call it.
    -- Else, find the first matching pattern (by its matchSpec), and exec.
    if type(resolveSpec) == "function" then
      return resolveSpec(msg)
    else
        for matchSpec, func in pairs(resolveSpec) do
            if utils.matchesSpec(msg, matchSpec) then
                return func(msg)
            end
        end
    end
  end
end

--- Given a pattern, returns the next message that matches the pattern.
-- This function uses Lua's coroutines under-the-hood to add a handler, pause,
-- and then resume the current coroutine. This allows us to effectively block
-- processing of one message until another is received that matches the pattern.
-- @function receive
-- @tparam {table | function} pattern The pattern to check for in the message
function handlers.receive(pattern)
  return 'not implemented'
end

--- Given a name, a pattern, and a handle, adds a handler to the list.
-- If name is not provided, "_once_" prefix plus onceNonce will be used as the name.
-- Adds handler with maxRuns of 1 such that it will only be called once then removed from the list.
-- @function once
-- @tparam {string} name The name of the handler
-- @tparam {table | function | string} pattern The pattern to check for in the message
-- @tparam {function} handle The function to call if the pattern matches
function handlers.once(...)
  local name, pattern, handle
  if select("#", ...) == 3 then
    name = select(1, ...)
    pattern = select(2, ...)
    handle = select(3, ...)
  else
    name = "_once_" .. tostring(handlers.onceNonce)
    handlers.onceNonce = handlers.onceNonce + 1
    pattern = select(1, ...)
    handle = select(2, ...)
  end
  handlers.prepend(name, pattern, handle, 1)
end

--- Given a name, a pattern, and a handle, adds a handler to the list.
-- @function add
-- @tparam {string} name The name of the handler
-- @tparam {table | function | string} pattern The pattern to check for in the message
-- @tparam {function} handle The function to call if the pattern matches
-- @tparam {number | string | nil} maxRuns The maximum number of times the handler should run, or nil if there is no limit
function handlers.add(...)
  local name, pattern, handle, maxRuns
  local args = select("#", ...)
  if args == 2 then
    name = select(1, ...)
    pattern = select(1, ...)
    handle = select(2, ...)
    maxRuns = nil
  elseif args == 3 then
    name = select(1, ...)
    pattern = select(2, ...)
    handle = select(3, ...)
    maxRuns = nil
  else
    name = select(1, ...)
    pattern = select(2, ...)
    handle = select(3, ...)
    maxRuns = select(4, ...)
  end
  assertAddArgs(name, pattern, handle, maxRuns)

  handle = handlers.generateResolver(handle)

  -- update existing handler by name
  local idx = findIndexByProp(handlers.list, "name", name)
  if idx ~= nil and idx > 0 then
    -- found update
    handlers.list[idx].pattern = pattern
    handlers.list[idx].handle = handle
    handlers.list[idx].maxRuns = maxRuns
  else
    -- not found then add    
    table.insert(handlers.list, { pattern = pattern, handle = handle, name = name, maxRuns = maxRuns })

  end
  return #handlers.list
end

--- Appends a new handler to the end of the handlers list.
-- @function append
-- @tparam {string} name The name of the handler
-- @tparam {table | function | string} pattern The pattern to check for in the message
-- @tparam {function} handle The function to call if the pattern matches
-- @tparam {number | string | nil} maxRuns The maximum number of times the handler should run, or nil if there is no limit
function handlers.append(...)
  local name, pattern, handle, maxRuns
  local args = select("#", ...)
  if args == 2 then
    name = select(1, ...)
    pattern = select(1, ...)
    handle = select(2, ...)
    maxRuns = nil
  elseif args == 3 then
    name = select(1, ...)
    pattern = select(2, ...)
    handle = select(3, ...)
    maxRuns = nil
  else
    name = select(1, ...)
    pattern = select(2, ...)
    handle = select(3, ...)
    maxRuns = select(4, ...)
  end
  assertAddArgs(name, pattern, handle, maxRuns)

  handle = handlers.generateResolver(handle)
  -- update existing handler by name
  local idx = findIndexByProp(handlers.list, "name", name)
  if idx ~= nil and idx > 0 then
    -- found update
    handlers.list[idx].pattern = pattern
    handlers.list[idx].handle = handle
    handlers.list[idx].maxRuns = maxRuns
  else
    table.insert(handlers.list, { pattern = pattern, handle = handle, name = name, maxRuns = maxRuns })
  end
end

--- Prepends a new handler to the beginning of the handlers list.
-- @function prepend
-- @tparam {string} name The name of the handler
-- @tparam {table | function | string} pattern The pattern to check for in the message
-- @tparam {function} handle The function to call if the pattern matches
-- @tparam {number | string | nil} maxRuns The maximum number of times the handler should run, or nil if there is no limit
function handlers.prepend(...)
  local name, pattern, handle, maxRuns
  local args = select("#", ...)
  if args == 2 then
    name = select(1, ...)
    pattern = select(1, ...)
    handle = select(2, ...)
    maxRuns = nil
  elseif args == 3 then
    name = select(1, ...)
    pattern = select(2, ...)
    handle = select(3, ...)
    maxRuns = nil
  else 
    name = select(1, ...)
    pattern = select(2, ...)
    handle = select(3, ...)
    maxRuns = select(4, ...)
  end
  assertAddArgs(name, pattern, handle, maxRuns)

  handle = handlers.generateResolver(handle)

  -- update existing handler by name
  local idx = findIndexByProp(handlers.list, "name", name)
  if idx ~= nil and idx > 0 then
    -- found update
    handlers.list[idx].pattern = pattern
    handlers.list[idx].handle = handle
    handlers.list[idx].maxRuns = maxRuns
  else  
    table.insert(handlers.list, 1, { pattern = pattern, handle = handle, name = name, maxRuns = maxRuns })
  end
end

--- Returns an object that allows adding a new handler before a specified handler.
-- @function before
-- @tparam {string} handleName The name of the handler before which the new handler will be added
-- @treturn {table} An object with an `add` method to insert the new handler
function handlers.before(handleName)
  assert(type(handleName) == 'string', 'Handler name MUST be a string')

  local idx = findIndexByProp(handlers.list, "name", handleName)
  return {
    add = function (name, pattern, handle, maxRuns) 
      assertAddArgs(name, pattern, handle, maxRuns)
      handle = handlers.generateResolver(handle)
      if idx then
        table.insert(handlers.list, idx, { pattern = pattern, handle = handle, name = name, maxRuns = maxRuns })
      end
    end
  }
end

--- Returns an object that allows adding a new handler after a specified handler.
-- @function after
-- @tparam {string} handleName The name of the handler after which the new handler will be added
-- @treturn {table} An object with an `add` method to insert the new handler
function handlers.after(handleName)
  assert(type(handleName) == 'string', 'Handler name MUST be a string')
  local idx = findIndexByProp(handlers.list, "name", handleName)
  return {
    add = function (name, pattern, handle, maxRuns)
      assertAddArgs(name, pattern, handle, maxRuns)
      handle = handlers.generateResolver(handle)
      if idx then
        table.insert(handlers.list, idx + 1, { pattern = pattern, handle = handle, name = name, maxRuns = maxRuns })
      end
    end
  }

end

--- Removes a handler from the handlers list by name.
-- @function remove
-- @tparam {string} name The name of the handler to be removed
function handlers.remove(name)
  assert(type(name) == 'string', 'name MUST be string')
  if #handlers.list == 1 and handlers.list[1].name == name then
    handlers.list = {}
  end

  local idx = findIndexByProp(handlers.list, "name", name)
  if idx ~= nil and idx > 0 then
    table.remove(handlers.list, idx)
  end
end

--- Evaluates each handler against a given message and environment. Handlers are called in the order they appear in the handlers list.
-- Return 0 to not call handler, -1 to break after handler is called, 1 to continue
-- @function evaluate
-- @tparam {table} msg The message to be processed by the handlers.
-- @tparam {table} env The environment in which the handlers are executed.
-- @treturn The response from the handler(s). Returns a default message if no handler matches.
function handlers.evaluate(msg, env)
  local handled = false
  assert(type(msg) == 'table', 'msg is not valid')
  assert(type(env) == 'table', 'env is not valid')
  for _, o in ipairs(handlers.list) do
    if o.name ~= "_default" then
      local match = utils.matchesSpec(msg, o.pattern)
      if not (type(match) == 'number' or type(match) == 'string' or type(match) == 'boolean') then
        error("Pattern result is not valid, it MUST be string, number, or boolean")
      end
      -- handle boolean returns
      if type(match) == "boolean" and match == true then
        match = -1
      elseif type(match) == "boolean" and match == false then
        match = 0
      end

      -- handle string returns
      if type(match) == "string" then
        if match == "continue" then
          match = 1
        elseif match == "break" then
          match = -1
        else
          match = 0
        end
      end

      if match ~= 0 then
        if match < 0 then
          handled = true
        end
        -- each handle function can accept, the msg, env
        local status, err = pcall(o.handle, msg, env)
        if not status then
          error(err)
        end
        -- remove handler if maxRuns is reached. maxRuns can be either a number or "inf"
        if o.maxRuns ~= nil and o.maxRuns ~= "inf" then
          o.maxRuns = o.maxRuns - 1
          if o.maxRuns == 0 then
            handlers.remove(o.name)
          end
        end
      end
      if match < 0 then
        return handled
      end
    end
  end
  -- do default
  if not handled then
    local idx = findIndexByProp(handlers.list, "name", "_default")
    handlers.list[idx].handle(msg,env)
  end
end

return handlers

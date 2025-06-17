--- The Handler Utils module is a lightweight Lua utility library designed to provide common functionalities for handling and processing messages within the AOS computer system. It offers a set of functions to check message attributes and send replies, simplifying the development of more complex scripts and modules. This document will guide you through the module's functionalities, installation, and usage. Returns the _utils table.
-- @module handlers-utils

--- The _utils table
-- @table _utils
-- @field _version The version number of the _utils module
-- @field hasMatchingTag The hasMatchingTag function
-- @field hasMatchingTagOf The hasMatchingTagOf function
-- @field hasMatchingData The hasMatchingData function
-- @field reply The reply function
-- @field continue The continue function
local _utils = { _version = "0.0.2" }

local _ = require('test.hyper-utils')

--- Checks if a given message has a tag that matches the specified name and value.
-- @function hasMatchingTag
-- @tparam {string} name The tag name to check
-- @tparam {string} value The value to match for in the tag
-- @treturn {function} A function that takes a message and returns whether there is a tag match (-1 if matches, 0 otherwise)
function _utils.hasMatchingTag(name, value)
  assert(type(name) == 'string' and type(value) == 'string', 'invalid arguments: (name : string, value : string)')

  return function (msg)
    return msg.Tags[name] == value
  end
end

--- Checks if a given message has a tag that matches the specified name and one of the specified values.
-- @function hasMatchingTagOf
-- @tparam {string} name The tag name to check
-- @tparam {string[]} values The list of values of which one should match
-- @treturn {function} A function that takes a message and returns whether there is a tag match (-1 if matches, 0 otherwise)
function _utils.hasMatchingTagOf(name, values)
  assert(type(name) == 'string' and type(values) == 'table', 'invalid arguments: (name : string, values : string[])')
  return function (msg)
    for _, value in ipairs(values) do
      local patternResult = Handlers.utils.hasMatchingTag(name, value)(msg)

      if patternResult ~= 0 and patternResult ~= false and patternResult ~= "skip" then
        return patternResult
      end
    end

    return 0
  end
end

--- Checks if a given message has data that matches the specified value.
-- @function hasMatchingData
-- @tparam {string} value The value to match against the message data
-- @treturn {function} A function that takes a message and returns whether the data matches the value (-1 if matches, 0 otherwise)
function _utils.hasMatchingData(value)
  assert(type(value) == 'string', 'invalid arguments: (value : string)')
  return function (msg)
    return msg.Data == value
  end
end

--- Given an input, returns a function that takes a message and replies to it.
-- @function reply
-- @tparam {table | string} input The content to send back. If a string, it sends it as data. If a table, it assumes a structure with `Tags`.
-- @treturn {function} A function that takes a message and replies to it
function _utils.reply(input) 
  assert(type(input) == 'table' or type(input) == 'string', 'invalid arguments: (input : table or string)')
  return function (msg)
    if type(input) == 'string' then
      msg.reply({ Data = input })
      return
    end
    msg.reply(input)
  end
end

--- Inverts the provided pattern's result if it matches, so that it continues execution with the next matching handler.
-- @function continue
-- @tparam {table | function} pattern The pattern to check for in the message
-- @treturn {function} Function that executes the pattern matching function and returns `1` (continue), so that the execution of handlers continues.
function _utils.continue(pattern)
  return function (msg)
    local match = _.matchesSpec(msg, pattern)

    if not match or match == 0 or match == "skip" then
      return match
    end
    return 1
  end
end

return _utils
--- The Assignment module provides functionality for handling assignments. Returns the Assignment table.
-- @module assignment

--- The Assignment module
-- @table Assignment
-- @field _version The version number of the assignment module
-- @field init The init function
local Assignment = { _version = "0.1.0" }
local utils = require('.utils')

--- Implement assignable polyfills on aos.
-- Creates addAssignable, removeAssignable, isAssignment, and isAssignable fields on aos.
-- @function init
-- @tparam {table} aos The aos environment object
-- @see aos.addAssignable
-- @see aos.removeAssignable
-- @see aos.isAssignment
-- @see aos.isAssignable
function Assignment.init (aos)
  -- Find the index of an object in an array by a given property
  -- @lfunction findIndexByProp
  -- @tparam {table} array The array to search
  -- @tparam {string} prop The property to search by
  -- @tparam {any} value The value to search for
  -- @treturn {number|nil} The index of the object, or nil if not found
  local function findIndexByProp(array, prop, value)
    for index, object in ipairs(array) do
      if object[prop] == value then return index end
    end

    return nil
  end

  aos.assignables = aos.assignables or {}

  aos.addAssignable = aos.addAssignable or function (...)
    local name = nil
    local matchSpec = nil

    local idx = nil

    -- Initialize the parameters based on arguments
    if select("#", ...) == 1 then
      matchSpec = select(1, ...)
    else
      name = select(1, ...)
      matchSpec = select(2, ...)
      assert(type(name) == 'string', 'MatchSpec name MUST be a string')
    end

    if name then idx = findIndexByProp(aos.assignables, "name", name) end

    if idx ~= nil and idx > 0 then
      -- found update
      aos.assignables[idx].pattern = matchSpec
    else
      -- append the new assignable, including potentially nil name
      table.insert(aos.assignables, { pattern = matchSpec, name = name })
    end
  end

  aos.removeAssignable = aos.removeAssignable or function (name)
    local idx = nil

    if type(name) == 'string' then idx = findIndexByProp(aos.assignables, "name", name)
    else
      assert(type(name) == 'number', 'index MUST be a number')
      idx = name
    end

    if idx == nil or idx <= 0 or idx > #aos.assignables then return end

    table.remove(aos.assignables, idx)
  end

  aos.isAssignment = aos.isAssignment or function (msg) return msg.Target ~= aos.id end

  aos.isAssignable = aos.isAssignable or function (msg)
    for _, assignable in pairs(aos.assignables) do
      if utils.matchesSpec(msg, assignable.pattern) then return true end
    end

    -- If assignables is empty, the the above loop will noop,
    -- and this expression will execute.
    --
    -- In other words, all msgs are not assignable, by default.
    return false
  end
end

return Assignment

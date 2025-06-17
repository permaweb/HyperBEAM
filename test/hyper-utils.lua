--- The Utils module provides a collection of utility functions for functional programming in Lua. It includes functions for array manipulation such as concatenation, mapping, reduction, filtering, and finding elements, as well as a property equality checker.
-- @module utils

--- The utils table
-- @table utils
-- @field _version The version number of the utils module
-- @field matchesPattern The matchesPattern function
-- @field matchesSpec The matchesSpec function
-- @field curry The curry function
-- @field concat The concat function
-- @field reduce The reduce function
-- @field map The map function
-- @field filter The filter function
-- @field find The find function
-- @field propEq The propEq function
-- @field reverse The reverse function
-- @field compose The compose function
-- @field prop The prop function
-- @field includes The includes function
-- @field keys The keys function
-- @field values The values function
local utils = { _version = "0.0.5" }

--- Given a pattern, a value, and a message, returns whether there is a pattern match.
-- @usage utils.matchesPattern(pattern, value, msg)
-- @param pattern The pattern to match
-- @param value The value to check for in the pattern
-- @param msg The message to check for the pattern
-- @treturn {boolean} Whether there is a pattern match
function utils.matchesPattern(pattern, value, msg)
  -- If the key is not in the message, then it does not match
  if (not pattern) then
    return false
  end
  -- if the patternMatchSpec is a wildcard, then it always matches
  if pattern == '_' then
    return true
  end
  -- if the patternMatchSpec is a function, then it is executed on the tag value
  if type(pattern) == "function" then
    if pattern(value, msg) then
      return true
    else
      return false
    end
  end
  -- if the patternMatchSpec is a string, check it for special symbols (less `-` alone)
  -- and exact string match mode
  if (type(pattern) == 'string') then
    if string.match(pattern, "[%^%$%(%)%%%.%[%]%*%+%?]") then
      if string.match(value, pattern) then
        return true
      end
    else
      if value == pattern then
        return true
      end
    end
  end

  -- if the pattern is a table, recursively check if any of its sub-patterns match
  if type(pattern) == 'table' then
    for _, subPattern in pairs(pattern) do
      if utils.matchesPattern(subPattern, value, msg) then
        return true
      end
    end
  end

  return false
end

--- Given a message and a spec, returns whether there is a spec match.
-- @usage utils.matchesSpec(msg, spec)
-- @param msg The message to check for the spec
-- @param spec The spec to check for in the message
-- @treturn {boolean} Whether there is a spec match
function utils.matchesSpec(msg, spec)
  if type(spec) == 'function' then
    return spec(msg)
  -- If the spec is a table, step through every key/value pair in the pattern and check if the msg matches
  -- Supported pattern types:
  --   - Exact string match
  --   - Lua gmatch string
  --   - '_' (wildcard: Message has tag, but can be any value)
  --   - Function execution on the tag, optionally using the msg as the second argument
  --   - Table of patterns, where ANY of the sub-patterns matching the tag will result in a match
  end
  if type(spec) == 'table' then
    for key, pattern in pairs(spec) do
      -- The key can either be in the top level of the 'msg' object  
      -- or in the body table of the msg
      local msgValue = msg[key] or msg.body[key]
      if not msgValue then
        return false
      end
      local matchesMsgValue = utils.matchesPattern(pattern, msgValue, msg)
      if not matchesMsgValue then
        return false
      end

    end
    return true
  end

  if type(spec) == 'string' and msg.action and msg.action == spec then
    return true
  end
  if type(spec) == 'string' and msg.body.action and msg.body.action == spec then
    return true
  end
  return false
end

--- Given a table, returns whether it is an array.
-- An 'array' is defined as a table with integer keys starting from 1 and
-- having no gaps between the keys.
-- @lfunction isArray
-- @param table The table to check
-- @treturn {boolean} Whether the table is an array
local function isArray(table)
  if type(table) == "table" then
      local maxIndex = 0
      for k, v in pairs(table) do
          if type(k) ~= "number" or k < 1 or math.floor(k) ~= k then
              return false -- If there's a non-integer key, it's not an array
          end
          maxIndex = math.max(maxIndex, k)
      end
      -- If the highest numeric index is equal to the number of elements, it's an array
      return maxIndex == #table
  end
  return false
end

--- Curries a function.
-- @tparam {function} fn The function to curry
-- @tparam {number} arity The arity of the function
-- @treturn {function} The curried function
utils.curry = function (fn, arity)
  assert(type(fn) == "function", "function is required as first argument")
  arity = arity or debug.getinfo(fn, "u").nparams
  if arity < 2 then return fn end

  return function (...)
    local args = {...}

    if #args >= arity then
      return fn(table.unpack(args))
    else
      return utils.curry(function (...)
        return fn(table.unpack(args),  ...)
      end, arity - #args)
    end
  end
end

--- Concat two Array Tables
-- @function concat
-- @usage utils.concat(a)(b)
-- @usage utils.concat({1, 2})({3, 4}) --> {1, 2, 3, 4}
-- @tparam {table<Array>} a The first array
-- @tparam {table<Array>} b The second array
-- @treturn {table<Array>} The concatenated array
utils.concat = utils.curry(function (a, b)
  assert(type(a) == "table", "first argument should be a table that is an array")
  assert(type(b) == "table", "second argument should be a table that is an array")
  assert(isArray(a), "first argument should be a table")
  assert(isArray(b), "second argument should be a table")

  local result = {}
  for i = 1, #a do
      result[#result + 1] = a[i]
  end
  for i = 1, #b do
      result[#result + 1] = b[i]
  end
  return result
end, 2)

--- Applies a function to each element of a table, reducing it to a single value.
-- @function utils.reduce
-- @usage utils.reduce(fn)(initial)(t)
-- @usage utils.reduce(function(acc, x) return acc + x end)(0)({1, 2, 3}) --> 6
-- @tparam {function} fn The function to apply
-- @param initial The initial value
-- @tparam {table<Array>} t The table to reduce
-- @return The reduced value
utils.reduce = utils.curry(function (fn, initial, t)
  assert(type(fn) == "function", "first argument should be a function that accepts (result, value, key)")
  assert(type(t) == "table" and isArray(t), "third argument should be a table that is an array")
  local result = initial
  for k, v in pairs(t) do
    if result == nil then
      result = v
    else
      result = fn(result, v, k)
    end
  end
  return result
end, 3)

--- Applies a function to each element of an array table, mapping it to a new value.
-- @function utils.map
-- @usage utils.map(fn)(t)
-- @usage utils.map(function(x) return x * 2 end)({1, 2, 3}) --> {2, 4, 6}
-- @tparam {function} fn The function to apply to each element
-- @tparam {table<Array>} data The table to map over
-- @treturn {table<Array>} The mapped table
utils.map = utils.curry(function (fn, data)
  assert(type(fn) == "function", "first argument should be a unary function")
  assert(type(data) == "table" and isArray(data), "second argument should be an Array")

  local function map (result, v, k)
    result[k] = fn(v, k)
    return result
  end

  return utils.reduce(map, {}, data)
end, 2)

--- Filters an array table based on a predicate function.
-- @function utils.filter
-- @usage utils.filter(fn)(t)
-- @usage utils.filter(function(x) return x > 1 end)({1, 2, 3}) --> {2,3}
-- @tparam {function} fn The predicate function to determine if an element should be included.
-- @tparam {table<Array>} data The array to filter
-- @treturn {table<Array>} The filtered table
utils.filter = utils.curry(function (fn, data)
  assert(type(fn) == "function", "first argument should be a unary function")
  assert(type(data) == "table" and isArray(data), "second argument should be an Array")

  local function filter (result, v, _k)
    if fn(v) then
      table.insert(result, v)
    end
    return result
  end

  return utils.reduce(filter,{}, data)
end, 2)

--- Finds the first element in an array table that satisfies a predicate function.
-- @function utils.find
-- @usage utils.find(fn)(t)
-- @usage utils.find(function(x) return x > 1 end)({1, 2, 3}) --> 2
-- @tparam {function} fn The predicate function to determine if an element should be included.
-- @tparam {table<Array>} t The array table to search
-- @treturn The first element that satisfies the predicate function
utils.find = utils.curry(function (fn, t)
  assert(type(fn) == "function", "first argument should be a unary function")
  assert(type(t) == "table", "second argument should be a table that is an array")
  for _, v in pairs(t) do
    if fn(v) then
      return v
    end
  end
end, 2)

--- Checks if a property of an object is equal to a value.
-- @function utils.propEq
-- @usage utils.propEq(propName)(value)(object)
-- @usage utils.propEq("name")("Lua")({name = "Lua"}) --> true
-- @tparam {string} propName The property name to check
-- @tparam {string} value The value to check against
-- @tparam {table} object The object to check
-- @treturn {boolean} Whether the property is equal to the value
utils.propEq = utils.curry(function (propName, value, object)
  assert(type(propName) == "string", "first argument should be a string")
  assert(type(value) == "string", "second argument should be a string")
  assert(type(object) == "table", "third argument should be a table<object>")
  
  return object[propName] == value
end, 3)

--- Reverses an array table.
-- @function utils.reverse
-- @usage utils.reverse(data)
-- @usage utils.reverse({1, 2, 3}) --> {3, 2, 1}
-- @tparam {table<Array>} data The array table to reverse
-- @treturn {table<Array>} The reversed array table
utils.reverse = function (data)
  assert(type(data) == "table", "argument needs to be a table that is an array")
  return utils.reduce(
    function (result, v, i)
      result[#data - i + 1] = v
      return result
    end,
    {},
    data
  )
end

--- Composes a series of functions into a single function.
-- @function utils.compose
-- @usage utils.compose(fn1)(fn2)(fn3)(v)
-- @usage utils.compose(function(x) return x + 1 end)(function(x) return x * 2 end)(3) --> 7
-- @tparam {function} ... The functions to compose
-- @treturn {function} The composed function
utils.compose = utils.curry(function (...)
  local mutations = utils.reverse({...})

  return function (v)
    local result = v
    for _, fn in pairs(mutations) do
      assert(type(fn) == "function", "each argument needs to be a function")
      result = fn(result)
    end
    return result
  end
end, 2)

--- Returns the value of a property of an object.
-- @function utils.prop
-- @usage utils.prop(propName)(object)
-- @usage utils.prop("name")({name = "Lua"}) --> "Lua"
-- @tparam {string} propName The property name to get
-- @tparam {table} object The object to get the property from
-- @treturn The value of the property
utils.prop = utils.curry(function (propName, object) 
  return object[propName]
end, 2)

--- Checks if an array table includes a value.
-- @function utils.includes
-- @usage utils.includes(val)(t)
-- @usage utils.includes(2)({1, 2, 3}) --> true
-- @param val The value to check for
-- @tparam {table<Array>} t The array table to check
-- @treturn {boolean} Whether the value is in the array table
utils.includes = utils.curry(function (val, t)
  assert(type(t) == "table", "argument needs to be a table")
  assert(isArray(t), "argument should be a table that is an array")
  return utils.find(function (v) return v == val end, t) ~= nil
end, 2)

--- Returns the keys of a table.
-- @usage utils.keys(t)
-- @usage utils.keys({name = "Lua", age = 25}) --> {"name", "age"}
-- @tparam {table} t The table to get the keys from
-- @treturn {table<Array>} The keys of the table
utils.keys = function (t)
  assert(type(t) == "table", "argument needs to be a table")
  local keys = {}
  for key in pairs(t) do
    table.insert(keys, key)
  end
  return keys
end

--- Returns the values of a table.
-- @usage utils.values(t)
-- @usage utils.values({name = "Lua", age = 25}) --> {"Lua", 25}
-- @tparam {table} t The table to get the values from
-- @treturn {table<Array>} The values of the table
utils.values = function (t)
  assert(type(t) == "table", "argument needs to be a table")
  local values = {}
  for _, value in pairs(t) do
    table.insert(values, value)
  end
  return values
end

--- Convert a message's tags to a table of key-value pairs
-- @function Tab
-- @tparam {table} msg The message containing tags
-- @treturn {table} A table with tag names as keys and their values
function utils.Tab(msg)
  local inputs = {}
  for _, o in ipairs(msg.Tags) do
    if not inputs[o.name] then
      inputs[o.name] = o.value
    end
  end
  return inputs
end


return utils

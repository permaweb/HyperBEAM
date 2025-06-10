
local function load_json() 
  local json = {_version = "0.2.0"}

-------------------------------------------------------------------------------
-- Encode
-------------------------------------------------------------------------------

local encode

local escape_char_map = {
    ["\\"] = "\\",
    ["\""] = "\"",
    ["\b"] = "b",
    ["\f"] = "f",
    ["\n"] = "n",
    ["\r"] = "r",
    ["\t"] = "t"
}

local escape_char_map_inv = {["/"] = "/"}
for k, v in pairs(escape_char_map) do escape_char_map_inv[v] = k end

local function escape_char(c)
    return "\\" .. (escape_char_map[c] or string.format("u%04x", c:byte()))
end

local function encode_nil(val) return "null" end

local function encode_table(val, stack)
    local res = {}
    stack = stack or {}

    -- Circular reference?
    if stack[val] then error("circular reference") end

    stack[val] = true

    if rawget(val, 1) ~= nil or next(val) == nil then
        -- Treat as array -- check keys are valid and it is not sparse
        local n = 0
        for k in pairs(val) do
            if type(k) ~= "number" then
                error("invalid table: mixed or invalid key types")
            end
            n = n + 1
        end
        if n ~= #val then error("invalid table: sparse array") end
        -- Encode
        for i = 1, #val do res[i] = encode(val[i], stack) end
        stack[val] = nil
        return "[" .. table.concat(res, ",") .. "]"
    else
        -- Treat as an object
        local i = 1
        for k, v in pairs(val) do
            if type(k) ~= "string" then
                error("invalid table: mixed or invalid key types")
            end
            if type(v) ~= "function" then
                res[i] = encode(k, stack) .. ":" .. encode(v, stack)
                i = i + 1
            end
        end
        stack[val] = nil
        return "{" .. table.concat(res, ",") .. "}"
    end
end

local function encode_string(val)
    return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
end

local function encode_number(val)
    -- Check for NaN, -inf and inf
    if val ~= val or val <= -math.huge or val >= math.huge then
        error("unexpected number value '" .. tostring(val) .. "'")
    end
    return string.format("%.14g", val)
end

local type_func_map = {
    ["nil"] = encode_nil,
    ["table"] = encode_table,
    ["string"] = encode_string,
    ["number"] = encode_number,
    ["boolean"] = tostring
}

encode = function(val, stack)
    local t = type(val)
    local f = type_func_map[t]
    if f then return f(val, stack) end
    error("unexpected type '" .. t .. "'")
end

function json.encode(val) return (encode(val)) end

-------------------------------------------------------------------------------
-- Decode
-------------------------------------------------------------------------------

local parse

local function create_set(...)
    local res = {}
    for i = 1, select("#", ...) do res[select(i, ...)] = true end
    return res
end

local space_chars = create_set(" ", "\t", "\r", "\n")
local delim_chars = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
local escape_chars = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
local literals = create_set("true", "false", "null")

local literal_map = {["true"] = true, ["false"] = false, ["null"] = nil}

local function next_char(str, idx, set, negate)
    for i = idx, #str do if set[str:sub(i, i)] ~= negate then return i end end
    return #str + 1
end

local function decode_error(str, idx, msg)
    local line_count = 1
    local col_count = 1
    for i = 1, idx - 1 do
        col_count = col_count + 1
        if str:sub(i, i) == "\n" then
            line_count = line_count + 1
            col_count = 1
        end
    end
    error(string.format("%s at line %d col %d", msg, line_count, col_count))
end

local function codepoint_to_utf8(n)
    local f = math.floor
    if n <= 0x7f then
        return string.char(n)
    elseif n <= 0x7ff then
        return string.char(f(n / 64) + 192, n % 64 + 128)
    elseif n <= 0xffff then
        return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128,
                           n % 64 + 128)
    elseif n <= 0x10ffff then
        return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                           f(n % 4096 / 64) + 128, n % 64 + 128)
    end
    error(string.format("invalid unicode codepoint '%x'", n))
end

local function parse_unicode_escape(s)
    local n1 = tonumber(s:sub(1, 4), 16)
    local n2 = tonumber(s:sub(7, 10), 16)
    if n2 then
        return
            codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
    else
        return codepoint_to_utf8(n1)
    end
end

local function parse_string(str, i)
    local res = {}
    local j = i + 1
    local k = j

    while j <= #str do
        local x = str:byte(j)

        if x < 32 then
            decode_error(str, j, "control character in string")
        elseif x == 92 then -- `\`: Escape
            res[#res + 1] = str:sub(k, j - 1)
            j = j + 1
            local c = str:sub(j, j)
            if c == "u" then
                local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1) or
                                str:match("^%x%x%x%x", j + 1) or
                                decode_error(str, j - 1,
                                             "invalid unicode escape in string")
                res[#res + 1] = parse_unicode_escape(hex)
                j = j + #hex
            else
                if not escape_chars[c] then
                    decode_error(str, j - 1,
                                 "invalid escape char '" .. c .. "' in string")
                end
                res[#res + 1] = escape_char_map_inv[c]
            end
            k = j + 1
        elseif x == 34 then -- `"`: End of string
            res[#res + 1] = str:sub(k, j - 1)
            return table.concat(res), j + 1
        end
        j = j + 1
    end

    decode_error(str, i, "expected closing quote for string")
end

local function parse_number(str, i)
    local x = next_char(str, i, delim_chars)
    local s = str:sub(i, x - 1)
    local n = tonumber(s)
    if not n then decode_error(str, i, "invalid number '" .. s .. "'") end
    return n, x
end

local function parse_literal(str, i)
    local x = next_char(str, i, delim_chars)
    local word = str:sub(i, x - 1)
    if not literals[word] then
        decode_error(str, i, "invalid literal '" .. word .. "'")
    end
    return literal_map[word], x
end

local function parse_array(str, i)
    local res = {}
    local n = 1
    i = i + 1
    while true do
        local x
        i = next_char(str, i, space_chars, true)
        if str:sub(i, i) == "]" then
            i = i + 1
            break
        end
        x, i = parse(str, i)
        res[n] = x
        n = n + 1
        i = next_char(str, i, space_chars, true)
        local chr = str:sub(i, i)
        i = i + 1
        if chr == "]" then break end
        if chr ~= "," then decode_error(str, i, "expected ']' or ','") end
    end
    return res, i
end

local function parse_object(str, i)
    local res = {}
    i = i + 1
    while true do
        local key, val
        i = next_char(str, i, space_chars, true)
        if str:sub(i, i) == "}" then
            i = i + 1
            break
        end
        if str:sub(i, i) ~= '"' then
            decode_error(str, i, "expected string for key")
        end
        key, i = parse(str, i)
        i = next_char(str, i, space_chars, true)
        if str:sub(i, i) ~= ":" then
            decode_error(str, i, "expected ':' after key")
        end
        i = next_char(str, i + 1, space_chars, true)
        val, i = parse(str, i)
        res[key] = val
        i = next_char(str, i, space_chars, true)
        local chr = str:sub(i, i)
        i = i + 1
        if chr == "}" then break end
        if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
    end
    return res, i
end

local char_func_map = {
    ['"'] = parse_string,
    ["0"] = parse_number,
    ["1"] = parse_number,
    ["2"] = parse_number,
    ["3"] = parse_number,
    ["4"] = parse_number,
    ["5"] = parse_number,
    ["6"] = parse_number,
    ["7"] = parse_number,
    ["8"] = parse_number,
    ["9"] = parse_number,
    ["-"] = parse_number,
    ["t"] = parse_literal,
    ["f"] = parse_literal,
    ["n"] = parse_literal,
    ["["] = parse_array,
    ["{"] = parse_object
}

parse = function(str, idx)
    local chr = str:sub(idx, idx)
    local f = char_func_map[chr]
    if f then return f(str, idx) end
    decode_error(str, idx, "unexpected character '" .. chr .. "'")
end

function json.decode(str)
    if type(str) ~= "string" then
        error("expected argument of type string, got " .. type(str))
    end
    local res, idx = parse(str, next_char(str, 1, space_chars, true))
    idx = next_char(str, idx, space_chars, true)
    if idx <= #str then decode_error(str, idx, "trailing garbage") end
    return res
end

return json

end
_G.package.loaded[".json"] = load_json()
print("loaded json")
  


local function load_stringify() 
  --- The Stringify module provides utilities for formatting and displaying Lua tables in a more readable manner. Returns the stringify table.
-- @module stringify

--- The stringify table
-- @table stringify
-- @field _version The version number of the stringify module
-- @field isSimpleArray The isSimpleArray function
-- @field format The format function
local stringify = { _version = "0.0.1" }

-- ANSI color codes
local colors = {
  red = "\27[31m",
  green = "\27[32m",
  blue = "\27[34m",
  reset = "\27[0m"
}

--- Checks if a table is a simple array (i.e., an array with consecutive numeric keys starting from 1).
-- @function isSimpleArray
-- @tparam {table} tbl The table to check
-- @treturn {boolean} Whether the table is a simple array
function stringify.isSimpleArray(tbl)
  local arrayIndex = 1
  for k, v in pairs(tbl) do
    if k ~= arrayIndex or (type(v) ~= "number" and type(v) ~= "string") then
      return false
    end
    arrayIndex = arrayIndex + 1
  end
  return true
end

--- Formats a table for display, handling circular references and formatting strings and tables recursively.
-- @function format
-- @tparam {table} tbl The table to format
-- @tparam {number} indent The indentation level (default is 0)
-- @tparam {table} visited A table to track visited tables and detect circular references (optional)
-- @treturn {string} A string representation of the table
function stringify.format(tbl, indent, visited)
  indent = indent or 0
  local toIndent = string.rep(" ", indent)
  local toIndentChild = string.rep(" ", indent + 2)

  local result = {}
  local isArray = true
  local arrayIndex = 1

  if stringify.isSimpleArray(tbl) then
    for _, v in ipairs(tbl) do
      if type(v) == "string" then
        v = colors.green .. '"' .. v .. '"' .. colors.reset
      else
        v = colors.blue .. tostring(v) .. colors.reset
      end
      table.insert(result, v)
    end
    return "{ " .. table.concat(result, ", ") .. " }"
  end

  for k, v in pairs(tbl) do
    if isArray then
      if k == arrayIndex then
        arrayIndex = arrayIndex + 1
        if type(v) == "table" then
          v = stringify.format(v, indent + 2)
        elseif type(v) == "string" then
          v = colors.green .. '"' .. v .. '"' .. colors.reset
        else
          v = colors.blue .. tostring(v) .. colors.reset
        end
        table.insert(result, toIndentChild .. v)
      else
        isArray = false
        result = {}
      end
    end
    if not isArray then
      if type(v) == "table" then
        visited = visited or {}
        if visited[v] then
            return "<circular reference>"
        end
        visited[v] = true

        v = stringify.format(v, indent + 2, visited)
      elseif type(v) == "string" then
        v = colors.green .. '"' .. v .. '"' .. colors.reset
      else
        v = colors.blue .. tostring(v) .. colors.reset
      end
      k = colors.red .. k .. colors.reset
      table.insert(result, toIndentChild .. k .. " = " .. v)
    end
  end

  local prefix = isArray and "{\n" or "{\n "
  local suffix = isArray and "\n" .. toIndent .. " }" or "\n" .. toIndent .. "}"
  local separator = isArray and ",\n" or ",\n "
  return prefix .. table.concat(result, separator) .. suffix
end

return stringify

end
_G.package.loaded[".stringify"] = load_stringify()
print("loaded stringify")
  


local function load_eval() 
  --- The Eval module provides a handler for evaluating Lua expressions. Returns the eval function.
-- @module eval

local stringify = require(".stringify")
local json = require('.json')
--- The eval function.
-- Handler for executing and evaluating Lua expressions.
-- After execution, the result is stringified and placed in ao.outbox.Output.
-- @function eval
-- @tparam {table} ao The ao environment object
-- @treturn {function} The handler function, which takes a message as an argument.
-- @see stringify
return function (ao)
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
      ao.outbox.Error = err
      return
    end
    if e then
      ao.outbox.Error = e
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
    --   ao.outbox.Output = {
    --     data = type(output) == "table" 
    --       and stringify.format(output) or tostring(output),
    --     prompt = Prompt()
    --   }
    --
    end
  end
end

end
_G.package.loaded[".eval"] = load_eval()
print("loaded eval")
  


local function load_utils() 
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

end
_G.package.loaded[".utils"] = load_utils()
print("loaded utils")
  


local function load_handlers_utils() 
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

local _ = require('.utils')

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

end
_G.package.loaded[".handlers-utils"] = load_handlers_utils()
print("loaded handlers-utils")
  


local function load_handlers() 
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
local utils = require('.utils')

handlers.utils = require('.handlers-utils')
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

end
_G.package.loaded[".handlers"] = load_handlers()
print("loaded handlers")
  


local function load_dump() 
  --
-- Copyright (C) 2018 Masatoshi Teruya
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
--
-- dump.lua
-- lua-dump
-- Created by Masatoshi Teruya on 18/04/22.
--
--- file-scope variables
local type = type
local floor = math.floor
local tostring = tostring
local tblsort = table.sort
local tblconcat = table.concat
local strmatch = string.match
local strformat = string.format
--- constants
local INFINITE_POS = math.huge
local LUA_FIELDNAME_PAT = '^[a-zA-Z_][a-zA-Z0-9_]*$'
local FOR_KEY = 'key'
local FOR_VAL = 'val'
local FOR_CIRCULAR = 'circular'
local RESERVED_WORD = {
    -- primitive data
    ['nil'] = true,
    ['true'] = true,
    ['false'] = true,
    -- declaraton
    ['local'] = true,
    ['function'] = true,
    -- boolean logic
    ['and'] = true,
    ['or'] = true,
    ['not'] = true,
    -- conditional statement
    ['if'] = true,
    ['elseif'] = true,
    ['else'] = true,
    -- iteration statement
    ['for'] = true,
    ['in'] = true,
    ['while'] = true,
    ['until'] = true,
    ['repeat'] = true,
    -- jump statement
    ['break'] = true,
    ['goto'] = true,
    ['return'] = true,
    -- block scope statement
    ['then'] = true,
    ['do'] = true,
    ['end'] = true,
}
local DEFAULT_INDENT = 4

--- filter function for dump
--- @param val any
--- @param depth integer
--- @param vtype string
--- @param use string
--- @param key any
--- @param udata any
--- @return any val
--- @return boolean nodump
local function DEFAULT_FILTER(val)
    return val
end

--- sort_index
--- @param a table
--- @param b table
local function sort_index(a, b)
    if a.typ == b.typ then
        if a.typ == 'boolean' then
            return b.key
        end

        return a.key < b.key
    end

    return a.typ == 'number'
end

--- dumptbl
--- @param tbl table
--- @param depth integer
--- @param indent string
--- @param nestIndent string
--- @param ctx table
--- @return string
local function dumptbl(tbl, depth, indent, nestIndent, ctx)
    local ref = tostring(tbl)

    -- circular reference
    if ctx.circular[ref] then
        local val, nodump = ctx.filter(tbl, depth, type(tbl), FOR_CIRCULAR, tbl,
                                       ctx.udata)

        if val ~= nil and val ~= tbl then
            local t = type(val)

            if t == 'table' then
                -- dump table value
                if not nodump then
                    return dumptbl(val, depth + 1, indent, nestIndent, ctx)
                end
                return tostring(val)
            elseif t == 'string' then
                return strformat('%q', val)
            elseif t == 'number' or t == 'boolean' then
                return tostring(val)
            end

            return strformat('%q', tostring(val))
        end

        return '"<Circular ' .. ref .. '>"'
    end

    local res = {}
    local arr = {}
    local narr = 0
    local fieldIndent = indent .. nestIndent

    -- save reference
    ctx.circular[ref] = true

    for k, v in pairs(tbl) do
        -- check key
        local key, nokdump = ctx.filter(k, depth, type(k), FOR_KEY, nil,
                                        ctx.udata)

        if key ~= nil then
            -- check val
            local val, novdump = ctx.filter(v, depth, type(v), FOR_VAL, key,
                                            ctx.udata)
            local kv

            if val ~= nil then
                local kt = type(key)
                local vt = type(val)

                -- convert key to suitable to be safely read back
                -- by the Lua interpreter
                if kt == 'number' or kt == 'boolean' then
                    k = key
                    key = '[' .. tostring(key) .. ']'
                    -- dump table value
                elseif kt == 'table' and not nokdump then
                    key = '[' ..
                              dumptbl(key, depth + 1, fieldIndent, nestIndent,
                                      ctx) .. ']'
                    k = key
                    kt = 'string'
                elseif kt ~= 'string' or RESERVED_WORD[key] or
                    not strmatch(key, LUA_FIELDNAME_PAT) then
                    key = strformat("[%q]", tostring(key), v)
                    k = key
                    kt = 'string'
                end

                -- convert key-val pair to suitable to be safely read back
                -- by the Lua interpreter
                if vt == 'number' or vt == 'boolean' then
                    kv = strformat('%s%s = %s', fieldIndent, key, tostring(val))
                elseif vt == 'string' then
                    -- dump a string-value
                    if not novdump then
                        kv = strformat('%s%s = %q', fieldIndent, key, val)
                    else
                        kv = strformat('%s%s = %s', fieldIndent, key, val)
                    end
                elseif vt == 'table' and not novdump then
                    kv = strformat('%s%s = %s', fieldIndent, key, dumptbl(val,
                                                                          depth +
                                                                              1,
                                                                          fieldIndent,
                                                                          nestIndent,
                                                                          ctx))
                else
                    kv = strformat('%s%s = %q', fieldIndent, key, tostring(val))
                end

                -- add to array
                narr = narr + 1
                arr[narr] = {
                    typ = kt,
                    key = k,
                    val = kv,
                }
            end
        end
    end

    -- remove reference
    ctx.circular[ref] = nil
    -- concat result
    if narr > 0 then
        tblsort(arr, sort_index)

        for i = 1, narr do
            res[i] = arr[i].val
        end
        res[1] = '{' .. ctx.LF .. res[1]
        res = tblconcat(res, ',' .. ctx.LF) .. ctx.LF .. indent .. '}'
    else
        res = '{}'
    end

    return res
end

--- is_uint
--- @param v any
--- @return boolean ok
local function is_uint(v)
    return type(v) == 'number' and v < INFINITE_POS and v >= 0 and floor(v) == v
end

--- dump
--- @param val any
--- @param indent integer
--- @param padding integer
--- @param filter function
--- @param udata
--- @return string
local function dump(val, indent, padding, filter, udata)
    local t = type(val)

    -- check indent
    if indent == nil then
        indent = DEFAULT_INDENT
    elseif not is_uint(indent) then
        error('indent must be unsigned integer', 2)
    end

    -- check padding
    if padding == nil then
        padding = 0
    elseif not is_uint(padding) then
        error('padding must be unsigned integer', 2)
    end

    -- check filter
    if filter == nil then
        filter = DEFAULT_FILTER
    elseif type(filter) ~= 'function' then
        error('filter must be function', 2)
    end

    -- dump table
    if t == 'table' then
        local ispace = ''
        local pspace = ''

        if indent > 0 then
            ispace = strformat('%' .. tostring(indent) .. 's', '')
        end

        if padding > 0 then
            pspace = strformat('%' .. tostring(padding) .. 's', '')
        end

        return dumptbl(val, 1, pspace, ispace, {
            LF = ispace == '' and ' ' or '\n',
            circular = {},
            filter = filter,
            udata = udata,
        })
    end

    -- dump value
    local v, nodump = filter(val, 0, t, FOR_VAL, nil, udata)
    if nodump == true then
        return tostring(v)
    end
    return strformat('%q', tostring(v))
end

return dump
end
_G.package.loaded[".dump"] = load_dump()
print("loaded dump")
  


local function load_pretty() 
  local pretty = { _version = "0.0.1" }

pretty.tprint = function (tbl, indent)
  if not indent then indent = 0 end
  local output = ""
  for k, v in pairs(tbl) do
    local formatting = string.rep(" ", indent) .. k .. ": "
    if type(v) == "table" then
      output = output .. formatting .. "\n"
      output = output .. pretty.tprint(v, indent+1)
    elseif type(v) == 'boolean' then
      output = output .. formatting .. tostring(v) .. "\n"
    else
      output = output .. formatting .. v .. "\n"
    end
  end
  return output
end

return pretty

end
_G.package.loaded[".pretty"] = load_pretty()
print("loaded pretty")
  


local function load_chance() 
  --- The Chance module provides utilities for generating random numbers and values. Returns the chance table.
-- @module chance

local N = 624
local M = 397
local MATRIX_A = 0x9908b0df
local UPPER_MASK = 0x80000000
local LOWER_MASK = 0x7fffffff

--- Initializes mt[N] with a seed
-- @lfunction init_genrand
-- @tparam {table} o The table to initialize
-- @tparam {number} s The seed
local function init_genrand(o, s)
    o.mt[0] = s & 0xffffffff
    for i = 1, N - 1 do
        o.mt[i] = (1812433253 * (o.mt[i - 1] ~ (o.mt[i - 1] >> 30))) + i
        -- See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier.
        -- In the previous versions, MSBs of the seed affect
        -- only MSBs of the array mt[].
        -- 2002/01/09 modified by Makoto Matsumoto
        o.mt[i] = o.mt[i] & 0xffffffff
        -- for >32 bit machines
    end
    o.mti = N
end

--- Generates a random number on [0,0xffffffff]-interval
-- @lfunction genrand_int32
-- @tparam {table} o The table to generate the random number from
-- @treturn {number} The random number
local function genrand_int32(o)
    local y
    local mag01 = {} -- mag01[x] = x * MATRIX_A  for x=0,1
    mag01[0] = 0x0
    mag01[1] = MATRIX_A
    if o.mti >= N then  -- generate N words at one time
        if o.mti == N + 1 then -- if init_genrand() has not been called,
            init_genrand(o, 5489)   -- a default initial seed is used
        end
        for kk = 0, N - M - 1 do
            y = (o.mt[kk] & UPPER_MASK) | (o.mt[kk + 1] & LOWER_MASK)
            o.mt[kk] = o.mt[kk + M] ~ (y >> 1) ~ mag01[y & 0x1]
        end
        for kk = N - M, N - 2 do
            y = (o.mt[kk] & UPPER_MASK) | (o.mt[kk + 1] & LOWER_MASK)
            o.mt[kk] = o.mt[kk + (M - N)] ~ (y >> 1) ~ mag01[y & 0x1]
        end
        y = (o.mt[N - 1] & UPPER_MASK) | (o.mt[0] & LOWER_MASK)
        o.mt[N - 1] = o.mt[M - 1] ~ (y >> 1) ~ mag01[y & 0x1]

        o.mti = 0
    end

    y = o.mt[o.mti]
    o.mti = o.mti + 1

    -- Tempering
    y = y ~ (y >> 11)
    y = y ~ ((y << 7) & 0x9d2c5680)
    y = y ~ ((y << 15) & 0xefc60000)
    y = y ~ (y >> 18)

    return y
end

local MersenneTwister = {}
MersenneTwister.mt = {}
MersenneTwister.mti = N + 1


--- The Random table
-- @table Random
-- @field seed The seed function
-- @field random The random function
-- @field integer The integer function
local Random = {}

--- Sets a new random table given a seed.
-- @function seed
-- @tparam {number} seed The seed
function Random.seed(seed)
    init_genrand(MersenneTwister, seed)
end

--- Generates a random number on [0,1)-real-interval.
-- @function random
-- @treturn {number} The random number
function Random.random()
    return genrand_int32(MersenneTwister) * (1.0 / 4294967296.0)
end

--- Returns a random integer. The min and max are INCLUDED in the range.
-- The max integer in lua is math.maxinteger
-- The min is math.mininteger
-- @function Random.integer
-- @tparam {number} min The minimum value
-- @tparam {number} max The maximum value
-- @treturn {number} The random integer
function Random.integer(min, max)
    assert(max >= min, "max must bigger than min")
    return math.floor(Random.random() * (max - min + 1) + min)
end

return Random
end
_G.package.loaded[".chance"] = load_chance()
print("loaded chance")
  


local function load_boot() 
  --- The Boot module provides functionality for booting the process. Returns the boot function.
-- @module boot

-- This is for aop6 Boot Loader
-- See: https://github.com/permaweb/aos/issues/342
-- For the Process as the first Message, if On-Boot
-- has the value 'data' then evaluate the data
-- if it is a tx id, then download and evaluate the tx

local drive = { _version = "0.0.1" }

function drive.getData(txId)
  local file = io.open('/data/' .. txId)
  if not file then
    return nil, "File not found!"
  end
  local contents = file:read(
    file:seek('end')
  )
  file:close()
  return contents
end

--- The boot function.
-- If the message has no On-Boot tag, do nothing.
-- If the message has an On-Boot tag with the value 'Data', then evaluate the message.
-- If the message has an On-Boot tag with a tx id, then download and evaluate the tx data.
-- @function boot
-- @param ao The ao environment object
-- @see eval
return function (ao)
  local eval = require(".eval")(ao)
  return function (msg)
    if #Inbox == 0 then
      table.insert(Inbox, msg)
    end
    if msg.Tags['On-Boot'] == nil then
      return
    end
    if msg.Tags['On-Boot'] == 'Data' then
      eval(msg)
    else
      local loadedVal = drive.getData(msg.Tags['On-Boot'])
      eval({ Data = loadedVal })
    end
  end
end
end
_G.package.loaded[".boot"] = load_boot()
print("loaded boot")
  


local function load_default() 
  local json = require('.json')
-- default handler for aos
return function (insertInbox)
  return function (msg)
    -- Add Message to Inbox
    insertInbox(msg)

    -- local txt = Colors.gray .. "New Message From " .. Colors.green .. 
    -- (msg.From and (msg.From:sub(1,3) .. "..." .. msg.From:sub(-3)) or "unknown") .. Colors.gray .. ": "
    -- if msg.Action then
    --   txt = txt .. Colors.gray .. (msg.Action and ("Action = " .. Colors.blue .. msg.Action:sub(1,20)) or "") .. Colors.reset
    -- else
    --   local data = msg.Data
    --   if type(data) == 'table' then
    --     data = json.encode(data)
    --   end
    --   txt = txt .. Colors.gray .. "Data = " .. Colors.blue .. (data and data:sub(1,20) or "") .. Colors.reset
    -- end
    -- Print to Output
    -- print(txt)
    print("New Message")
  end

end

end
_G.package.loaded[".default"] = load_default()
print("loaded default")
  


local function load_ao() 
  Handlers = Handlers or require('.handlers')

local oldao = ao or {}

local utils = require('.utils')

local ao = {
    _version = "0.0.6",
    id = oldao.id or "",
    _module = oldao._module or "",
    authorities = oldao.authorities or {},
    reference = oldao.reference or 0,
    outbox = oldao.outbox or
        {Output = {}, Messages = {}, Spawns = {}, Assignments = {}},
    nonExtractableTags = {
        'data-protocol', 'variant', 'from-process', 'from-module', 'type',
        'from', 'owner', 'anchor', 'target', 'data', 'tags', 'read-only'
    },
    nonForwardableTags = {
        'data-protocol', 'variant', 'from-process', 'from-module', 'type',
        'from', 'owner', 'anchor', 'target', 'tags', 'tagArray', 'hash-chain',
        'timestamp', 'nonce', 'slot', 'epoch', 'signature', 'forwarded-by',
        'pushed-for', 'read-only', 'cron', 'block-height', 'reference', 'id',
        'reply-to'
    },
    Nonce = nil
}

function ao.clearOutbox()
  ao.outbox = { Output = {}, Messages = {}, Spawns = {}, Assignments = {}}
end

local function getId(m)
  local id = ""
  utils.map(function (k)
    local c = m.commitments[k]
    if c.alg == "rsa-pss-sha512" then
      id = k
    elseif c.alg == "signed" and c['commitment-device'] == "ans104" then
      id = k
    end
  end, utils.keys(m.commitments)
  )
  return id
end

function ao.init(env)
  if ao.id == "" then ao.id = getId(env.process) end

  -- if ao._module == "" then
  --   ao._module = env.Module.Id
  -- end
  -- TODO: need to deal with assignables
  if #ao.authorities < 1 then
      if type(env.process.authority) == 'string' then
        ao.authorities = { env.process.authority }
      else
        ao.authorities = env.process.authority
      end
  end

  ao.outbox = {Output = {}, Messages = {}, Spawns = {}, Assignments = {}}
  ao.env = env

end

function ao.send(msg)
  assert(type(msg) == 'table', 'msg should be a table')

  ao.reference = ao.reference + 1
  local referenceString = tostring(ao.reference)
  -- set kv
  msg.reference = referenceString

  -- clone message info and add to outbox
  table.insert(ao.outbox.Messages, utils.reduce(
    function (acc, key)
      acc[key] = msg[key]
      return acc
    end,
    {},
    utils.keys(msg)
  ))

  if msg.target then
    msg.onReply = function(...)
      local from, resolver
      if select("#", ...) == 2 then
        from = select(1, ...)
        resolver = select(2, ...)
      else
        from = msg.target
        resolver = select(1, ...)
      end
      Handlers.once({
        from = from,
        ["x-reference"] = referenceString
      }, resolver)
    end
  end
  return msg
end

function ao.spawn(module, msg)
  assert(type(module) == "string", "Module source id is required!")
  assert(type(msg) == "table", "Message must be a table.")

  ao.reference = ao.reference + 1

  local spawnRef = tostring(ao.reference)

  msg["reference"] = spawnRef

  -- clone message info and add to outbox
  table.insert(ao.outbox.Spawns, utils.reduce(
    function (acc, key)
      acc[key] = msg[key]
      return acc
    end,
    {},
    utils.keys(msg)
  ))

  msg.onReply = function(cb)
    Handlers.once({
      action = "Spawned",
      from = ao.id,
      ["x-reference"] = spawnRef
    }, cb)
  end

  return msg

end

function ao.result(result)
  if ao.outbox.Error or result.Error then
    return { Error = result.Error or ao.outbox.Error }
  end
  return {
    Output = result.Output or ao.output.Output,
    Messages = ao.outbox.Messages,
    Spawns = ao.outbox.Spawns,
    Assignments = ao.outbox.Assignments
  }
end

-- set global Send and Spawn
Send = Send or ao.send
Spawn = Spawn or ao.spawn

return ao

end
_G.package.loaded[".ao"] = load_ao()
print("loaded ao")
  


local function load_base64() 
  --[[

 base64 -- v1.5.3 public domain Lua base64 encoder/decoder
 no warranty implied; use at your own risk

 Needs bit32.extract function. If not present it's implemented using BitOp
 or Lua 5.3 native bit operators. For Lua 5.1 fallbacks to pure Lua
 implementation inspired by Rici Lake's post:
   http://ricilake.blogspot.co.uk/2007/10/iterating-bits-in-lua.html

 author: Ilya Kolbin (iskolbin@gmail.com)
 url: github.com/iskolbin/lbase64

 COMPATIBILITY

 Lua 5.1+, LuaJIT

 LICENSE

 See end of file for license information.

--]]


local base64 = {}

local extract = _G.bit32 and _G.bit32.extract -- Lua 5.2/Lua 5.3 in compatibility mode
if not extract then
	if _G.bit then -- LuaJIT
		local shl, shr, band = _G.bit.lshift, _G.bit.rshift, _G.bit.band
		extract = function( v, from, width )
			return band( shr( v, from ), shl( 1, width ) - 1 )
		end
	elseif _G._VERSION == "Lua 5.1" then
		extract = function( v, from, width )
			local w = 0
			local flag = 2^from
			for i = 0, width-1 do
				local flag2 = flag + flag
				if v % flag2 >= flag then
					w = w + 2^i
				end
				flag = flag2
			end
			return w
		end
	else -- Lua 5.3+
		extract = load[[return function( v, from, width )
			return ( v >> from ) & ((1 << width) - 1)
		end]]()
	end
end


function base64.makeencoder( s62, s63, spad )
	local encoder = {}
	for b64code, char in pairs{[0]='A','B','C','D','E','F','G','H','I','J',
		'K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y',
		'Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n',
		'o','p','q','r','s','t','u','v','w','x','y','z','0','1','2',
		'3','4','5','6','7','8','9',s62 or '+',s63 or'/',spad or'='} do
		encoder[b64code] = char:byte()
	end
	return encoder
end

function base64.makedecoder( s62, s63, spad )
	local decoder = {}
	for b64code, charcode in pairs( base64.makeencoder( s62, s63, spad )) do
		decoder[charcode] = b64code
	end
	return decoder
end

local DEFAULT_ENCODER = base64.makeencoder()
local DEFAULT_DECODER = base64.makedecoder()

local char, concat = string.char, table.concat

function base64.encode( str, encoder, usecaching )
	encoder = encoder or DEFAULT_ENCODER
	local t, k, n = {}, 1, #str
	local lastn = n % 3
	local cache = {}
	for i = 1, n-lastn, 3 do
		local a, b, c = str:byte( i, i+2 )
		local v = a*0x10000 + b*0x100 + c
		local s
		if usecaching then
			s = cache[v]
			if not s then
				s = char(encoder[extract(v,18,6)], encoder[extract(v,12,6)], encoder[extract(v,6,6)], encoder[extract(v,0,6)])
				cache[v] = s
			end
		else
			s = char(encoder[extract(v,18,6)], encoder[extract(v,12,6)], encoder[extract(v,6,6)], encoder[extract(v,0,6)])
		end
		t[k] = s
		k = k + 1
	end
	if lastn == 2 then
		local a, b = str:byte( n-1, n )
		local v = a*0x10000 + b*0x100
		t[k] = char(encoder[extract(v,18,6)], encoder[extract(v,12,6)], encoder[extract(v,6,6)], encoder[64])
	elseif lastn == 1 then
		local v = str:byte( n )*0x10000
		t[k] = char(encoder[extract(v,18,6)], encoder[extract(v,12,6)], encoder[64], encoder[64])
	end
	return concat( t )
end

function base64.decode( b64, decoder, usecaching )
	decoder = decoder or DEFAULT_DECODER
	local pattern = '[^%w%+%/%=]'
	if decoder then
		local s62, s63
		for charcode, b64code in pairs( decoder ) do
			if b64code == 62 then s62 = charcode
			elseif b64code == 63 then s63 = charcode
			end
		end
		pattern = ('[^%%w%%%s%%%s%%=]'):format( char(s62), char(s63) )
	end
	b64 = b64:gsub( pattern, '' )
	local cache = usecaching and {}
	local t, k = {}, 1
	local n = #b64
	local padding = b64:sub(-2) == '==' and 2 or b64:sub(-1) == '=' and 1 or 0
	for i = 1, padding > 0 and n-4 or n, 4 do
		local a, b, c, d = b64:byte( i, i+3 )
		local s
		if usecaching then
			local v0 = a*0x1000000 + b*0x10000 + c*0x100 + d
			s = cache[v0]
			if not s then
				local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40 + decoder[d]
				s = char( extract(v,16,8), extract(v,8,8), extract(v,0,8))
				cache[v0] = s
			end
		else
			local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40 + decoder[d]
			s = char( extract(v,16,8), extract(v,8,8), extract(v,0,8))
		end
		t[k] = s
		k = k + 1
	end
	if padding == 1 then
		local a, b, c = b64:byte( n-3, n-1 )
		local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40
		t[k] = char( extract(v,16,8), extract(v,8,8))
	elseif padding == 2 then
		local a, b = b64:byte( n-3, n-2 )
		local v = decoder[a]*0x40000 + decoder[b]*0x1000
		t[k] = char( extract(v,16,8))
	end
	return concat( t )
end

return base64

--[[
------------------------------------------------------------------------------
This software is available under 2 licenses -- choose whichever you prefer.
------------------------------------------------------------------------------
ALTERNATIVE A - MIT License
Copyright (c) 2018 Ilya Kolbin
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
------------------------------------------------------------------------------
ALTERNATIVE B - Public Domain (www.unlicense.org)
This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.
In jurisdictions that recognize copyright laws, the author or authors of this
software dedicate any and all copyright interest in the software to the public
domain. We make this dedication for the benefit of the public at large and to
the detriment of our heirs and successors. We intend this dedication to be an
overt act of relinquishment in perpetuity of all present and future rights to
this software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
------------------------------------------------------------------------------
--]]
end
_G.package.loaded[".base64"] = load_base64()
print("loaded base64")
  


local function load_state() 
  ao = ao or require('.ao')
local state = {}
local stringify = require('.stringify')
local utils = require('.utils')

Colors = { red = "\27[31m", green = "\27[32m",
  blue = "\27[34m", reset = "\27[0m", gray = "\27[90m"
}
Bell = "\x07"

Initialized = Initialized or false
Name = Name or "aos"

Owner = Owner or ""
Inbox = Inbox or {}

-- global prompt function
function Prompt()
  return "aos> "
end

local maxInboxCount = 10000

function state.insertInbox(msg)
  table.insert(Inbox, msg)
  local overflow = #Inbox - maxInboxCount
  for i = 1,overflow do
    table.remove(Inbox,1)
  end
end
local function getOwnerAddress(m)
  local _owner = nil 
  utils.map(function (k)
    local c = m.commitments[k]
    if c.alg == "rsa-pss-sha512" then
      _owner = c.committer
    elseif c.alg == "signed" and c['commitment-device'] == "ans104" then
      _owner = c.commiter
    end
  end, utils.keys(m.commitments))
  return _owner
end

local function isFromOwner(m)
  local _owner = getOwnerAddress(m)
  local _fromProcess = m['from-process'] or _owner
  return _owner ~= nil and _fromProcess == _owner
end

local function getOwner(m)
  local id = ""
  if m['from-process'] then
    return m['from-process']
  end

  utils.map(function (k)
    local c = m.commitments[k]
    if c.alg == "rsa-pss-sha512" then
      id = c.committer
    elseif c.alg == "signed" and c['commitment-device'] == "ans104" then
      id = c.committer
    end
  end, utils.keys(m.commitments)
  )
  return id
end

function state.init(req, base)
  if not Initialized then
    Owner = getOwner(base.process)
    -- if process id is equal to message id then set Owner
    -- TODO: need additional check, like msg.Slot == 1
    -- if env.Process.Id == msg.Id and Owner ~= msg.Id then
    --   Owner = env.Process['From-Process'] or msg.From
    -- end
    -- if env.Process.Name then
    --   Name = Name == "aos" and env.Process.Name
    -- end
    -- global print function
    function print(a)
      if type(a) == "table" then
        a = stringify.format(a)
      end

      if type(a) == "boolean" then
        a = Colors.blue .. tostring(a) .. Colors.reset
      end
      if type(a) == "nil" then
        a = Colors.red .. tostring(a) .. Colors.reset
      end
      if type(a) == "number" then
        a = Colors.green .. tostring(a) .. Colors.reset
      end

      if HandlerPrintLogs then
        table.insert(HandlerPrintLogs, a)
        return nil
      end

      return tostring(a)
    end

    Initialized = true
  end
end

function state.getFrom(req)
  return getOwner(req.body)
end

function state.isTrusted(req)
  if isFromOwner(req.body) then
    return true
  end
  local _trusted = false

  if req.body['from-process'] then
    _trusted = utils.includes(
      req.body['from-process'],
      ao.authorities
    )
  end

  if not _trusted then
    _trusted = utils.includes(
      getOwner(req.body), ao.authorities
    )
  end
  return _trusted
end

function state.checkSlot(req, ao)
  -- slot check
  if not ao.slot then
    ao.slot = tonumber(req.slot)
  else
    if tonumber(req.slot) ~= (ao.slot + 1) then
      print(table.concat({
      Colors.red,
      "WARNING: Slot did not match, may be due to an error generated by process",
      Colors.reset
      }))
      print("")
    end
  end
end

function state.reset(tbl)
  tbl = nil
  collectgarbage()
  return {}
end

return state

end
_G.package.loaded[".state"] = load_state()
print("loaded state")
  


local function load_process() 
  ao = ao or require('.ao')
Handlers = require('.handlers')
Utils = require('.utils')
Dump = require('.dump')

local process = { _version = "2.0.7" }
local state = require('.state')
local eval = require('.eval')
local default = require('.default')
local json = require('.json')

function Prompt()
  return "aos> "
end

function process.handle(req, base)
  HandlerPrintLogs = state.reset(HandlerPrintLogs)
  os.time = function () return tonumber(req['block-timestamp']) end

  ao.init(base)
  -- initialize state
  state.init(req, base)


  -- magic table
  req.body.data = req.body['Content-Type'] == 'application/json'
    and json.decode(req.body.data or "{}")
    or req.body.data

  Errors = Errors or {}
  -- clear outbox
  ao.clearOutbox()

  if not state.isTrusted(req) then
    return ao.result({
      Output = {
        data = "Message is not trusted."
      }
    })
  end

  req.reply = function (_reply)
    local _from = state.getFrom(req)
    _reply.target = _reply.target and _reply.target or _from
    _reply['x-reference'] = req.body.reference or nil
    _reply['x-origin'] = req.body['x-origin'] or nil
    return ao.send(_reply)
  end


  -- state.checkSlot(msg, ao)
  Handlers.add("_eval", function (_req)
    local function getMsgFrom(m)
      local from = ""
      Utils.map(
        function (k)
          local c = m.commitments[k]
          if c.alg == "rsa-pss-sha512" then
            from = c.committer
          end
        end,
        Utils.keys(m.commitments)
      )
      return from
    end
    return _req.body.action == "Eval" and Owner == getMsgFrom(_req.body)
  end, eval(ao))

  Handlers.add("_default",
    function () return true end,
    default(state.insertInbox)
  )

  local status, error = pcall(Handlers.evaluate, req, base)

  -- cleanup handlers so that they are always at the end of the pipeline
  Handlers.remove("_eval")
  Handlers.remove("_default")

  local printData = table.concat(HandlerPrintLogs, "\n")
  if not status then
    if req.body.action == "Eval" then
      return {
        Error = table.concat({
          printData,
          "\n",
          Colors.red,
          "error: " .. error,
          Colors.reset,
        })
      }
    end
    print(Colors.red .. "Error" .. Colors.gray .. " handling message " .. Colors.reset)
    print(Colors.green .. error .. Colors.reset)
    -- print("\n" .. Colors.gray .. debug.traceback() .. Colors.reset)
    return ao.result({
      Output = {
        data = printData .. '\n\n' .. Colors.red .. 'error:\n' .. Colors.reset .. error
      },
      Messages = {},
      Spawns = {},
      Assignments = {}
    })
  end

  local response = {}

  if req.body.action == "Eval" then
    response = ao.result({
      Output = {
        data = printData,
        prompt = Prompt()
      }
    })
  else
    response = ao.result({
      Output = {
        data = printData,
        prompt = Prompt(),
        print = true
      }
    })
  end

  HandlerPrintLogs = state.reset(HandlerPrintLogs) -- clear logs
  -- ao.Slot = msg.Slot
  return response
end

function Version()
  print("version: " .. process._version)
end

return process

end
_G.package.loaded[".process"] = load_process()
print("loaded process")
  

ao = require('.ao')
local _process = require('.process')

function compute(base, req, opts)
  local _results = _process.handle(req, base)
  base.results = {
    outbox = {},
    output = _results.Output
  }
  for i=1,#_results.Messages do
    base.results.outbox[tostring(i)] = _results.Messages[i]
  end
  return base
end


print [[     _    ___  ____  
    / \  / _ \/ ___| 
   / _ \| | | \___ \ 
  / ___ \ |_| |___) |
 /_/   \_\___/|____/ 
                     ]]

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

_G.package.loaded[".stringify"] = stringify




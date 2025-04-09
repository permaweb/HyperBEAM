
local function load_ao() 
  --- The AO module provides functionality for managing the AO environment and handling messages. Returns the ao table.
-- @module ao

local oldao = ao or {}

--- The AO module
-- @table ao
-- @field _version The version number of the ao module
-- @field _module The module id of the process
-- @field id The id of the process
-- @field authorities A table of authorities of the process
-- @field reference The reference number of the process
-- @field outbox The outbox of the process
-- @field nonExtractableTags The non-extractable tags
-- @field nonForwardableTags The non-forwardable tags
-- @field clone The clone function
-- @field normalize The normalize function
-- @field sanitize The sanitize function
-- @field init The init function
-- @field log The log function
-- @field clearOutbox The clearOutbox function
-- @field send The send function
-- @field spawn The spawn function
-- @field assign The assign function
-- @field isTrusted The isTrusted function
-- @field result The result function
local ao = {
    _version = "0.0.6",
    id = oldao.id or "",
    _module = oldao._module or "",
    authorities = oldao.authorities or {},
    reference = oldao.reference or 0,
    outbox = oldao.outbox or
        {Output = {}, Messages = {}, Spawns = {}, Assignments = {}},
    nonExtractableTags = {
        'Data-Protocol', 'Variant', 'From-Process', 'From-Module', 'Type',
        'From', 'Owner', 'Anchor', 'Target', 'Data', 'Tags', 'Read-Only'
    },
    nonForwardableTags = {
        'Data-Protocol', 'Variant', 'From-Process', 'From-Module', 'Type',
        'From', 'Owner', 'Anchor', 'Target', 'Tags', 'TagArray', 'Hash-Chain',
        'Timestamp', 'Nonce', 'Epoch', 'Signature', 'Forwarded-By',
        'Pushed-For', 'Read-Only', 'Cron', 'Block-Height', 'Reference', 'Id',
        'Reply-To'
    },
    Nonce = nil
}

--- Checks if a key exists in a list.
-- @lfunction _includes
-- @tparam {table} list The list to check against
-- @treturn {function} A function that takes a key and returns true if the key exists in the list
local function _includes(list)
    return function(key)
        local exists = false
        for _, listKey in ipairs(list) do
            if key == listKey then
                exists = true
                break
            end
        end
        if not exists then return false end
        return true
    end
end

--- Checks if a table is an array.
-- @lfunction isArray
-- @tparam {table} table The table to check
-- @treturn {boolean} True if the table is an array, false otherwise
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

--- Pads a number with leading zeros to 32 digits.
-- @lfunction padZero32
-- @tparam {number} num The number to pad
-- @treturn {string} The padded number as a string
local function padZero32(num) return string.format("%032d", num) end

--- Clones a table recursively.
-- @function clone
-- @tparam {any} obj The object to clone
-- @tparam {table} seen The table of seen objects (default is nil)
-- @treturn {any} The cloned object
function ao.clone(obj, seen)
    -- Handle non-tables and previously-seen tables.
    if type(obj) ~= 'table' then return obj end
    if seen and seen[obj] then return seen[obj] end

    -- New table; mark it as seen and copy recursively.
    local s = seen or {}
    local res = {}
    s[obj] = res
    for k, v in pairs(obj) do res[ao.clone(k, s)] = ao.clone(v, s) end
    return setmetatable(res, getmetatable(obj))
end

--- Normalizes a message by extracting tags.
-- @function normalize
-- @tparam {table} msg The message to normalize
-- @treturn {table} The normalized message
function ao.normalize(msg)
    for _, o in ipairs(msg.Tags) do
        if not _includes(ao.nonExtractableTags)(o.name) then
            msg[o.name] = o.value
        end
    end
    return msg
end

--- Sanitizes a message by removing non-forwardable tags.
-- @function sanitize
-- @tparam {table} msg The message to sanitize
-- @treturn {table} The sanitized message
function ao.sanitize(msg)
    local newMsg = ao.clone(msg)

    for k, _ in pairs(newMsg) do
        if _includes(ao.nonForwardableTags)(k) then newMsg[k] = nil end
    end

    return newMsg
end

--- Initializes the AO environment, including ID, module, authorities, outbox, and environment.
-- @function init
-- @tparam {table} env The environment object
function ao.init(env)
    if ao.id == "" then ao.id = env.Process.Id end

    if ao._module == "" then
        for _, o in ipairs(env.Process.Tags) do
            if o.name == "Module" then ao._module = o.value end
        end
    end

    if #ao.authorities < 1 then
        for _, o in ipairs(env.Process.Tags) do
            if o.name == "Authority" then
                table.insert(ao.authorities, o.value)
            end
        end
    end

    ao.outbox = {Output = {}, Messages = {}, Spawns = {}, Assignments = {}}
    ao.env = env

end

--- Logs a message to the output.
-- @function log
-- @tparam {string} txt The message to log
function ao.log(txt)
    if type(ao.outbox.Output) == 'string' then
        ao.outbox.Output = {ao.outbox.Output}
    end
    table.insert(ao.outbox.Output, txt)
end

--- Clears the outbox.
-- @function clearOutbox
function ao.clearOutbox()
    ao.outbox = {Output = {}, Messages = {}, Spawns = {}, Assignments = {}}
end

--- Sends a message.
-- @function send
-- @tparam {table} msg The message to send
function ao.send(msg)
    assert(type(msg) == 'table', 'msg should be a table')
    ao.reference = ao.reference + 1
    local referenceString = tostring(ao.reference)

    local message = {
        Target = msg.Target,
        Data = msg.Data,
        Anchor = padZero32(ao.reference),
        Tags = {
            {name = "Data-Protocol", value = "ao"},
            {name = "Variant", value = "ao.TN.1"},
            {name = "Type", value = "Message"},
            {name = "Reference", value = referenceString}
        }
    }

    -- if custom tags in root move them to tags
    for k, v in pairs(msg) do
        if not _includes({"Target", "Data", "Anchor", "Tags", "From"})(k) then
            table.insert(message.Tags, {name = k, value = v})
        end
    end

    if msg.Tags then
        if isArray(msg.Tags) then
            for _, o in ipairs(msg.Tags) do
                table.insert(message.Tags, o)
            end
        else
            for k, v in pairs(msg.Tags) do
                table.insert(message.Tags, {name = k, value = v})
            end
        end
    end

    -- If running in an environment without the AOS Handlers module, do not add
    -- the onReply and receive functions to the message.
    if not Handlers then return message end

    -- clone message info and add to outbox
    local extMessage = {}
    for k, v in pairs(message) do extMessage[k] = v end

    -- add message to outbox
    table.insert(ao.outbox.Messages, extMessage)

    -- add callback for onReply handler(s)
    message.onReply =
        function(...) -- Takes either (AddressThatWillReply, handler(s)) or (handler(s))
            local from, resolver
            if select("#", ...) == 2 then
                from = select(1, ...)
                resolver = select(2, ...)
            else
                from = message.Target
                resolver = select(1, ...)
            end

            -- Add a one-time callback that runs the user's (matching) resolver on reply
            Handlers.once({From = from, ["X-Reference"] = referenceString},
                          resolver)
        end

    message.receive = function(...)
        local from = message.Target
        if select("#", ...) == 1 then from = select(1, ...) end
        return
            Handlers.receive({From = from, ["X-Reference"] = referenceString})
    end

    return message
end

--- Spawns a process.
-- @function spawn
-- @tparam {string} module The module source id
-- @tparam {table} msg The message to send
function ao.spawn(module, msg)
    assert(type(module) == "string", "Module source id is required!")
    assert(type(msg) == 'table', 'Message must be a table')
    -- inc spawn reference
    ao.reference = ao.reference + 1
    local spawnRef = tostring(ao.reference)

    local spawn = {
        Data = msg.Data or "NODATA",
        Anchor = padZero32(ao.reference),
        Tags = {
            {name = "Data-Protocol", value = "ao"},
            {name = "Variant", value = "ao.TN.1"},
            {name = "Type", value = "Process"},
            {name = "From-Process", value = ao.id},
            {name = "From-Module", value = ao._module},
            {name = "Module", value = module},
            {name = "Reference", value = spawnRef}
        }
    }

    -- if custom tags in root move them to tags
    for k, v in pairs(msg) do
        if not _includes({"Target", "Data", "Anchor", "Tags", "From"})(k) then
            table.insert(spawn.Tags, {name = k, value = v})
        end
    end

    if msg.Tags then
        if isArray(msg.Tags) then
            for _, o in ipairs(msg.Tags) do
                table.insert(spawn.Tags, o)
            end
        else
            for k, v in pairs(msg.Tags) do
                table.insert(spawn.Tags, {name = k, value = v})
            end
        end
    end

    -- If running in an environment without the AOS Handlers module, do not add
    -- the after and receive functions to the spawn.
    if not Handlers then return spawn end

    -- clone spawn info and add to outbox
    local extSpawn = {}
    for k, v in pairs(spawn) do extSpawn[k] = v end

    table.insert(ao.outbox.Spawns, extSpawn)

    -- add 'after' callback to returned table
    -- local result = {}
    spawn.onReply = function(callback)
        Handlers.once({
            Action = "Spawned",
            From = ao.id,
            ["Reference"] = spawnRef
        }, callback)
    end

    spawn.receive = function()
        return Handlers.receive({
            Action = "Spawned",
            From = ao.id,
            ["Reference"] = spawnRef
        })

    end

    return spawn
end

--- Assigns a message to a process.
-- @function assign
-- @tparam {table} assignment The assignment to assign
function ao.assign(assignment)
    assert(type(assignment) == 'table', 'assignment should be a table')
    assert(type(assignment.Processes) == 'table', 'Processes should be a table')
    assert(type(assignment.Message) == "string", "Message should be a string")
    table.insert(ao.outbox.Assignments, assignment)
end

--- Checks if a message is trusted.
-- The default security model of AOS processes: Trust all and *only* those on the ao.authorities list.
-- @function isTrusted
-- @tparam {table} msg The message to check
-- @treturn {boolean} True if the message is trusted, false otherwise
function ao.isTrusted(msg)
    for _, authority in ipairs(ao.authorities) do
        if msg.From == authority then return true end
        if msg.Owner == authority then return true end
    end
    return false
end

--- Returns the result of the process.
-- @function result
-- @tparam {table} result The result of the process
-- @treturn {table} The result of the process, including Output, Messages, Spawns, and Assignments
function ao.result(result)
    -- if error then only send the Error to CU
    if ao.outbox.Error or result.Error then
        return {Error = result.Error or ao.outbox.Error}
    end
    return {
        Output = result.Output or ao.outbox.Output,
        Messages = ao.outbox.Messages,
        Spawns = ao.outbox.Spawns,
        Assignments = ao.outbox.Assignments
    }
end


--- Add the MatchSpec to the ao.assignables table. A optional name may be provided.
-- This implies that ao.assignables may have both number and string indices.
-- Added in the assignment module.
-- @function addAssignable
-- @tparam ?string|number|any nameOrMatchSpec The name of the MatchSpec
--        to be added to ao.assignables. if a MatchSpec is provided, then
--        no name is included
-- @tparam ?any matchSpec The MatchSpec to be added to ao.assignables. Only provided
--        if its name is passed as the first parameter
-- @treturn ?string|number name The name of the MatchSpec, either as provided
--          as an argument or as incremented
-- @see assignment

--- Remove the MatchSpec, either by name or by index
-- If the name is not found, or if the index does not exist, then do nothing.
-- Added in the assignment module.
-- @function removeAssignable
-- @tparam {string|number} name The name or index of the MatchSpec to be removed
-- @see assignment

--- Return whether the msg is an assignment or not. This can be determined by simply check whether the msg's Target is this process' id
-- Added in the assignment module.
-- @function isAssignment
-- @param msg The msg to be checked
-- @treturn boolean isAssignment
-- @see assignment

--- Check whether the msg matches any assignable MatchSpec.
-- If not assignables are configured, the msg is deemed not assignable, by default.
-- Added in the assignment module.
-- @function isAssignable
-- @param msg The msg to be checked
-- @treturn boolean isAssignable
-- @see assignment

return ao

end
_G.package.loaded[".ao"] = load_ao()
-- print("loaded ao")
  


local function load_aos() 
  
end
_G.package.loaded[".aos"] = load_aos()
-- print("loaded aos")
  


local function load_assignment() 
  --- The Assignment module provides functionality for handling assignments. Returns the Assignment table.
-- @module assignment

--- The Assignment module
-- @table Assignment
-- @field _version The version number of the assignment module
-- @field init The init function
local Assignment = { _version = "0.1.0" }

--- Implement assignable polyfills on ao.
-- Creates addAssignable, removeAssignable, isAssignment, and isAssignable fields on ao.
-- @function init
-- @tparam {table} ao The ao environment object
-- @see ao.addAssignable
-- @see ao.removeAssignable
-- @see ao.isAssignment
-- @see ao.isAssignable
function Assignment.init (ao)
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

  ao.assignables = ao.assignables or {}

  ao.addAssignable = ao.addAssignable or function (...)
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

    if name then idx = findIndexByProp(ao.assignables, "name", name) end

    if idx ~= nil and idx > 0 then
      -- found update
      ao.assignables[idx].pattern = matchSpec
    else
      -- append the new assignable, including potentially nil name
      table.insert(ao.assignables, { pattern = matchSpec, name = name })
    end
  end

  ao.removeAssignable = ao.removeAssignable or function (name)
    local idx = nil

    if type(name) == 'string' then idx = findIndexByProp(ao.assignables, "name", name)
    else
      assert(type(name) == 'number', 'index MUST be a number')
      idx = name
    end

    if idx == nil or idx <= 0 or idx > #ao.assignables then return end

    table.remove(ao.assignables, idx)
  end

  ao.isAssignment = ao.isAssignment or function (msg) return msg.Target ~= ao.id end

  ao.isAssignable = ao.isAssignable or function (msg)
    for _, assignable in pairs(ao.assignables) do
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

end
_G.package.loaded[".assignment"] = load_assignment()
-- print("loaded assignment")
  


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
-- print("loaded base64")
  


local function load_bint() 
  --[[--
lua-bint - v0.5.1 - 26/Jun/2023
Eduardo Bart - edub4rt@gmail.com
https://github.com/edubart/lua-bint

Small portable arbitrary-precision integer arithmetic library in pure Lua for
computing with large integers.

Different from most arbitrary-precision integer libraries in pure Lua out there this one
uses an array of lua integers as underlying data-type in its implementation instead of
using strings or large tables, this make it efficient for working with fixed width integers
and to make bitwise operations.

## Design goals

The main design goal of this library is to be small, correct, self contained and use few
resources while retaining acceptable performance and feature completeness.

The library is designed to follow recent Lua integer semantics, this means that
integer overflow warps around,
signed integers are implemented using two-complement arithmetic rules,
integer division operations rounds towards minus infinity,
any mixed operations with float numbers promotes the value to a float,
and the usual division/power operation always promotes to floats.

The library is designed to be possible to work with only unsigned integer arithmetic
when using the proper methods.

All the lua arithmetic operators (+, -, *, //, /, %) and bitwise operators (&, |, ~, <<, >>)
are implemented as metamethods.

The integer size must be fixed in advance and the library is designed to be more efficient when
working with integers of sizes between 64-4096 bits. If you need to work with really huge numbers
without size restrictions then use another library. This choice has been made to have more efficiency
in that specific size range.

## Usage

First on you should require the bint file including how many bits the bint module will work with,
by calling the returned function from the require, for example:

```lua
local bint = require 'bint'(1024)
```

For more information about its arguments see @{newmodule}.
Then when you need create a bint, you can use one of the following functions:

* @{bint.fromuinteger} (convert from lua integers, but read as unsigned integer)
* @{bint.frominteger} (convert from lua integers, preserving the sign)
* @{bint.frombase} (convert from arbitrary bases, like hexadecimal)
* @{bint.fromstring} (convert from arbitrary string, support binary/hexadecimal/decimal)
* @{bint.trunc} (convert from lua numbers, truncating the fractional part)
* @{bint.new} (convert from anything, asserts on invalid integers)
* @{bint.tobint} (convert from anything, returns nil on invalid integers)
* @{bint.parse} (convert from anything, returns a lua number as fallback)
* @{bint.zero}
* @{bint.one}
* `bint`

You can also call `bint` as it is an alias to `bint.new`.
In doubt use @{bint.new} to create a new bint.

Then you can use all the usual lua numeric operations on it,
all the arithmetic metamethods are implemented.
When you are done computing and need to get the result,
get the output from one of the following functions:

* @{bint.touinteger} (convert to a lua integer, wraps around as an unsigned integer)
* @{bint.tointeger} (convert to a lua integer, wraps around, preserves the sign)
* @{bint.tonumber} (convert to lua float, losing precision)
* @{bint.tobase} (convert to a string in any base)
* @{bint.__tostring} (convert to a string in base 10)

To output a very large integer with no loss you probably want to use @{bint.tobase}
or call `tostring` to get a string representation.

## Precautions

All library functions can be mixed with lua numbers,
this makes easy to mix operations between bints and lua numbers,
however the user should take care in some situations:

* Don't mix integers and float operations if you want to work with integers only.
* Don't use the regular equal operator ('==') to compare values from this library,
unless you know in advance that both values are of the same primitive type,
otherwise it will always return false, use @{bint.eq} to be safe.
* Don't pass fractional numbers to functions that an integer is expected
* Don't mix operations between bint classes with different sizes as this is not supported, this
will throw assertions.
* Remember that casting back to lua integers or numbers precision can be lost.
* For dividing while preserving integers use the @{bint.__idiv} (the '//' operator).
* For doing power operation preserving integers use the @{bint.ipow} function.
* Configure the proper integer size you intend to work with, otherwise large integers may wrap around.

]]

-- Returns number of bits of the internal lua integer type.
local function luainteger_bitsize()
  local n, i = -1, 0
  repeat
    n, i = n >> 16, i + 16
  until n==0
  return i
end

local math_type = math.type
local math_floor = math.floor
local math_abs = math.abs
local math_ceil = math.ceil
local math_modf = math.modf
local math_mininteger = math.mininteger
local math_maxinteger = math.maxinteger
local math_max = math.max
local math_min = math.min
local string_format = string.format
local table_insert = table.insert
local table_concat = table.concat
local table_unpack = table.unpack

local memo = {}

--- Create a new bint module representing integers of the desired bit size.
-- This is the returned function when `require 'bint'` is called.
-- @function newmodule
-- @param bits Number of bits for the integer representation, must be multiple of wordbits and
-- at least 64.
-- @param[opt] wordbits Number of the bits for the internal word,
-- defaults to half of Lua's integer size.
local function newmodule(bits, wordbits)

local intbits = luainteger_bitsize()
bits = bits or 256
wordbits = wordbits or (intbits // 2)

-- Memoize bint modules
local memoindex = bits * 64 + wordbits
if memo[memoindex] then
  return memo[memoindex]
end

-- Validate
assert(bits % wordbits == 0, 'bitsize is not multiple of word bitsize')
assert(2*wordbits <= intbits, 'word bitsize must be half of the lua integer bitsize')
assert(bits >= 64, 'bitsize must be >= 64')
assert(wordbits >= 8, 'wordbits must be at least 8')
assert(bits % 8 == 0, 'bitsize must be multiple of 8')

-- Create bint module
local bint = {}
bint.__index = bint

--- Number of bits representing a bint instance.
bint.bits = bits

-- Constants used internally
local BINT_BITS = bits
local BINT_BYTES = bits // 8
local BINT_WORDBITS = wordbits
local BINT_SIZE = BINT_BITS // BINT_WORDBITS
local BINT_WORDMAX = (1 << BINT_WORDBITS) - 1
local BINT_WORDMSB = (1 << (BINT_WORDBITS - 1))
local BINT_LEPACKFMT = '<'..('I'..(wordbits // 8)):rep(BINT_SIZE)
local BINT_MATHMININTEGER, BINT_MATHMAXINTEGER
local BINT_MININTEGER

--- Create a new bint with 0 value.
function bint.zero()
  local x = setmetatable({}, bint)
  for i=1,BINT_SIZE do
    x[i] = 0
  end
  return x
end
local bint_zero = bint.zero

--- Create a new bint with 1 value.
function bint.one()
  local x = setmetatable({}, bint)
  x[1] = 1
  for i=2,BINT_SIZE do
    x[i] = 0
  end
  return x
end
local bint_one = bint.one

-- Convert a value to a lua integer without losing precision.
local function tointeger(x)
  x = tonumber(x)
  local ty = math_type(x)
  if ty == 'float' then
    local floorx = math_floor(x)
    if floorx == x then
      x = floorx
      ty = math_type(x)
    end
  end
  if ty == 'integer' then
    return x
  end
end

--- Create a bint from an unsigned integer.
-- Treats signed integers as an unsigned integer.
-- @param x A value to initialize from convertible to a lua integer.
-- @return A new bint or nil in case the input cannot be represented by an integer.
-- @see bint.frominteger
function bint.fromuinteger(x)
  x = tointeger(x)
  if x then
    if x == 1 then
      return bint_one()
    elseif x == 0 then
      return bint_zero()
    end
    local n = setmetatable({}, bint)
    for i=1,BINT_SIZE do
      n[i] = x & BINT_WORDMAX
      x = x >> BINT_WORDBITS
    end
    return n
  end
end
local bint_fromuinteger = bint.fromuinteger

--- Create a bint from a signed integer.
-- @param x A value to initialize from convertible to a lua integer.
-- @return A new bint or nil in case the input cannot be represented by an integer.
-- @see bint.fromuinteger
function bint.frominteger(x)
  x = tointeger(x)
  if x then
    if x == 1 then
      return bint_one()
    elseif x == 0 then
      return bint_zero()
    end
    local neg = false
    if x < 0 then
      x = math_abs(x)
      neg = true
    end
    local n = setmetatable({}, bint)
    for i=1,BINT_SIZE do
      n[i] = x & BINT_WORDMAX
      x = x >> BINT_WORDBITS
    end
    if neg then
      n:_unm()
    end
    return n
  end
end
local bint_frominteger = bint.frominteger

local basesteps = {}

-- Compute the read step for frombase function
local function getbasestep(base)
  local step = basesteps[base]
  if step then
    return step
  end
  step = 0
  local dmax = 1
  local limit = math_maxinteger // base
  repeat
    step = step + 1
    dmax = dmax * base
  until dmax >= limit
  basesteps[base] = step
  return step
end

-- Compute power with lua integers.
local function ipow(y, x, n)
  if n == 1 then
    return y * x
  elseif n & 1 == 0 then --even
    return ipow(y, x * x, n // 2)
  end
  return ipow(x * y, x * x, (n-1) // 2)
end

--- Create a bint from a string of the desired base.
-- @param s The string to be converted from,
-- must have only alphanumeric and '+-' characters.
-- @param[opt] base Base that the number is represented, defaults to 10.
-- Must be at least 2 and at most 36.
-- @return A new bint or nil in case the conversion failed.
function bint.frombase(s, base)
  if type(s) ~= 'string' then
    return
  end
  base = base or 10
  if not (base >= 2 and base <= 36) then
    -- number base is too large
    return
  end
  local step = getbasestep(base)
  if #s < step then
    -- string is small, use tonumber (faster)
    return bint_frominteger(tonumber(s, base))
  end
  local sign, int = s:lower():match('^([+-]?)(%w+)$')
  if not (sign and int) then
    -- invalid integer string representation
    return
  end
  local n = bint_zero()
  for i=1,#int,step do
    local part = int:sub(i,i+step-1)
    local d = tonumber(part, base)
    if not d then
      -- invalid integer string representation
      return
    end
    if i > 1 then
      n = n * ipow(1, base, #part)
    end
    if d ~= 0 then
      n:_add(d)
    end
  end
  if sign == '-' then
    n:_unm()
  end
  return n
end
local bint_frombase = bint.frombase

--- Create a new bint from a string.
-- The string can by a decimal number, binary number prefixed with '0b' or hexadecimal number prefixed with '0x'.
-- @param s A string convertible to a bint.
-- @return A new bint or nil in case the conversion failed.
-- @see bint.frombase
function bint.fromstring(s)
  if type(s) ~= 'string' then
    return
  end
  if s:find('^[+-]?[0-9]+$') then
    return bint_frombase(s, 10)
  elseif s:find('^[+-]?0[xX][0-9a-fA-F]+$') then
    return bint_frombase(s:gsub('0[xX]', '', 1), 16)
  elseif s:find('^[+-]?0[bB][01]+$') then
    return bint_frombase(s:gsub('0[bB]', '', 1), 2)
  end
end
local bint_fromstring = bint.fromstring

--- Create a new bint from a buffer of little-endian bytes.
-- @param buffer Buffer of bytes, extra bytes are trimmed from the right, missing bytes are padded to the right.
-- @raise An assert is thrown in case buffer is not an string.
-- @return A bint.
function bint.fromle(buffer)
  assert(type(buffer) == 'string', 'buffer is not a string')
  if #buffer > BINT_BYTES then -- trim extra bytes from the right
    buffer = buffer:sub(1, BINT_BYTES)
  elseif #buffer < BINT_BYTES then -- add missing bytes to the right
    buffer = buffer..('\x00'):rep(BINT_BYTES - #buffer)
  end
  return setmetatable({BINT_LEPACKFMT:unpack(buffer)}, bint)
end

--- Create a new bint from a buffer of big-endian bytes.
-- @param buffer Buffer of bytes, extra bytes are trimmed from the left, missing bytes are padded to the left.
-- @raise An assert is thrown in case buffer is not an string.
-- @return A bint.
function bint.frombe(buffer)
  assert(type(buffer) == 'string', 'buffer is not a string')
  if #buffer > BINT_BYTES then -- trim extra bytes from the left
    buffer = buffer:sub(-BINT_BYTES, #buffer)
  elseif #buffer < BINT_BYTES then -- add missing bytes to the left
    buffer = ('\x00'):rep(BINT_BYTES - #buffer)..buffer
  end
  return setmetatable({BINT_LEPACKFMT:unpack(buffer:reverse())}, bint)
end

--- Create a new bint from a value.
-- @param x A value convertible to a bint (string, number or another bint).
-- @return A new bint, guaranteed to be a new reference in case needed.
-- @raise An assert is thrown in case x is not convertible to a bint.
-- @see bint.tobint
-- @see bint.parse
function bint.new(x)
  if getmetatable(x) ~= bint then
    local ty = type(x)
    if ty == 'number' then
      x = bint_frominteger(x)
    elseif ty == 'string' then
      x = bint_fromstring(x)
    end
    assert(x, 'value cannot be represented by a bint')
    return x
  end
  -- return a clone
  local n = setmetatable({}, bint)
  for i=1,BINT_SIZE do
    n[i] = x[i]
  end
  return n
end
local bint_new = bint.new

--- Convert a value to a bint if possible.
-- @param x A value to be converted (string, number or another bint).
-- @param[opt] clone A boolean that tells if a new bint reference should be returned.
-- Defaults to false.
-- @return A bint or nil in case the conversion failed.
-- @see bint.new
-- @see bint.parse
function bint.tobint(x, clone)
  if getmetatable(x) == bint then
    if not clone then
      return x
    end
    -- return a clone
    local n = setmetatable({}, bint)
    for i=1,BINT_SIZE do
      n[i] = x[i]
    end
    return n
  end
  local ty = type(x)
  if ty == 'number' then
    return bint_frominteger(x)
  elseif ty == 'string' then
    return bint_fromstring(x)
  end
end
local tobint = bint.tobint

--- Convert a value to a bint if possible otherwise to a lua number.
-- Useful to prepare values that you are unsure if it's going to be an integer or float.
-- @param x A value to be converted (string, number or another bint).
-- @param[opt] clone A boolean that tells if a new bint reference should be returned.
-- Defaults to false.
-- @return A bint or a lua number or nil in case the conversion failed.
-- @see bint.new
-- @see bint.tobint
function bint.parse(x, clone)
  local i = tobint(x, clone)
  if i then
    return i
  end
  return tonumber(x)
end
local bint_parse = bint.parse

--- Convert a bint to an unsigned integer.
-- Note that large unsigned integers may be represented as negatives in lua integers.
-- Note that lua cannot represent values larger than 64 bits,
-- in that case integer values wrap around.
-- @param x A bint or a number to be converted into an unsigned integer.
-- @return An integer or nil in case the input cannot be represented by an integer.
-- @see bint.tointeger
function bint.touinteger(x)
  if getmetatable(x) == bint then
    local n = 0
    for i=1,BINT_SIZE do
      n = n | (x[i] << (BINT_WORDBITS * (i - 1)))
    end
    return n
  end
  return tointeger(x)
end

--- Convert a bint to a signed integer.
-- It works by taking absolute values then applying the sign bit in case needed.
-- Note that lua cannot represent values larger than 64 bits,
-- in that case integer values wrap around.
-- @param x A bint or value to be converted into an unsigned integer.
-- @return An integer or nil in case the input cannot be represented by an integer.
-- @see bint.touinteger
function bint.tointeger(x)
  if getmetatable(x) == bint then
    local n = 0
    local neg = x:isneg()
    if neg then
      x = -x
    end
    for i=1,BINT_SIZE do
      n = n | (x[i] << (BINT_WORDBITS * (i - 1)))
    end
    if neg then
      n = -n
    end
    return n
  end
  return tointeger(x)
end
local bint_tointeger = bint.tointeger

local function bint_assert_tointeger(x)
  x = bint_tointeger(x)
  if not x then
    error('value has no integer representation')
  end
  return x
end

--- Convert a bint to a lua float in case integer would wrap around or lua integer otherwise.
-- Different from @{bint.tointeger} the operation does not wrap around integers,
-- but digits precision are lost in the process of converting to a float.
-- @param x A bint or value to be converted into a lua number.
-- @return A lua number or nil in case the input cannot be represented by a number.
-- @see bint.tointeger
function bint.tonumber(x)
  if getmetatable(x) == bint then
    if x <= BINT_MATHMAXINTEGER and x >= BINT_MATHMININTEGER then
      return x:tointeger()
    end
    return tonumber(tostring(x))
  end
  return tonumber(x)
end
local bint_tonumber = bint.tonumber

-- Compute base letters to use in bint.tobase
local BASE_LETTERS = {}
do
  for i=1,36 do
    BASE_LETTERS[i-1] = ('0123456789abcdefghijklmnopqrstuvwxyz'):sub(i,i)
  end
end

--- Convert a bint to a string in the desired base.
-- @param x The bint to be converted from.
-- @param[opt] base Base to be represented, defaults to 10.
-- Must be at least 2 and at most 36.
-- @param[opt] unsigned Whether to output as an unsigned integer.
-- Defaults to false for base 10 and true for others.
-- When unsigned is false the symbol '-' is prepended in negative values.
-- @return A string representing the input.
-- @raise An assert is thrown in case the base is invalid.
function bint.tobase(x, base, unsigned)
  x = tobint(x)
  if not x then
    -- x is a fractional float or something else
    return
  end
  base = base or 10
  if not (base >= 2 and base <= 36) then
    -- number base is too large
    return
  end
  if unsigned == nil then
    unsigned = base ~= 10
  end
  local isxneg = x:isneg()
  if (base == 10 and not unsigned) or (base == 16 and unsigned and not isxneg) then
    if x <= BINT_MATHMAXINTEGER and x >= BINT_MATHMININTEGER then
      -- integer is small, use tostring or string.format (faster)
      local n = x:tointeger()
      if base == 10 then
        return tostring(n)
      elseif unsigned then
        return string_format('%x', n)
      end
    end
  end
  local ss = {}
  local neg = not unsigned and isxneg
  x = neg and x:abs() or bint_new(x)
  local xiszero = x:iszero()
  if xiszero then
    return '0'
  end
  -- calculate basepow
  local step = 0
  local basepow = 1
  local limit = (BINT_WORDMSB - 1) // base
  repeat
    step = step + 1
    basepow = basepow * base
  until basepow >= limit
  -- serialize base digits
  local size = BINT_SIZE
  local xd, carry, d
  repeat
    -- single word division
    carry = 0
    xiszero = true
    for i=size,1,-1 do
      carry = carry | x[i]
      d, xd = carry // basepow, carry % basepow
      if xiszero and d ~= 0 then
        size = i
        xiszero = false
      end
      x[i] = d
      carry = xd << BINT_WORDBITS
    end
    -- digit division
    for _=1,step do
      xd, d = xd // base, xd % base
      if xiszero and xd == 0 and d == 0 then
        -- stop on leading zeros
        break
      end
      table_insert(ss, 1, BASE_LETTERS[d])
    end
  until xiszero
  if neg then
    table_insert(ss, 1, '-')
  end
  return table_concat(ss)
end

local function bint_assert_convert(x)
  return assert(tobint(x), 'value has not integer representation')
end

--- Convert a bint to a buffer of little-endian bytes.
-- @param x A bint or lua integer.
-- @param[opt] trim If true, zero bytes on the right are trimmed.
-- @return A buffer of bytes representing the input.
-- @raise Asserts in case input is not convertible to an integer.
function bint.tole(x, trim)
  x = bint_assert_convert(x)
  local s = BINT_LEPACKFMT:pack(table_unpack(x))
  if trim then
    s = s:gsub('\x00+$', '')
    if s == '' then
      s = '\x00'
    end
  end
  return s
end

--- Convert a bint to a buffer of big-endian bytes.
-- @param x A bint or lua integer.
-- @param[opt] trim If true, zero bytes on the left are trimmed.
-- @return A buffer of bytes representing the input.
-- @raise Asserts in case input is not convertible to an integer.
function bint.tobe(x, trim)
  x = bint_assert_convert(x)
  local s = BINT_LEPACKFMT:pack(table_unpack(x)):reverse()
  if trim then
    s = s:gsub('^\x00+', '')
    if s == '' then
      s = '\x00'
    end
  end
  return s
end

--- Check if a number is 0 considering bints.
-- @param x A bint or a lua number.
function bint.iszero(x)
  if getmetatable(x) == bint then
    for i=1,BINT_SIZE do
      if x[i] ~= 0 then
        return false
      end
    end
    return true
  end
  return x == 0
end

--- Check if a number is 1 considering bints.
-- @param x A bint or a lua number.
function bint.isone(x)
  if getmetatable(x) == bint then
    if x[1] ~= 1 then
      return false
    end
    for i=2,BINT_SIZE do
      if x[i] ~= 0 then
        return false
      end
    end
    return true
  end
  return x == 1
end

--- Check if a number is -1 considering bints.
-- @param x A bint or a lua number.
function bint.isminusone(x)
  if getmetatable(x) == bint then
    for i=1,BINT_SIZE do
      if x[i] ~= BINT_WORDMAX then
        return false
      end
    end
    return true
  end
  return x == -1
end
local bint_isminusone = bint.isminusone

--- Check if the input is a bint.
-- @param x Any lua value.
function bint.isbint(x)
  return getmetatable(x) == bint
end

--- Check if the input is a lua integer or a bint.
-- @param x Any lua value.
function bint.isintegral(x)
  return getmetatable(x) == bint or math_type(x) == 'integer'
end

--- Check if the input is a bint or a lua number.
-- @param x Any lua value.
function bint.isnumeric(x)
  return getmetatable(x) == bint or type(x) == 'number'
end

--- Get the number type of the input (bint, integer or float).
-- @param x Any lua value.
-- @return Returns "bint" for bints, "integer" for lua integers,
-- "float" from lua floats or nil otherwise.
function bint.type(x)
  if getmetatable(x) == bint then
    return 'bint'
  end
  return math_type(x)
end

--- Check if a number is negative considering bints.
-- Zero is guaranteed to never be negative for bints.
-- @param x A bint or a lua number.
function bint.isneg(x)
  if getmetatable(x) == bint then
    return x[BINT_SIZE] & BINT_WORDMSB ~= 0
  end
  return x < 0
end
local bint_isneg = bint.isneg

--- Check if a number is positive considering bints.
-- @param x A bint or a lua number.
function bint.ispos(x)
  if getmetatable(x) == bint then
    return not x:isneg() and not x:iszero()
  end
  return x > 0
end

--- Check if a number is even considering bints.
-- @param x A bint or a lua number.
function bint.iseven(x)
  if getmetatable(x) == bint then
    return x[1] & 1 == 0
  end
  return math_abs(x) % 2 == 0
end

--- Check if a number is odd considering bints.
-- @param x A bint or a lua number.
function bint.isodd(x)
  if getmetatable(x) == bint then
    return x[1] & 1 == 1
  end
  return math_abs(x) % 2 == 1
end

--- Create a new bint with the maximum possible integer value.
function bint.maxinteger()
  local x = setmetatable({}, bint)
  for i=1,BINT_SIZE-1 do
    x[i] = BINT_WORDMAX
  end
  x[BINT_SIZE] = BINT_WORDMAX ~ BINT_WORDMSB
  return x
end

--- Create a new bint with the minimum possible integer value.
function bint.mininteger()
  local x = setmetatable({}, bint)
  for i=1,BINT_SIZE-1 do
    x[i] = 0
  end
  x[BINT_SIZE] = BINT_WORDMSB
  return x
end

--- Bitwise left shift a bint in one bit (in-place).
function bint:_shlone()
  local wordbitsm1 = BINT_WORDBITS - 1
  for i=BINT_SIZE,2,-1 do
    self[i] = ((self[i] << 1) | (self[i-1] >> wordbitsm1)) & BINT_WORDMAX
  end
  self[1] = (self[1] << 1) & BINT_WORDMAX
  return self
end

--- Bitwise right shift a bint in one bit (in-place).
function bint:_shrone()
  local wordbitsm1 = BINT_WORDBITS - 1
  for i=1,BINT_SIZE-1 do
    self[i] = ((self[i] >> 1) | (self[i+1] << wordbitsm1)) & BINT_WORDMAX
  end
  self[BINT_SIZE] = self[BINT_SIZE] >> 1
  return self
end

-- Bitwise left shift words of a bint (in-place). Used only internally.
function bint:_shlwords(n)
  for i=BINT_SIZE,n+1,-1 do
    self[i] = self[i - n]
  end
  for i=1,n do
    self[i] = 0
  end
  return self
end

-- Bitwise right shift words of a bint (in-place). Used only internally.
function bint:_shrwords(n)
  if n < BINT_SIZE then
    for i=1,BINT_SIZE-n do
      self[i] = self[i + n]
    end
    for i=BINT_SIZE-n+1,BINT_SIZE do
      self[i] = 0
    end
  else
    for i=1,BINT_SIZE do
      self[i] = 0
    end
  end
  return self
end

--- Increment a bint by one (in-place).
function bint:_inc()
  for i=1,BINT_SIZE do
    local tmp = self[i]
    local v = (tmp + 1) & BINT_WORDMAX
    self[i] = v
    if v > tmp then
      break
    end
  end
  return self
end

--- Increment a number by one considering bints.
-- @param x A bint or a lua number to increment.
function bint.inc(x)
  local ix = tobint(x, true)
  if ix then
    return ix:_inc()
  end
  return x + 1
end

--- Decrement a bint by one (in-place).
function bint:_dec()
  for i=1,BINT_SIZE do
    local tmp = self[i]
    local v = (tmp - 1) & BINT_WORDMAX
    self[i] = v
    if v <= tmp then
      break
    end
  end
  return self
end

--- Decrement a number by one considering bints.
-- @param x A bint or a lua number to decrement.
function bint.dec(x)
  local ix = tobint(x, true)
  if ix then
    return ix:_dec()
  end
  return x - 1
end

--- Assign a bint to a new value (in-place).
-- @param y A value to be copied from.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_assign(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = y[i]
  end
  return self
end

--- Take absolute of a bint (in-place).
function bint:_abs()
  if self:isneg() then
    self:_unm()
  end
  return self
end

--- Take absolute of a number considering bints.
-- @param x A bint or a lua number to take the absolute.
function bint.abs(x)
  local ix = tobint(x, true)
  if ix then
    return ix:_abs()
  end
  return math_abs(x)
end
local bint_abs = bint.abs

--- Take the floor of a number considering bints.
-- @param x A bint or a lua number to perform the floor operation.
function bint.floor(x)
  if getmetatable(x) == bint then
    return bint_new(x)
  end
  return bint_new(math_floor(tonumber(x)))
end

--- Take ceil of a number considering bints.
-- @param x A bint or a lua number to perform the ceil operation.
function bint.ceil(x)
  if getmetatable(x) == bint then
    return bint_new(x)
  end
  return bint_new(math_ceil(tonumber(x)))
end

--- Wrap around bits of an integer (discarding left bits) considering bints.
-- @param x A bint or a lua integer.
-- @param y Number of right bits to preserve.
function bint.bwrap(x, y)
  x = bint_assert_convert(x)
  if y <= 0 then
    return bint_zero()
  elseif y < BINT_BITS then
    return x & (bint_one() << y):_dec()
  end
  return bint_new(x)
end

--- Rotate left integer x by y bits considering bints.
-- @param x A bint or a lua integer.
-- @param y Number of bits to rotate.
function bint.brol(x, y)
  x, y = bint_assert_convert(x), bint_assert_tointeger(y)
  if y > 0 then
    return (x << y) | (x >> (BINT_BITS - y))
  elseif y < 0 then
    if y ~= math_mininteger then
      return x:bror(-y)
    else
      x:bror(-(y+1))
      x:bror(1)
    end
  end
  return x
end

--- Rotate right integer x by y bits considering bints.
-- @param x A bint or a lua integer.
-- @param y Number of bits to rotate.
function bint.bror(x, y)
  x, y = bint_assert_convert(x), bint_assert_tointeger(y)
  if y > 0 then
    return (x >> y) | (x << (BINT_BITS - y))
  elseif y < 0 then
    if y ~= math_mininteger then
      return x:brol(-y)
    else
      x:brol(-(y+1))
      x:brol(1)
    end
  end
  return x
end

--- Truncate a number to a bint.
-- Floats numbers are truncated, that is, the fractional port is discarded.
-- @param x A number to truncate.
-- @return A new bint or nil in case the input does not fit in a bint or is not a number.
function bint.trunc(x)
  if getmetatable(x) ~= bint then
    x = tonumber(x)
    if x then
      local ty = math_type(x)
      if ty == 'float' then
        -- truncate to integer
        x = math_modf(x)
      end
      return bint_frominteger(x)
    end
    return
  end
  return bint_new(x)
end

--- Take maximum between two numbers considering bints.
-- @param x A bint or lua number to compare.
-- @param y A bint or lua number to compare.
-- @return A bint or a lua number. Guarantees to return a new bint for integer values.
function bint.max(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    return bint_new(ix > iy and ix or iy)
  end
  return bint_parse(math_max(x, y))
end

--- Take minimum between two numbers considering bints.
-- @param x A bint or lua number to compare.
-- @param y A bint or lua number to compare.
-- @return A bint or a lua number. Guarantees to return a new bint for integer values.
function bint.min(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    return bint_new(ix < iy and ix or iy)
  end
  return bint_parse(math_min(x, y))
end

--- Add an integer to a bint (in-place).
-- @param y An integer to be added.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_add(y)
  y = bint_assert_convert(y)
  local carry = 0
  for i=1,BINT_SIZE do
    local tmp = self[i] + y[i] + carry
    carry = tmp >> BINT_WORDBITS
    self[i] = tmp & BINT_WORDMAX
  end
  return self
end

--- Add two numbers considering bints.
-- @param x A bint or a lua number to be added.
-- @param y A bint or a lua number to be added.
function bint.__add(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local z = setmetatable({}, bint)
    local carry = 0
    for i=1,BINT_SIZE do
      local tmp = ix[i] + iy[i] + carry
      carry = tmp >> BINT_WORDBITS
      z[i] = tmp & BINT_WORDMAX
    end
    return z
  end
  return bint_tonumber(x) + bint_tonumber(y)
end

--- Subtract an integer from a bint (in-place).
-- @param y An integer to subtract.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_sub(y)
  y = bint_assert_convert(y)
  local borrow = 0
  local wordmaxp1 = BINT_WORDMAX + 1
  for i=1,BINT_SIZE do
    local res = self[i] + wordmaxp1 - y[i] - borrow
    self[i] = res & BINT_WORDMAX
    borrow = (res >> BINT_WORDBITS) ~ 1
  end
  return self
end

--- Subtract two numbers considering bints.
-- @param x A bint or a lua number to be subtracted from.
-- @param y A bint or a lua number to subtract.
function bint.__sub(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local z = setmetatable({}, bint)
    local borrow = 0
    local wordmaxp1 = BINT_WORDMAX + 1
    for i=1,BINT_SIZE do
      local res = ix[i] + wordmaxp1 - iy[i] - borrow
      z[i] = res & BINT_WORDMAX
      borrow = (res >> BINT_WORDBITS) ~ 1
    end
    return z
  end
  return bint_tonumber(x) - bint_tonumber(y)
end

--- Multiply two numbers considering bints.
-- @param x A bint or a lua number to multiply.
-- @param y A bint or a lua number to multiply.
function bint.__mul(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local z = bint_zero()
    local sizep1 = BINT_SIZE+1
    local s = sizep1
    local e = 0
    for i=1,BINT_SIZE do
      if ix[i] ~= 0 or iy[i] ~= 0 then
        e = math_max(e, i)
        s = math_min(s, i)
      end
    end
    for i=s,e do
      for j=s,math_min(sizep1-i,e) do
        local a = ix[i] * iy[j]
        if a ~= 0 then
          local carry = 0
          for k=i+j-1,BINT_SIZE do
            local tmp = z[k] + (a & BINT_WORDMAX) + carry
            carry = tmp >> BINT_WORDBITS
            z[k] = tmp & BINT_WORDMAX
            a = a >> BINT_WORDBITS
          end
        end
      end
    end
    return z
  end
  return bint_tonumber(x) * bint_tonumber(y)
end

--- Check if bints are equal.
-- @param x A bint to compare.
-- @param y A bint to compare.
function bint.__eq(x, y)
  for i=1,BINT_SIZE do
    if x[i] ~= y[i] then
      return false
    end
  end
  return true
end

--- Check if numbers are equal considering bints.
-- @param x A bint or lua number to compare.
-- @param y A bint or lua number to compare.
function bint.eq(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    return ix == iy
  end
  return x == y
end
local bint_eq = bint.eq

local function findleftbit(x)
  for i=BINT_SIZE,1,-1 do
    local v = x[i]
    if v ~= 0 then
      local j = 0
      repeat
        v = v >> 1
        j = j + 1
      until v == 0
      return (i-1)*BINT_WORDBITS + j - 1, i
    end
  end
end

-- Single word division modulus
local function sudivmod(nume, deno)
  local rema
  local carry = 0
  for i=BINT_SIZE,1,-1 do
    carry = carry | nume[i]
    nume[i] = carry // deno
    rema = carry % deno
    carry = rema << BINT_WORDBITS
  end
  return rema
end

--- Perform unsigned division and modulo operation between two integers considering bints.
-- This is effectively the same of @{bint.udiv} and @{bint.umod}.
-- @param x The numerator, must be a bint or a lua integer.
-- @param y The denominator, must be a bint or a lua integer.
-- @return The quotient following the remainder, both bints.
-- @raise Asserts on attempt to divide by zero
-- or if inputs are not convertible to integers.
-- @see bint.udiv
-- @see bint.umod
function bint.udivmod(x, y)
  local nume = bint_new(x)
  local deno = bint_assert_convert(y)
  -- compute if high bits of denominator are all zeros
  local ishighzero = true
  for i=2,BINT_SIZE do
    if deno[i] ~= 0 then
      ishighzero = false
      break
    end
  end
  if ishighzero then
    -- try to divide by a single word (optimization)
    local low = deno[1]
    assert(low ~= 0, 'attempt to divide by zero')
    if low == 1 then
      -- denominator is one
      return nume, bint_zero()
    elseif low <= (BINT_WORDMSB - 1) then
      -- can do single word division
      local rema = sudivmod(nume, low)
      return nume, bint_fromuinteger(rema)
    end
  end
  if nume:ult(deno) then
    -- denominator is greater than numerator
    return bint_zero(), nume
  end
  -- align leftmost digits in numerator and denominator
  local denolbit = findleftbit(deno)
  local numelbit, numesize = findleftbit(nume)
  local bit = numelbit - denolbit
  deno = deno << bit
  local wordmaxp1 = BINT_WORDMAX + 1
  local wordbitsm1 = BINT_WORDBITS - 1
  local denosize = numesize
  local quot = bint_zero()
  while bit >= 0 do
    -- compute denominator <= numerator
    local le = true
    local size = math_max(numesize, denosize)
    for i=size,1,-1 do
      local a, b = deno[i], nume[i]
      if a ~= b then
        le = a < b
        break
      end
    end
    -- if the portion of the numerator above the denominator is greater or equal than to the denominator
    if le then
      -- subtract denominator from the portion of the numerator
      local borrow = 0
      for i=1,size do
        local res = nume[i] + wordmaxp1 - deno[i] - borrow
        nume[i] = res & BINT_WORDMAX
        borrow = (res >> BINT_WORDBITS) ~ 1
      end
      -- concatenate 1 to the right bit of the quotient
      local i = (bit // BINT_WORDBITS) + 1
      quot[i] = quot[i] | (1 << (bit % BINT_WORDBITS))
    end
    -- shift right the denominator in one bit
    for i=1,denosize-1 do
      deno[i] = ((deno[i] >> 1) | (deno[i+1] << wordbitsm1)) & BINT_WORDMAX
    end
    local lastdenoword = deno[denosize] >> 1
    deno[denosize] = lastdenoword
    -- recalculate denominator size (optimization)
    if lastdenoword == 0 then
      while deno[denosize] == 0 do
        denosize = denosize - 1
      end
      if denosize == 0 then
        break
      end
    end
    -- decrement current set bit for the quotient
    bit = bit - 1
  end
  -- the remaining numerator is the remainder
  return quot, nume
end
local bint_udivmod = bint.udivmod

--- Perform unsigned division between two integers considering bints.
-- @param x The numerator, must be a bint or a lua integer.
-- @param y The denominator, must be a bint or a lua integer.
-- @return The quotient, a bint.
-- @raise Asserts on attempt to divide by zero
-- or if inputs are not convertible to integers.
function bint.udiv(x, y)
  return (bint_udivmod(x, y))
end

--- Perform unsigned integer modulo operation between two integers considering bints.
-- @param x The numerator, must be a bint or a lua integer.
-- @param y The denominator, must be a bint or a lua integer.
-- @return The remainder, a bint.
-- @raise Asserts on attempt to divide by zero
-- or if the inputs are not convertible to integers.
function bint.umod(x, y)
  local _, rema = bint_udivmod(x, y)
  return rema
end
local bint_umod = bint.umod

--- Perform integer truncate division and modulo operation between two numbers considering bints.
-- This is effectively the same of @{bint.tdiv} and @{bint.tmod}.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient following the remainder, both bint or lua number.
-- @raise Asserts on attempt to divide by zero or on division overflow.
-- @see bint.tdiv
-- @see bint.tmod
function bint.tdivmod(x, y)
  local ax, ay = bint_abs(x), bint_abs(y)
  local ix, iy = tobint(ax), tobint(ay)
  local quot, rema
  if ix and iy then
    assert(not (bint_eq(x, BINT_MININTEGER) and bint_isminusone(y)), 'division overflow')
    quot, rema = bint_udivmod(ix, iy)
  else
    quot, rema = ax // ay, ax % ay
  end
  local isxneg, isyneg = bint_isneg(x), bint_isneg(y)
  if isxneg ~= isyneg then
    quot = -quot
  end
  if isxneg then
    rema = -rema
  end
  return quot, rema
end
local bint_tdivmod = bint.tdivmod

--- Perform truncate division between two numbers considering bints.
-- Truncate division is a division that rounds the quotient towards zero.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient, a bint or lua number.
-- @raise Asserts on attempt to divide by zero or on division overflow.
function bint.tdiv(x, y)
  return (bint_tdivmod(x, y))
end

--- Perform integer truncate modulo operation between two numbers considering bints.
-- The operation is defined as the remainder of the truncate division
-- (division that rounds the quotient towards zero).
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The remainder, a bint or lua number.
-- @raise Asserts on attempt to divide by zero or on division overflow.
function bint.tmod(x, y)
  local _, rema = bint_tdivmod(x, y)
  return rema
end

--- Perform integer floor division and modulo operation between two numbers considering bints.
-- This is effectively the same of @{bint.__idiv} and @{bint.__mod}.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient following the remainder, both bint or lua number.
-- @raise Asserts on attempt to divide by zero.
-- @see bint.__idiv
-- @see bint.__mod
function bint.idivmod(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local isnumeneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local isdenoneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if isnumeneg then
      ix = -ix
    end
    if isdenoneg then
      iy = -iy
    end
    local quot, rema = bint_udivmod(ix, iy)
    if isnumeneg ~= isdenoneg then
      quot:_unm()
      -- round quotient towards minus infinity
      if not rema:iszero() then
        quot:_dec()
        -- adjust the remainder
        if isnumeneg and not isdenoneg then
          rema:_unm():_add(y)
        elseif isdenoneg and not isnumeneg then
          rema:_add(y)
        end
      end
    elseif isnumeneg then
      -- adjust the remainder
      rema:_unm()
    end
    return quot, rema
  end
  local nx, ny = bint_tonumber(x), bint_tonumber(y)
  return nx // ny, nx % ny
end
local bint_idivmod = bint.idivmod

--- Perform floor division between two numbers considering bints.
-- Floor division is a division that rounds the quotient towards minus infinity,
-- resulting in the floor of the division of its operands.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient, a bint or lua number.
-- @raise Asserts on attempt to divide by zero.
function bint.__idiv(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local isnumeneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local isdenoneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if isnumeneg then
      ix = -ix
    end
    if isdenoneg then
      iy = -iy
    end
    local quot, rema = bint_udivmod(ix, iy)
    if isnumeneg ~= isdenoneg then
      quot:_unm()
      -- round quotient towards minus infinity
      if not rema:iszero() then
        quot:_dec()
      end
    end
    return quot, rema
  end
  return bint_tonumber(x) // bint_tonumber(y)
end

--- Perform division between two numbers considering bints.
-- This always casts inputs to floats, for integer division only use @{bint.__idiv}.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient, a lua number.
function bint.__div(x, y)
  return bint_tonumber(x) / bint_tonumber(y)
end

--- Perform integer floor modulo operation between two numbers considering bints.
-- The operation is defined as the remainder of the floor division
-- (division that rounds the quotient towards minus infinity).
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The remainder, a bint or lua number.
-- @raise Asserts on attempt to divide by zero.
function bint.__mod(x, y)
  local _, rema = bint_idivmod(x, y)
  return rema
end

--- Perform integer power between two integers considering bints.
-- If y is negative then pow is performed as an unsigned integer.
-- @param x The base, an integer.
-- @param y The exponent, an integer.
-- @return The result of the pow operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__pow
-- @see bint.upowmod
function bint.ipow(x, y)
  y = bint_assert_convert(y)
  if y:iszero() then
    return bint_one()
  elseif y:isone() then
    return bint_new(x)
  end
  -- compute exponentiation by squaring
  x, y = bint_new(x),  bint_new(y)
  local z = bint_one()
  repeat
    if y:iseven() then
      x = x * x
      y:_shrone()
    else
      z = x * z
      x = x * x
      y:_dec():_shrone()
    end
  until y:isone()
  return x * z
end

--- Perform integer power between two unsigned integers over a modulus considering bints.
-- @param x The base, an integer.
-- @param y The exponent, an integer.
-- @param m The modulus, an integer.
-- @return The result of the pow operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__pow
-- @see bint.ipow
function bint.upowmod(x, y, m)
  m = bint_assert_convert(m)
  if m:isone() then
    return bint_zero()
  end
  x, y = bint_new(x),  bint_new(y)
  local z = bint_one()
  x = bint_umod(x, m)
  while not y:iszero() do
    if y:isodd() then
      z = bint_umod(z*x, m)
    end
    y:_shrone()
    x = bint_umod(x*x, m)
  end
  return z
end

--- Perform numeric power between two numbers considering bints.
-- This always casts inputs to floats, for integer power only use @{bint.ipow}.
-- @param x The base, a bint or lua number.
-- @param y The exponent, a bint or lua number.
-- @return The result of the pow operation, a lua number.
-- @see bint.ipow
function bint.__pow(x, y)
  return bint_tonumber(x) ^ bint_tonumber(y)
end

--- Bitwise left shift integers considering bints.
-- @param x An integer to perform the bitwise shift.
-- @param y An integer with the number of bits to shift.
-- @return The result of shift operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__shl(x, y)
  x, y = bint_new(x), bint_assert_tointeger(y)
  if y == math_mininteger or math_abs(y) >= BINT_BITS then
    return bint_zero()
  end
  if y < 0 then
    return x >> -y
  end
  local nvals = y // BINT_WORDBITS
  if nvals ~= 0 then
    x:_shlwords(nvals)
    y = y - nvals * BINT_WORDBITS
  end
  if y ~= 0 then
    local wordbitsmy = BINT_WORDBITS - y
    for i=BINT_SIZE,2,-1 do
      x[i] = ((x[i] << y) | (x[i-1] >> wordbitsmy)) & BINT_WORDMAX
    end
    x[1] = (x[1] << y) & BINT_WORDMAX
  end
  return x
end

--- Bitwise right shift integers considering bints.
-- @param x An integer to perform the bitwise shift.
-- @param y An integer with the number of bits to shift.
-- @return The result of shift operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__shr(x, y)
  x, y = bint_new(x), bint_assert_tointeger(y)
  if y == math_mininteger or math_abs(y) >= BINT_BITS then
    return bint_zero()
  end
  if y < 0 then
    return x << -y
  end
  local nvals = y // BINT_WORDBITS
  if nvals ~= 0 then
    x:_shrwords(nvals)
    y = y - nvals * BINT_WORDBITS
  end
  if y ~= 0 then
    local wordbitsmy = BINT_WORDBITS - y
    for i=1,BINT_SIZE-1 do
      x[i] = ((x[i] >> y) | (x[i+1] << wordbitsmy)) & BINT_WORDMAX
    end
    x[BINT_SIZE] = x[BINT_SIZE] >> y
  end
  return x
end

--- Bitwise AND bints (in-place).
-- @param y An integer to perform bitwise AND.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_band(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = self[i] & y[i]
  end
  return self
end

--- Bitwise AND two integers considering bints.
-- @param x An integer to perform bitwise AND.
-- @param y An integer to perform bitwise AND.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__band(x, y)
  return bint_new(x):_band(y)
end

--- Bitwise OR bints (in-place).
-- @param y An integer to perform bitwise OR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_bor(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = self[i] | y[i]
  end
  return self
end

--- Bitwise OR two integers considering bints.
-- @param x An integer to perform bitwise OR.
-- @param y An integer to perform bitwise OR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__bor(x, y)
  return bint_new(x):_bor(y)
end

--- Bitwise XOR bints (in-place).
-- @param y An integer to perform bitwise XOR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_bxor(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = self[i] ~ y[i]
  end
  return self
end

--- Bitwise XOR two integers considering bints.
-- @param x An integer to perform bitwise XOR.
-- @param y An integer to perform bitwise XOR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__bxor(x, y)
  return bint_new(x):_bxor(y)
end

--- Bitwise NOT a bint (in-place).
function bint:_bnot()
  for i=1,BINT_SIZE do
    self[i] = (~self[i]) & BINT_WORDMAX
  end
  return self
end

--- Bitwise NOT a bint.
-- @param x An integer to perform bitwise NOT.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__bnot(x)
  local y = setmetatable({}, bint)
  for i=1,BINT_SIZE do
    y[i] = (~x[i]) & BINT_WORDMAX
  end
  return y
end

--- Negate a bint (in-place). This effectively applies two's complements.
function bint:_unm()
  return self:_bnot():_inc()
end

--- Negate a bint. This effectively applies two's complements.
-- @param x A bint to perform negation.
function bint.__unm(x)
  return (~x):_inc()
end

--- Compare if integer x is less than y considering bints (unsigned version).
-- @param x Left integer to compare.
-- @param y Right integer to compare.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__lt
function bint.ult(x, y)
  x, y = bint_assert_convert(x), bint_assert_convert(y)
  for i=BINT_SIZE,1,-1 do
    local a, b = x[i], y[i]
    if a ~= b then
      return a < b
    end
  end
  return false
end

--- Compare if bint x is less or equal than y considering bints (unsigned version).
-- @param x Left integer to compare.
-- @param y Right integer to compare.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__le
function bint.ule(x, y)
  x, y = bint_assert_convert(x), bint_assert_convert(y)
  for i=BINT_SIZE,1,-1 do
    local a, b = x[i], y[i]
    if a ~= b then
      return a < b
    end
  end
  return true
end

--- Compare if number x is less than y considering bints and signs.
-- @param x Left value to compare, a bint or lua number.
-- @param y Right value to compare, a bint or lua number.
-- @see bint.ult
function bint.__lt(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local xneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local yneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if xneg == yneg then
      for i=BINT_SIZE,1,-1 do
        local a, b = ix[i], iy[i]
        if a ~= b then
          return a < b
        end
      end
      return false
    end
    return xneg and not yneg
  end
  return bint_tonumber(x) < bint_tonumber(y)
end

--- Compare if number x is less or equal than y considering bints and signs.
-- @param x Left value to compare, a bint or lua number.
-- @param y Right value to compare, a bint or lua number.
-- @see bint.ule
function bint.__le(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local xneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local yneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if xneg == yneg then
      for i=BINT_SIZE,1,-1 do
        local a, b = ix[i], iy[i]
        if a ~= b then
          return a < b
        end
      end
      return true
    end
    return xneg and not yneg
  end
  return bint_tonumber(x) <= bint_tonumber(y)
end

--- Convert a bint to a string on base 10.
-- @see bint.tobase
function bint:__tostring()
  return self:tobase(10)
end

-- Allow creating bints by calling bint itself
setmetatable(bint, {
  __call = function(_, x)
    return bint_new(x)
  end
})

BINT_MATHMININTEGER, BINT_MATHMAXINTEGER = bint_new(math.mininteger), bint_new(math.maxinteger)
BINT_MININTEGER = bint.mininteger()
memo[memoindex] = bint

return bint

end

return newmodule
end
_G.package.loaded[".bint"] = load_bint()
-- print("loaded bint")
  


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
-- print("loaded boot")
  


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
-- print("loaded chance")
  


local function load_default() 
  -- default handler for aos
return function (insertInbox)
  return function (msg)
    -- Add Message to Inbox
    insertInbox(msg)

    local txt = Colors.gray .. "New Message From " .. Colors.green .. 
    (msg.From and (msg.From:sub(1,3) .. "..." .. msg.From:sub(-3)) or "unknown") .. Colors.gray .. ": "
    if msg.Action then
      txt = txt .. Colors.gray .. (msg.Action and ("Action = " .. Colors.blue .. msg.Action:sub(1,20)) or "") .. Colors.reset
    else
      local data = msg.Data
      if type(data) == 'table' then
        data = require('.json').encode(data)
      end
      txt = txt .. Colors.gray .. "Data = " .. Colors.blue .. (data and data:sub(1,20) or "") .. Colors.reset
    end
    -- Print to Output
    print(txt)
  end

end

end
_G.package.loaded[".default"] = load_default()
-- print("loaded default")
  


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
-- print("loaded dump")
  


local function load_eval() 
  --- The Eval module provides a handler for evaluating Lua expressions. Returns the eval function.
-- @module eval

--- The eval function.
-- Handler for executing and evaluating Lua expressions.
-- After execution, the result is stringified and placed in ao.outbox.Output.
-- @function eval
-- @tparam {table} ao The ao environment object
-- @treturn {function} The handler function, which takes a message as an argument.
-- @see stringify
return function (ao)
  return function (msg)
    -- exec expression
    local expr = msg.Data
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
    if HANDLER_PRINT_LOGS and output then
      table.insert(HANDLER_PRINT_LOGS, type(output) == "table" and stringify.format(output) or tostring(output))
    else
      -- set result in outbox.Output (Left for backwards compatibility)
      ao.outbox.Output = {  
        json = type(output) == "table" and pcall(function () return json.encode(output) end) and output or "undefined",
        data = {
          output = type(output) == "table" and stringify.format(output) or output,
          prompt = Prompt()
        }, 
        prompt = Prompt()
      }

    end
  end
end

end
_G.package.loaded[".eval"] = load_eval()
-- print("loaded eval")
  


local function load_handle() 

function handle(msgJSON, aoJSON)
    -- by requiring '.process' here we are able to reload via .updates
    local process = require ".process"
    local json = require '.json'
    -- decode inputs
    local msg = json.decode(msgJSON)
    local env = json.decode(aoJSON)

    -- handle process
    local status, response = pcall(function()
        return (process.handle(msg, env))
    end)

    -- encode output
    local responseJSON = json.encode({ok = status, response = response})
    -- free 
    response = nil
    collectgarbage()
    return responseJSON
end

end
_G.package.loaded[".handle"] = load_handle()
-- print("loaded handle")
  


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

local ao = require(".ao")

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
-- print("loaded handlers-utils")
  


local function load_handlers() 
  --- The Handlers library provides a flexible way to manage and execute a series of handlers based on patterns. Each handler consists of a pattern function, a handle function, and a name. This library is suitable for scenarios where different actions need to be taken based on varying input criteria. Returns the handlers table.
-- @module handlers

--- The handlers table
-- @table handlers
-- @field _version The version number of the handlers module
-- @field list The list of handlers
-- @field coroutines The coroutines of the handlers
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

handlers.utils = require('.handlers-utils')
-- if update we need to keep defined handlers
if Handlers then
  handlers.list = Handlers.list or {}
  handlers.coroutines = Handlers.coroutines or {}
else
  handlers.list = {}
  handlers.coroutines = {}

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
    '\tpattern : Action : string | MsgMatch : table,\n' ..
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
  local self = coroutine.running()
  handlers.once(pattern, function (msg)
      -- If the result of the resumed coroutine is an error then we should bubble it up to the process
      local _, success, errmsg = coroutine.resume(self, msg)
      if not success then
        error(errmsg)
      end
  end)
  return coroutine.yield(pattern)
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
-- print("loaded handlers")
  


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
-- print("loaded json")
  


local function load_pretty() 
  --- The Pretty module provides a utility for printing Lua tables in a more readable format.
-- @module pretty

--- The pretty module
-- @table pretty
-- @field _version The version number of the pretty module
-- @field tprint The tprint function
local pretty = { _version = "0.0.1" }

--- Prints a table with indentation for better readability.
-- @function tprint
-- @tparam {table} tbl The table to print
-- @tparam {number} indent The indentation level (default is 0)
-- @treturn {string} A string representation of the table with indentation
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
-- print("loaded pretty")
  


local function load_process() 
  --- The Process library provides an environment for managing and executing processes on the AO network. It includes capabilities for handling messages, spawning processes, and customizing the environment with programmable logic and handlers. Returns the process table.
-- @module process

-- @dependencies
local pretty = require('.pretty')
local base64 = require('.base64')
local json = require('.json')
local chance = require('.chance')
-- set alias ao for .ao library
if not _G.package.loaded['ao'] then _G.package.loaded['ao'] = require('.ao') end

Colors = {
  red = "\27[31m",
  green = "\27[32m",
  blue = "\27[34m",
  reset = "\27[0m",
  gray = "\27[90m"
}

Bell = "\x07"

Dump = require('.dump')
Handlers = require('.handlers')
local assignment = require('.assignment')
Nonce = Nonce or nil
ao = nil
if _G.package.loaded['.ao'] then
  ao = require('.ao')
elseif _G.package.loaded['ao'] then
  ao = require('ao')
end
-- Implement assignable polyfills on _ao
assignment.init(ao)

--- The process table
-- @table process
-- @field _version The version number of the process

local process = { _version = "2.0.4" }
-- The maximum number of messages to store in the inbox
local maxInboxCount = 10000

-- wrap ao.send and ao.spawn for magic table
local aosend = ao.send
local aospawn = ao.spawn

ao.send = function (msg)
  if msg.Data and type(msg.Data) == 'table' then
    msg['Content-Type'] = 'application/json'
    msg.Data = require('.json').encode(msg.Data)
  end
  return aosend(msg)
end
ao.spawn = function (module, msg) 
  if msg.Data and type(msg.Data) == 'table' then
    msg['Content-Type'] = 'application/json'
    msg.Data = require('.json').encode(msg.Data)
  end
  return aospawn(module, msg)
end

--- Remove the last three lines from a string
-- @lfunction removeLastThreeLines
-- @tparam {string} input The string to remove the last three lines from
-- @treturn {string} The string with the last three lines removed
local function removeLastThreeLines(input)
  local lines = {}
  for line in input:gmatch("([^\n]*)\n?") do
      table.insert(lines, line)
  end

  -- Remove the last three lines
  for i = 1, 3 do
      table.remove(lines)
  end

  -- Concatenate the remaining lines
  return table.concat(lines, "\n")
end

--- Insert a message into the inbox and manage overflow
-- @lfunction insertInbox
-- @tparam {table} msg The message to insert into the inbox
local function insertInbox(msg)
  table.insert(Inbox, msg)
  if #Inbox > maxInboxCount then
    local overflow = #Inbox - maxInboxCount
    for i = 1,overflow do
      table.remove(Inbox, 1)
    end
  end
end

--- Find an object in an array by a given key and value
-- @lfunction findObject
-- @tparam {table} array The array to search through
-- @tparam {string} key The key to search for
-- @tparam {any} value The value to search for
local function findObject(array, key, value)
  for i, object in ipairs(array) do
    if object[key] == value then
      return object
    end
  end
  return nil
end

--- Convert a message's tags to a table of key-value pairs
-- @function Tab
-- @tparam {table} msg The message containing tags
-- @treturn {table} A table with tag names as keys and their values
function Tab(msg)
  local inputs = {}
  for _, o in ipairs(msg.Tags) do
    if not inputs[o.name] then
      inputs[o.name] = o.value
    end
  end
  return inputs
end

--- Generate a prompt string for the current process
-- @function Prompt
-- @treturn {string} The custom command prompt string
function Prompt()
  return Colors.green .. Name .. Colors.gray
    .. "@" .. Colors.blue .. "aos-" .. process._version .. Colors.gray
    .. "[Inbox:" .. Colors.red .. tostring(#Inbox) .. Colors.gray
    .. "]" .. Colors.reset .. "> "
end

--- Print a value, formatting tables and converting non-string types
-- @function print
-- @tparam {any} a The value to print
function print(a)
  if type(a) == "table" then
    a = stringify.format(a)
  end
  --[[
In order to print non string types we need to convert to string
  ]]
  if type(a) == "boolean" then
    a = Colors.blue .. tostring(a) .. Colors.reset
  end
  if type(a) == "nil" then
    a = Colors.red .. tostring(a) .. Colors.reset
  end
  if type(a) == "number" then
    a = Colors.green .. tostring(a) .. Colors.reset
  end
  
  local data = a
  if ao.outbox.Output.data then
    data =  ao.outbox.Output.data .. "\n" .. a
  end
  ao.outbox.Output = { data = data, prompt = Prompt(), print = true }

  -- Only supported for newer version of AOS
  if HANDLER_PRINT_LOGS then 
    table.insert(HANDLER_PRINT_LOGS, a)
    return nil
  end

  return tostring(a)
end

--- Send a message to a target process
-- @function Send
-- @tparam {table} msg The message to send
function Send(msg)
  if not msg.Target then
    print("WARN: No target specified for message. Data will be stored, but no process will receive it.")
  end
  local result = ao.send(msg)
  return {
    output = "Message added to outbox",
    receive = result.receive,
    onReply = result.onReply
  }
end

--- Spawn a new process
-- @function Spawn
-- @tparam {...any} args The arguments to pass to the spawn function
function Spawn(...)
  local module, spawnMsg

  if select("#", ...) == 1 then
    spawnMsg = select(1, ...)
    module = ao._module
  else
    module = select(1, ...)
    spawnMsg = select(2, ...)
  end

  if not spawnMsg then
    spawnMsg = {}
  end
  local result = ao.spawn(module, spawnMsg)
  return {
    output = "Spawn process request added to outbox",
    after = result.after,
    receive = result.receive
  }
end

--- Calls Handlers.receive with the provided pattern criteria, awaiting a message that matches the criteria.
-- @function Receive
-- @tparam {table} match The pattern criteria for the message
-- @treturn {any} The result of the message handling
function Receive(match)
  return Handlers.receive(match)
end

--- Assigns based on the assignment passed.
-- @function Assign
-- @tparam {table} assignment The assignment to be made
function Assign(assignment)
  if not ao.assign then
    print("Assign is not implemented.")
    return "Assign is not implemented."
  end
  ao.assign(assignment)
  print("Assignment added to outbox.")
  return 'Assignment added to outbox.'
end

Seeded = Seeded or false

--- Converts a string to a seed value
-- @lfunction stringToSeed
-- @tparam {string} s The string to convert to a seed
-- @treturn {number} The seed value
-- this is a temporary approach...
local function stringToSeed(s)
  local seed = 0
  for i = 1, #s do
      local char = string.byte(s, i)
      seed = seed + char
  end
  return seed
end

--- Initializes or updates the state of the process based on the incoming message and environment.
-- @lfunction initializeState
-- @tparam {table} msg The message to initialize the state with
-- @tparam {table} env The environment to initialize the state with
local function initializeState(msg, env)
  if not Seeded then
    chance.seed(tonumber(msg['Block-Height'] .. stringToSeed(msg.Owner .. msg.Module .. msg.Id)))
    math.random = function (...)
      local args = {...}
      local n = #args
      if n == 0 then
        return chance.random()
      end
      if n == 1 then
        return chance.integer(1, args[1])
      end
      if n == 2 then
        return chance.integer(args[1], args[2])
      end
      return chance.random()
    end
    Seeded = true
  end
  Errors = Errors or {}
  Inbox = Inbox or {}

  -- Owner should only be assiged once
  if env.Process.Id == msg.Id and not Owner then
    local _from = findObject(env.Process.Tags, "name", "From-Process")
    if _from then
      Owner = _from.value
    else
      Owner = msg.From
    end
  end

  if not Name then
    local aosName = findObject(env.Process.Tags, "name", "Name")
    if aosName then
      Name = aosName.value
    else
      Name = 'aos'
    end
  end

end

--- Prints the version of the process
-- @function Version
function Version()
  print("version: " .. process._version)
end

--- Main handler for processing incoming messages. It initializes the state, processes commands, and handles message evaluation and inbox management.
-- @function handle
-- @tparam {table} msg The message to handle
-- @tparam {table} _ The environment to handle the message in
function process.handle(msg, _)
  local env = nil
  if _.Process then
    env = _
  else
    env = _.env
  end
  
  ao.init(env)
  -- relocate custom tags to root message
  msg = ao.normalize(msg)
  -- set process id
  ao.id = ao.env.Process.Id
  initializeState(msg, ao.env)
  HANDLER_PRINT_LOGS = {}
  
  -- set os.time to return msg.Timestamp
  os.time = function () return msg.Timestamp end

  -- tagify msg
  msg.TagArray = msg.Tags
  msg.Tags = Tab(msg)
  -- tagify Process
  ao.env.Process.TagArray = ao.env.Process.Tags
  ao.env.Process.Tags = Tab(ao.env.Process)
  -- magic table - if Content-Type == application/json - decode msg.Data to a Table
  if msg.Tags['Content-Type'] and msg.Tags['Content-Type'] == 'application/json' then
    msg.Data = json.decode(msg.Data or "{}")
  end
  -- init Errors
  Errors = Errors or {}
  -- clear Outbox
  ao.clearOutbox()

  -- Only check for Nonce if msg is not read-only and not cron
  if not msg['Read-Only'] and not msg['Cron'] then
    if not ao.Nonce then
      ao.Nonce = tonumber(msg.Nonce)
    else
      if tonumber(msg.Nonce) ~= (ao.Nonce + 1) then
        print(Colors.red .. "WARNING: Nonce did not match, may be due to an error generated by process" .. Colors.reset)
        print("")
        --return ao.result({Error = "HALT Nonce is out of sync " .. ao.Nonce .. " <> " .. (msg.Nonce or "0") })
      end 
    end
  end

  -- Only trust messages from a signed owner or an Authority
  if msg.From ~= msg.Owner and not ao.isTrusted(msg) then
    if msg.From ~= ao.id then
      Send({Target = msg.From, Data = "Message is not trusted by this process!"})
    end
    print('Message is not trusted! From: ' .. msg.From .. ' - Owner: ' .. msg.Owner)
    return ao.result({ }) 
  end

  if ao.isAssignment(msg) and not ao.isAssignable(msg) then
    if msg.From ~= ao.id then
      Send({Target = msg.From, Data = "Assignment is not trusted by this process!"})
    end
    print('Assignment is not trusted! From: ' .. msg.From .. ' - Owner: ' .. msg.Owner)
    return ao.result({ })
  end

  Handlers.add("_eval",
    function (msg)
      return msg.Action == "Eval" and Owner == msg.From
    end,
    require('.eval')(ao)
  )

  -- Added for aop6 boot loader
  -- See: https://github.com/permaweb/aos/issues/342
  -- Only run bootloader when Process Message is First Message
  if env.Process.Id == msg.Id then
    Handlers.once("_boot",
      function (msg)
        return msg.Tags.Type == "Process" and Owner == msg.From 
      end,
      require('.boot')(ao)
    )
  end

  Handlers.append("_default", function () return true end, require('.default')(insertInbox))

  -- call evaluate from handlers passing env
  msg.reply =
    function(replyMsg)
      replyMsg.Target = msg["Reply-To"] or (replyMsg.Target or msg.From)
      replyMsg["X-Reference"] = msg["X-Reference"] or msg.Reference
      replyMsg["X-Origin"] = msg["X-Origin"] or nil

      return ao.send(replyMsg)
    end
  
  msg.forward =
    function(target, forwardMsg)
      -- Clone the message and add forwardMsg tags
      local newMsg =  ao.sanitize(msg)
      forwardMsg = forwardMsg or {}

      for k,v in pairs(forwardMsg) do
        newMsg[k] = v
      end

      -- Set forward-specific tags
      newMsg.Target = target
      newMsg["Reply-To"] = msg["Reply-To"] or msg.From
      newMsg["X-Reference"] = msg["X-Reference"] or msg.Reference
      newMsg["X-Origin"] = msg["X-Origin"] or msg.From
      -- clear functions
      newMsg.reply = nil
      newMsg.forward = nil 
      
      ao.send(newMsg)
    end

  local co = coroutine.create(
    function()
      return pcall(Handlers.evaluate, msg, env)
    end
  )
  local _, status, result = coroutine.resume(co)

  -- Make sure we have a reference to the coroutine if it will wake up.
  -- Simultaneously, prune any dead coroutines so that they can be
  -- freed by the garbage collector.
  table.insert(Handlers.coroutines, co)
  for i, x in ipairs(Handlers.coroutines) do
    if coroutine.status(x) == "dead" then
      table.remove(Handlers.coroutines, i)
    end
  end

  if not status then
    if (msg.Action == "Eval") then
      table.insert(Errors, result)
      local printData = table.concat(HANDLER_PRINT_LOGS, "\n")
      return { Error = printData .. '\n\n' .. Colors.red .. 'error:\n' .. Colors.reset .. result }
    end 
    --table.insert(Errors, result)
    --ao.outbox.Output.data = ""
    if msg.Action then
      print(Colors.red .. "Error" .. Colors.gray .. " handling message with Action = " .. msg.Action  .. Colors.reset)
    else
      print(Colors.red .. "Error" .. Colors.gray .. " handling message " .. Colors.reset)
    end
    print(Colors.green .. result .. Colors.reset)
    print("\n" .. Colors.gray .. removeLastThreeLines(debug.traceback()) .. Colors.reset)
    local printData = table.concat(HANDLER_PRINT_LOGS, "\n")
    return ao.result({Error = printData .. '\n\n' .. Colors.red .. 'error:\n' .. Colors.reset .. result, Messages = {}, Spawns = {}, Assignments = {} })
  end
  
  if msg.Action == "Eval" then
    local response = ao.result({ 
      Output = {
        data = table.concat(HANDLER_PRINT_LOGS, "\n"),
        prompt = Prompt(),
        test = Dump(HANDLER_PRINT_LOGS)
      }
    })
    HANDLER_PRINT_LOGS = {} -- clear logs
    ao.Nonce = msg.Nonce
    return response
  elseif msg.Tags.Type == "Process" and Owner == msg.From then 
    local response = ao.result({ 
      Output = {
        data = table.concat(HANDLER_PRINT_LOGS, "\n"),
        prompt = Prompt(),
        print = true
      }
    })
    HANDLER_PRINT_LOGS = {} -- clear logs
    ao.Nonce = msg.Nonce
    return response

    -- local response = nil
  
    -- -- detect if there was any output from the boot loader call
    -- for _, value in pairs(HANDLER_PRINT_LOGS) do
    --   if value ~= "" then
    --     -- there was output from the Boot Loader eval so we want to print it
    --     response = ao.result({ Output = { data = table.concat(HANDLER_PRINT_LOGS, "\n"), prompt = Prompt(), print = true } })
    --     break
    --   end
    -- end
  
    -- if response == nil then 
    --   -- there was no output from the Boot Loader eval, so we shouldn't print it
    --   response = ao.result({ Output = { data = "", prompt = Prompt() } })
    -- end

    -- HANDLER_PRINT_LOGS = {} -- clear logs
    -- return response
  else
    local response = ao.result({ Output = { data = table.concat(HANDLER_PRINT_LOGS, "\n"), prompt = Prompt(), print = true } })
    HANDLER_PRINT_LOGS = {} -- clear logs
    ao.Nonce = msg.Nonce
    return response
  end
end

return process

end
_G.package.loaded[".process"] = load_process()
-- print("loaded process")
  


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
-- print("loaded stringify")
  


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
      -- The key can either be in the top level of the 'msg' object or within the 'Tags' 

      local msgValue = msg[key]
      local msgTagValue = msg['Tags'] and msg['Tags'][key]
  
      if not msgValue and not msgTagValue then
        return false
      end
  
      local matchesMsgValue = utils.matchesPattern(pattern, msgValue, msg)
      local matchesMsgTagValue = utils.matchesPattern(pattern, msgTagValue, msg)
  
      if not matchesMsgValue and not matchesMsgTagValue then
        return false
      end
    end
    return true
  end
  
  if type(spec) == 'string' and msg.Action and msg.Action == spec then
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

return utils

end
_G.package.loaded[".utils"] = load_utils()
-- print("loaded utils")
  

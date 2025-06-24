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
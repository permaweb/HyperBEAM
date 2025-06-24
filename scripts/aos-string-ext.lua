  -- @module string-ext
  --- The String Extensions module provides missing string functions for Luerl compatibility.
  --- Currently implements `string.gmatch` and returns the `string-ext` table.

  -- @table string-ext
  -- @field _version The version number of the string-ext module
  -- @field install  The install function to patch the global string table
  local string_ext = { _version = "0.3.0" }

  -- Security constants
  local MAX_STRING_LENGTH = 100000  -- 100KB limit
  local MAX_PATTERN_LENGTH = 500    -- 500 char pattern limit
  local MAX_ITERATIONS = 10000      -- Prevent infinite loops

  -- Pattern validation: check for potentially dangerous patterns
  local function validate_pattern(pattern)
    -- Block patterns with excessive quantifiers that could cause exponential backtracking
    local quantifier_count = 0
    local pos = 1
    while pos <= #pattern do
      local found = string.find(pattern, "[%*%+%?]", pos)
      if found then
        quantifier_count = quantifier_count + 1
        pos = found + 1
      else
        break
      end
    end
    if quantifier_count > 5 then
      error("pattern contains too many quantifiers", 3)
    end
    
    -- Block nested quantifiers like .*.*
    if string.find(pattern, "%.%*%.%*") or string.find(pattern, "%.%+%.%+") then
      error("nested quantifiers not allowed", 3)
    end
    
    -- Block patterns with excessive alternation (if using | for alternation)
    local pipe_count = 0
    pos = 1
    while pos <= #pattern do
      local found = string.find(pattern, "|", pos)
      if found then
        pipe_count = pipe_count + 1
        pos = found + 1
      else
        break
      end
    end
    if pipe_count > 10 then
      error("pattern too complex", 3)
    end
    
    -- Check for malformed patterns that could cause issues
    -- Unclosed brackets
    local bracket_depth = 0
    pos = 1
    while pos <= #pattern do
      local char = pattern:sub(pos, pos)
      if char == "[" and (pos == 1 or pattern:sub(pos-1, pos-1) ~= "%") then
        bracket_depth = bracket_depth + 1
      elseif char == "]" and (pos == 1 or pattern:sub(pos-1, pos-1) ~= "%") then
        bracket_depth = bracket_depth - 1
      end
      pos = pos + 1
    end
    if bracket_depth ~= 0 then
      error("malformed pattern (unclosed '[')", 3)
    end
    
    -- Check for potential ReDoS patterns - be more specific to avoid false positives
    -- Look for nested quantified groups like (a+)+ or (a*)*
    if string.find(pattern, "%(.*%+%)%+") or string.find(pattern, "%(.*%*%)%*") then
      error("pattern may cause catastrophic backtracking", 3)
    end
  end

  --- Install string extensions into the global string table.
  -- Adds `string.gmatch` even if string.gmatch exists but is broken (like in Luerl)
  -- @function install
  function string_ext.install()
      --- Iterator that finds successive matches of *pattern* in *s*.
      -- Behaves like native `string.gmatch` in Lua 5.3+.
      -- If the pattern contains captures, each iterator step returns all captures.
      -- Otherwise, it returns the full match substring.
      -- @function string.gmatch
      -- @tparam string s       The string to search
      -- @tparam string pattern The Lua pattern (must be non-empty)
      -- @treturn function      Iterator yielding captures or full matches
      string.gmatch = function(s, pattern)
        -- argument validation
        if type(s) ~= "string" then
          error("bad argument #1 to 'gmatch' (string expected, got " .. type(s) .. ")", 2)
        end
        if type(pattern) ~= "string" then
          error("bad argument #2 to 'gmatch' (string expected, got " .. type(pattern) .. ")", 2)
        end
        if pattern == "" then
          error("bad argument #2 to 'gmatch' (non-empty string expected)", 2)
        end
        
        -- Security checks
        if #s > MAX_STRING_LENGTH then
          error("string too long", 2)
        end
        if #pattern > MAX_PATTERN_LENGTH then
          error("pattern too long", 2)
        end
        
        validate_pattern(pattern)

        -- current search position (idx is 1-based)
        local pos = 1
        local iteration_count = 0
        local last_pos = 0  -- Track last position to detect zero-width matches

        return function()
          -- DoS protection: prevent infinite loops
          iteration_count = iteration_count + 1
          if iteration_count > MAX_ITERATIONS then
            error("too many iterations", 2)
          end
          
          -- iterator finished once pos passes string length
          if pos > #s then
            return nil
          end

          -- string.find returns: start, stop, capture1, capture2, ...
          -- Use table to handle unlimited captures
          local results = { string.find(s, pattern, pos) }
          local start_idx, stop_idx = results[1], results[2]

          -- no further matches
          if not start_idx then
            return nil
          end

          -- Handle zero-width matches: if we matched at the same position as last time
          -- and the match is zero-width, advance by 1 to prevent infinite loop
          if start_idx == last_pos and start_idx == stop_idx then
            pos = pos + 1
          else
            -- Normal advancement: move past the current match
            pos = math.max(stop_idx + 1, pos + 1)
          end
          
          last_pos = start_idx

          -- if captures exist, yield them; otherwise yield the raw match
          if #results > 2 then
            -- Return all captures (handles any number of captures)
            return table.unpack(results, 3)
          else
            return string.sub(s, start_idx, stop_idx)
          end
        end
      end
  end

  -- Auto-install string extensions when module is loaded
  string_ext.install()

  return string_ext
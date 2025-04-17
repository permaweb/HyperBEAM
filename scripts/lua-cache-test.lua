--- Test functions for ao.cache.* bindings
--- 
-- Writes a map using ao.cache_write
-- @param map_to_write The Lua table/map to write
-- @return The result table from ao.cache_write (e.g., { status = "ok", value = "id_string" })
function write_test(map_to_write)
    if map_to_write == nil then
        return { status = "error", reason = "map_to_write is nil" }
    end
	ao.event("cache_test_lua", {"test_write called", {map=map_to_write}})
	-- ao.cache_write now returns multiple values in Lua (status, value_or_reason)
	-- based on the Erlang list [Status, ValueOrReason]
	local status, value_or_reason = ao.cache_write(map_to_write)
	ao.event("cache_test_lua", {"test_write raw result", {status=status, value_or_reason=value_or_reason}})
  
	if status == "ok" then
	  -- On success, value_or_reason is the ID
	  return { status = status, value = value_or_reason }
	else
	  -- On error, value_or_reason is the ReasonMap
	  return { status = status, reason = value_or_reason }
	end
end
  
-- Reads data using ao.cache_read
-- @param id_or_path The ID or path string
-- @return The result table from ao.cache_read (e.g., { status = "ok", value = map } or {status="not_found"})
function read_test(id_or_path)
    if id_or_path == nil then
        return { status = "error", reason = "id_or_path is nil" }
    end
	ao.event("cache_test_lua", {"test_read called", {id=id_or_path}})
	-- ao.cache_read returns multiple values (status, value_or_reason)
	-- OR just one value ('not_found')
	local status, value_or_reason = ao.cache_read(id_or_path)
	ao.event("cache_test_lua", {"test_read raw result", {status=status, value_or_reason=value_or_reason}})
  
	if status == "ok" then
	   -- Success case: value_or_reason holds the actual data
	  return { status = status, value = value_or_reason }
	elseif status == "not_found" then
	  -- Not found case: Erlang returned [not_found], so Lua receives status="not_found", value_or_reason=nil
	  return { status = status }
	else
	  -- Error case: status="error", value_or_reason=ReasonMap
	  return { status = status, reason = value_or_reason }
	end
end

-- Test the cache functions
function test_cache()
    -- Test writing a map
    local map_to_write = {
        test_key = "test_value"
    }
    local result = write_test(map_to_write)
    ao.event("cache_test_lua", {"test_write result", {result=result}})

    -- Test reading the map
    local id_or_path = result.value
    local read_result = read_test(id_or_path)
    ao.event("cache_test_lua", {"test_read result", {result=read_result}})
end

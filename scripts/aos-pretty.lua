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

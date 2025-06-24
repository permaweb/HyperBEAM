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
-- @param aos The aos environment object
-- @see eval
return function (aos)
  local eval = require(".eval")(aos)
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

function compute(process, message, opts)
	-- early return when no body is provided
	if not message.body.body then
		return process
	end
	-- main logic to initialize the crons
	process.crons = process.crons or {}
	if message.body.body.path == "once" then
		ao.event("debug_cron", { "add once process", "1" })
		table.insert(process.crons, message.body)
	elseif message.body.body.path == "every" then
		ao.event("debug_cron", { "add every process", "2" })
		table.insert(process.crons, message.body)
	elseif message.body.body.path == "stop" then
		ao.event("debug_cron", { "stop process", "3" })
		-- TODO: stop the cron (needs table for loop)
		-- table.remove(process.crons, message.body)
	end
	return process
end

-- test function for cache test in dev_cron.erl
function hello(base, req, opts)
	ao.event("debug_cron", { "base", base })
	ao.event("debug_cron", { "req", req })
	ao.event("debug_cron", { "opts", opts })
    base.hello = req.name or "world"
    return base
end
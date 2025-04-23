-- main handler for cron caching
-- function handle(base, req, opts)
-- 	ao.event("debug_cron", { "handle:base", base })
-- 	ao.event("debug_cron", { "handle:req", req })
-- 	ao.event("debug_cron", { "handle:opts", opts })
--     base.crons = base.crons or {}
--     if req.body.path == "once" then
-- 		ao.event("debug_cron", { "handle:path:once", req.body.path })
--         table.insert(base.crons, req.body)
--     elseif req.body.path == "every" then
-- 		ao.event("debug_cron", { "handle:path:every", req.body.path })
--         table.insert(base.crons, req.body)
--     elseif req.body.path == "stop" then
-- 		ao.event("debug_cron", { "handle:path:stop", req.body.path })
--         -- TODO: Stop the cron
--     end
-- 	base.crons = {
-- 		output = {
-- 			foo = 42
-- 		}
-- 	}
--     return base
-- end

function compute(process, message, opts)
	local res = 42
	process.crons = {
		body = res
    }
	ao.event("debug_cron", { "111compute:message", message.body })
	-- ao.event("debug_cron", { "compute:opts", opts })
	-- if message.body.body and message.body.body.body.path == "once" then
	-- 	ao.event("debug_cron", { "log1", "1" })
	-- 	-- process.crons = {
	-- 	-- 	body = res + 1
	-- 	-- }
	-- else
	-- 	ao.event("debug_cron", { "log2", "2" })
	-- 	-- ao.event("debug_cron", { "2compute:message.body.body", message.body.body })
	-- 	-- process.crons = {
	-- 	-- 	body = res + 2
	-- 	-- }
	-- end
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

-- (currently ignored) main handler for cron caching
function handle(base, req, opts)
	ao.event("debug_cron", { "handle:base", base })
	ao.event("debug_cron", { "handle:req", req })
	ao.event("debug_cron", { "handle:opts", opts })
	base.crons = base.crons or {}
	if req.body.path == "once" then
		ao.event("debug_cron", { "handle:path:once", req.body.path })
		table.insert(base.crons, req.body)
	elseif req.body.path == "every" then
		ao.event("debug_cron", { "handle:path:every", req.body.path })
		table.insert(base.crons, req.body)
	elseif req.body.path == "stop" then
		ao.event("debug_cron", { "handle:path:stop", req.body.path })
		-- TODO(viksit): remove the cron from the table
	end
	base.crons = {
		output = {
			foo = 42
		}
	}
	return base
end

-- TODO: how do we cache this to file system?
function compute(process, message, opts)
    -- Early return when no body is provided
    -- This handles the initial invocation during process setup
    if not message.body or not message.body.body then
        return process
    end
    
    -- Initialize crons table if it doesn't exist
    process.crons = process.crons or {}
    
    local command = message.body.body.path
    ao.event("cron_debug", { "compute command", command })
    
    if command == "once" then
        -- Standard addition to the cache with task_id
        ao.event("cron_debug", { "adding task", message.body.body.task_id })
        table.insert(process.crons, message.body)
        
    elseif command == "every" then
        -- Recurring task addition
        ao.event("cron_debug", { "adding recurring task", message.body.body.task_id })
        table.insert(process.crons, message.body)
        
    elseif command == "remove" then
		-- TODO(viksit): this also needs to be called when a process is stopped. 
		-- how do we do that?
        -- Remove an entry by task_id
        local task_id = message.body.body.task_id
        ao.event("cron_debug", { "removing task", task_id })
        
        -- Find and remove the task with matching task_id
        for i, job in ipairs(process.crons) do
            if job.body and job.body.body and job.body.body.task_id == task_id then
                table.remove(process.crons, i)
                break
            end
        end
        
    elseif command == "clear" then
        -- Clear all entries
        ao.event("cron_debug", { "clearing all tasks" })
        process.crons = {}
        
    elseif command == "stop" then
        -- Stop a specific task
        local task_id = message.body.body.task_id
        ao.event("cron_debug", { "stopping task", task_id })
        
        -- Find and mark the task as stopped
        for i, job in ipairs(process.crons) do
            if job.body and job.body.body and job.body.body.task_id == task_id then
                job.body.body.stopped = true
                break
            end
        end
    end
    
    return process
end

-- (currently ignored) original compute function
function computeOld(process, message, opts)
	-- early return when no body is provided
	-- for some reason, the function is call twice, once without params
	-- likely during schedule.
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
		-- TODO: remove the cron from the table (needs table for loop)
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
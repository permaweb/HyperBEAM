function compute(process, message, opts)
    -- Early return when no body is provided
    -- This handles the initial invocation during process setup
    if not message.body or not message.body.body then
        return process
    end
    
    -- Initialize crons table if it doesn't exist
    process.crons = process.crons or {}
    
    local command = message.body.body.path
    ao.event("debug_cron", { "compute command", command })
    
    if command == "put" then
        -- Standard addition to the cache with task_id
        ao.event("debug_cron", { "adding task", message.body.body.task_id })
        table.insert(process.crons, message.body)
        
    elseif command == "remove" then
        -- Remove an entry by task_id
        local task_id = message.body.body.task_id
        ao.event("debug_cron", { "removing task", task_id, "process.crons", process.crons })
        
        -- Find and remove the task with matching task_id
        for i, job in ipairs(process.crons) do
            if job.body and job.body.task_id == task_id then
                table.remove(process.crons, i)
				ao.event("debug_cron", { "removed task", task_id, "process.crons", process.crons })
                break
            end
        end
        
    elseif command == "clear" then
        -- Clear all entries
        ao.event("debug_cron", { "clearing all tasks" })
        process.crons = {}

    end
    
    return process
end

-- Command handlers
local handle_put_command = function(process, message)
    -- Validate the task_id for put
    if not message.body.body.task_id or type(message.body.body.task_id) ~= "string" then
        ao.event("debug_cron", { "error", "Invalid put: missing/invalid task_id", command_body = message.body.body })
        return process
    end

    if not message.body.body.data or type(message.body.body.data) ~= "table" then
        ao.event("debug_cron", { "error", "Invalid put: missing/invalid data", command_body = message.body.body })
        return process
    end

    ao.event("debug_cron", { "adding task", message.body.body.task_id })
    table.insert(process.crons, message.body)
    return process
end

local handle_remove_command = function(process, message)
    -- Validate the task_id for remove
    if not message.body.body.task_id or type(message.body.body.task_id) ~= "string" then
        ao.event("debug_cron", { "error", "Invalid remove: missing/invalid task_id", command_body = message.body.body })
        return process
    end

    local task_id_to_remove = message.body.body.task_id
    ao.event("debug_cron", { "removing task", task_id_to_remove, "crons_count_before", #process.crons })

    -- Find the index of the task to remove.
    local idx_to_remove = nil
    -- process.crons is a list of maps, each with a "body" key
    -- the task_id is stored in the "body" map
    for i, job in ipairs(process.crons) do
        if job.body and job.body.task_id == task_id_to_remove then
            idx_to_remove = i
            break
        end
    end
    
    -- If an index is found, remove the task. Otherwise, log a warning.
    if idx_to_remove then
        table.remove(process.crons, idx_to_remove)
        ao.event("debug_cron", { "removed task", task_id_to_remove, "crons_count_after", #process.crons })
    else
        ao.event("debug_cron", { "warn", "Task not found for removal", task_id = task_id_to_remove })
    end

    return process
end

local handle_clear_command = function(process)
    ao.event("debug_cron", { "clearing all tasks" })
    process.crons = {}
    collectgarbage()
    ao.event("debug_cron", { "cleared all tasks" })
    return process
end

function compute(process, message, opts)
    -- Early return when no body is provided
    -- This handles the initial invocation during process setup
    if not message or not message.body or not message.body.body then
        return process
    end
    
    -- Validate that the path exists
    if not message.body.body.path or type(message.body.body.path) ~= "string" then
        ao.event("debug_cron", { "error", "Invalid path", command_body = message.body.body })
        return process
    end

    ao.event("debug_cron", { "compute incoming", message })
    
    -- Supported commands: "put" | "remove" | "clear"
    local command = message.body.body.path
    ao.event("debug_cron", { "compute command", command })
    
    -- Initialize the crons table if it doesn't exist
    process.crons = process.crons or {}
    
    -- Command dispatch
    if command == "put" then
        local ok, new_process_or_err = pcall(handle_put_command, process, message)
        if not ok then
            ao.event("debug_cron",
                     { "error", "Error handling put command", error = new_process_or_err })
            return process
        end
        return new_process_or_err
    elseif command == "remove" then
        local ok, new_process_or_err = pcall(handle_remove_command, process, message)
        if not ok then
            ao.event("debug_cron",
                     { "error", "Error handling remove command", error = new_process_or_err })
            return process
        end
        return new_process_or_err
    elseif command == "clear" then
        local ok, new_process_or_err = pcall(handle_clear_command, process)
        if not ok then
            ao.event("debug_cron",
                     { "error", "Error handling clear command", error = new_process_or_err })
            return process
        end
        return new_process_or_err
    else
        ao.event("debug_cron", { "error", "Unknown command received", command = command, command_body = message.body.body })
        return process
    end
end

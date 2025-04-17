-- Command handlers
local function extract_task_id(msg_body)
    if not msg_body then return nil end

    -- Case 1: Direct access when task_id sits under .body (format from handle_put_command)
    if msg_body.body and msg_body.body.task_id then
        return msg_body.body.task_id
    end

    -- Case 2: Nested access for stored format (body.body.task_id)  
    if msg_body.body and msg_body.body.body and msg_body.body.body.task_id then
        return msg_body.body.body.task_id
    end

    if msg_body.task_id then
        return msg_body.task_id
    end

    return nil
end

-- Helper function to find job index by task_id
local function find_job_index(process, task_id)
    for i, job in ipairs(process.crons) do
        local existing_id = extract_task_id(job)
        if existing_id == task_id then
            return i
        end
    end
    return nil
end

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
    
    local incoming_id = message.body.body.task_id
    -- If a cron with the same task_id already exists we
    -- simply replace it and return, avoiding duplicates when the node is
    -- normalised after a reboot.
    local idx_to_replace = find_job_index(process, incoming_id)
    
    -- If we found a replacement index, replace the job. 
    -- Otherwise, insert as new.
    if idx_to_replace then
        process.crons[idx_to_replace] = message.body
    else
        table.insert(process.crons, message.body)
    end
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
    local idx_to_remove = find_job_index(process, task_id_to_remove)
    
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

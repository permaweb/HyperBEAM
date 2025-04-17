function handle(base, req, opts)
    base.crons = base.crons or {}
    if req.body.path == "once" then
        table.insert(base.crons, req.body)
    elseif req.body.path == "every" then
        table.insert(base.crons, req.body)
    elseif req.body.path == "stop" then
        -- TODO: Stop the cron
    end
    return base
end
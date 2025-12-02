-- Append characters from scheduled messages in order.
-- Each scheduled assignment should include `char` in its body.

local function ensure_state(base)
    base.acc = base.acc or ""
    return base
end

function compute(base, assignment)
    base = ensure_state(base)
    local ch = assignment.body and assignment.body.char or ""
    base.acc = base.acc .. (ch or "")
    base.results = { status = "ok", acc = base.acc, slot = assignment.slot }
    return "ok", base
end

function snapshot(base, assignment)
    -- no special snapshot logic; reuse current base
    return "ok", ensure_state(base)
end

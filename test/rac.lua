-- Test process for `~rac@1.0`, driven by `~process@1.0` + `~lua@5.3a`.
--
-- On each computed message the process either emits an outbound message
-- through a rac channel (`action = "send"`) or gates an inbound message
-- through the rac ratchet. Admitted inbound messages advance `applied-count`;
-- rejected ones leave state untouched. Both paths call `~rac@1.0` directly via
-- `ao.resolve({"as", "rac@1.0", ...})`, mirroring the "called directly" usage.

function compute(base, req)
    local body = req.body or {}

    if body.action == "send" then
        -- Emit an outbound message on a rac channel.
        local status, nb =
            ao.resolve(
                base,
                {"as", "rac@1.0",
                    {
                        path = "send",
                        recipient = body.recipient,
                        body = { note = body.note }
                    }
                }
            )
        nb.device = "process@1.0"
        return "ok", nb
    end

    -- Gate the inbound message through the ratchet.
    local status, gated =
        ao.resolve(
            base,
            {"as", "rac@1.0",
                { path = "compute", body = req.body }
            }
        )
    gated.device = "process@1.0"
    if status == "ok" then
        -- Admitted: record that we applied it.
        gated["applied-count"] = (gated["applied-count"] or 0) + 1
        return "ok", gated
    else
        -- Rejected: discard, leaving state unchanged.
        return "ok", base
    end
end

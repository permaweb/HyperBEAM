function compute(base, req, opts)
    base.results = {
        output = {
            body = 42
        }
    }
    return base
end

function hello_world(base, req, opts)
    base.animals = base.animals or {}
    base.animals.output = {
        names = {
            "rainbows",
            "unicorns"
        }
    }
    return base
end

function animals_count(base, req, opts)
    local names = {}
    if base.animals and base.animals.output and base.animals.output.names then
        names = base.animals.output.names
    end
    base.results = base.results or {}
    base.results.count = #names
    return base
end

-- address
-- BO9CdGPLR4MZ3np-KqJBq_LdDChk8uZM8GhnDGxwSLU
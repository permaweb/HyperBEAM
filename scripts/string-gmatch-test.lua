-- Load and execute hyper-aos.lua to get all the modules
dofile("test/hyper-aos.lua")

-- Test function that uses the monkey-patched string.gmatch from hyper-aos
-- The string.gmatch is automatically installed when string-ext module is loaded
function run_string_gmatch()
    local results = {}
    for word in string.gmatch('hello world test', '%w+') do
        table.insert(results, word)
    end
    return results
end

function run_alpha_gmatch()
    local results = {}
    for word in string.gmatch("Hello Lua user", "%a+") do
        table.insert(results, word)
    end
    return results
end

function run_words_gmatch()
    local results = {}
    local text = "The quick brown fox jumps over the lazy dog"
    for word in string.gmatch(text, "%w+") do
        table.insert(results, word)
    end
    return results
end

function run_numbers_gmatch()
    local results = {}
    local numbers = "Price: $25.99, Tax: $3.50, Total: $29.49"
    for number in string.gmatch(numbers, "%d+%.?%d*") do
        table.insert(results, number)
    end
    return results
end

function run_groups_gmatch()
    local results = {}
    local data = "John:25:Engineer Mary:30:Designer"
    for name, age, job in string.gmatch(data, "(%w+):(%d+):(%w+)") do
        table.insert(results, {name = name, age = age, job = job})
    end
    return results
end
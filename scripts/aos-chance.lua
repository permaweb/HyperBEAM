--- The Chance module provides utilities for generating random numbers and values. Returns the chance table.
-- @module chance

local N = 624
local M = 397
local MATRIX_A = 0x9908b0df
local UPPER_MASK = 0x80000000
local LOWER_MASK = 0x7fffffff

--- Initializes mt[N] with a seed
-- @lfunction init_genrand
-- @tparam {table} o The table to initialize
-- @tparam {number} s The seed
local function init_genrand(o, s)
    o.mt[0] = s & 0xffffffff
    for i = 1, N - 1 do
        o.mt[i] = (1812433253 * (o.mt[i - 1] ~ (o.mt[i - 1] >> 30))) + i
        -- See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier.
        -- In the previous versions, MSBs of the seed affect
        -- only MSBs of the array mt[].
        -- 2002/01/09 modified by Makoto Matsumoto
        o.mt[i] = o.mt[i] & 0xffffffff
        -- for >32 bit machines
    end
    o.mti = N
end

--- Generates a random number on [0,0xffffffff]-interval
-- @lfunction genrand_int32
-- @tparam {table} o The table to generate the random number from
-- @treturn {number} The random number
local function genrand_int32(o)
    local y
    local mag01 = {} -- mag01[x] = x * MATRIX_A  for x=0,1
    mag01[0] = 0x0
    mag01[1] = MATRIX_A
    if o.mti >= N then  -- generate N words at one time
        if o.mti == N + 1 then -- if init_genrand() has not been called,
            init_genrand(o, 5489)   -- a default initial seed is used
        end
        for kk = 0, N - M - 1 do
            y = (o.mt[kk] & UPPER_MASK) | (o.mt[kk + 1] & LOWER_MASK)
            o.mt[kk] = o.mt[kk + M] ~ (y >> 1) ~ mag01[y & 0x1]
        end
        for kk = N - M, N - 2 do
            y = (o.mt[kk] & UPPER_MASK) | (o.mt[kk + 1] & LOWER_MASK)
            o.mt[kk] = o.mt[kk + (M - N)] ~ (y >> 1) ~ mag01[y & 0x1]
        end
        y = (o.mt[N - 1] & UPPER_MASK) | (o.mt[0] & LOWER_MASK)
        o.mt[N - 1] = o.mt[M - 1] ~ (y >> 1) ~ mag01[y & 0x1]

        o.mti = 0
    end

    y = o.mt[o.mti]
    o.mti = o.mti + 1

    -- Tempering
    y = y ~ (y >> 11)
    y = y ~ ((y << 7) & 0x9d2c5680)
    y = y ~ ((y << 15) & 0xefc60000)
    y = y ~ (y >> 18)

    return y
end

local MersenneTwister = {}
MersenneTwister.mt = {}
MersenneTwister.mti = N + 1


--- The Random table
-- @table Random
-- @field seed The seed function
-- @field random The random function
-- @field integer The integer function
local Random = {}

--- Sets a new random table given a seed.
-- @function seed
-- @tparam {number} seed The seed
function Random.seed(seed)
    init_genrand(MersenneTwister, seed)
end

--- Generates a random number on [0,1)-real-interval.
-- @function random
-- @treturn {number} The random number
function Random.random()
    return genrand_int32(MersenneTwister) * (1.0 / 4294967296.0)
end

--- Returns a random integer. The min and max are INCLUDED in the range.
-- The max integer in lua is math.maxinteger
-- The min is math.mininteger
-- @function Random.integer
-- @tparam {number} min The minimum value
-- @tparam {number} max The maximum value
-- @treturn {number} The random integer
function Random.integer(min, max)
    assert(max >= min, "max must bigger than min")
    return math.floor(Random.random() * (max - min + 1) + min)
end

return Random
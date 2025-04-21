-- load hyperAOS
require('../test/hyper-aos')

function hello_test()
    local state = {
        process = {
            commitments = {
                PROCESS = {
                    alg = "rsa-pss-sha512",
                    committer = "OWNER"
                }
            }
        }
    }

    local req = {
        path = "schedule",
        method = "POST",
        body = {
            target = "PROCESS",
            data = "1 + 1",
            action = "Eval",
            commitments = {
                MSG = {
                    alg = "rsa-pss-sha512",
                    committer = "OWNER"
                }
            }
        }
    }
    state = compute(state, req)
    assert(state.results.output.data == "2", "should equal 2")
    return "ok"
end
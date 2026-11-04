%%% @doc Any-Honest time-lock interop vectors for `rng@1.0'.
%%%
%%% BLS: produced by the beacon's own threshold-BLS code (Random-Process
%%% `v2/provider/bls.mjs', noble-curves 2.4.0 `bls12_381.longSignatures'): a
%%% 2-of-3 joint-Feldman DKG, a group signature combined from two partials, one
%%% member's own signature, and invalid cases. noble agreed with every `valid'
%%% flag when they were generated. The live round is round 1 of the beacon
%%% process `3K_0gcFJixOwe5vsewbHztrzTyfImWxjHRfHwmbtz6A', with the value it
%%% published.
%%%
%%% Time-lock: produced by the reference implementation (Random-Process
%%% `provider/timelock.mjs', generator `test/gen-timelock-vectors.mjs') in the
%%% built-in RSA-2048 group with T = 1024: the generator, reveals (c, alpha, h)
%%% and Wesolowski cracks (x, t, y, proof), valid and invalid. The reference
%%% agreed with every `valid' flag when they were generated.
-module(dev_rng_timelock_test_vectors).
-export([bls_vectors/0, live_round/0]).
-export([timelock_vectors/0, fixed_challenge_prime/0]).

%% @doc The generated vectors, as `{Name, Inputs, Valid}'.
bls_vectors() ->
    [
        {
            <<"group-sig">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "abb36051e68131a7621b302eb8d1d99083651546414546546dfa581966f9d6a8"
                        "57aee681217a298a81bb726801e4faaf166a988fe4a0e15edbfb1e714b4f3f90"
                        "a5ff01a596e40809de26b335f1c74a1bd9169daa79fc848c7f92e9332f0a7ff0"
                    >>
            },
            true
        },
        {
            <<"group-sig-round-2">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|2">>,
                <<"signature">> =>
                    <<
                        "97b006d98c3e7102e4b12010629256e2097159b3ea3553a12f7f19b22091fb1a"
                        "64553eadc62d6e9f4c4ec25d651b63ed0d231a94dac7d8b11005dd644844acf5"
                        "2a5f627a1b3874452280a920edd0d79a232bd0788a0a5085236a70043c0f7f88"
                    >>
            },
            true
        },
        {
            <<"single-key-sig">>,
            #{
                <<"public-key">> =>
                    <<
                        "b52db73744c3a6e8b1fef213082ec5a48448390e76c2ed42a35f55d15e9c5a87"
                        "889ac2f50c9bdccd864569d091096c30"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "a035b2e8130278dffd067fca21b95863c01bdfa5bc669ac0914e11f7f4343392"
                        "75f3104fe0a696f7275b21f2812598050eee8eee504fbe6a2197c63b0c654985"
                        "b27f6d71f18c43865663a5526693a3038d678ed2d73bf931c48ca1fa3cb2bf67"
                    >>
            },
            true
        },
        {
            <<"wrong-message">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|2">>,
                <<"signature">> =>
                    <<
                        "abb36051e68131a7621b302eb8d1d99083651546414546546dfa581966f9d6a8"
                        "57aee681217a298a81bb726801e4faaf166a988fe4a0e15edbfb1e714b4f3f90"
                        "a5ff01a596e40809de26b335f1c74a1bd9169daa79fc848c7f92e9332f0a7ff0"
                    >>
            },
            false
        },
        {
            <<"wrong-key">>,
            #{
                <<"public-key">> =>
                    <<
                        "8bd88ff3ba9b8689783ea765ac6c354fd75463311de265dd9c77602f88bde5f1"
                        "c61c806130d1467dd5b50977c1570d7e"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "abb36051e68131a7621b302eb8d1d99083651546414546546dfa581966f9d6a8"
                        "57aee681217a298a81bb726801e4faaf166a988fe4a0e15edbfb1e714b4f3f90"
                        "a5ff01a596e40809de26b335f1c74a1bd9169daa79fc848c7f92e9332f0a7ff0"
                    >>
            },
            false
        },
        {
            <<"partial-under-group-key">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "a035b2e8130278dffd067fca21b95863c01bdfa5bc669ac0914e11f7f4343392"
                        "75f3104fe0a696f7275b21f2812598050eee8eee504fbe6a2197c63b0c654985"
                        "b27f6d71f18c43865663a5526693a3038d678ed2d73bf931c48ca1fa3cb2bf67"
                    >>
            },
            false
        },
        {
            <<"corrupted-sig">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "abb36051e68131a7621b302eb8d1d99083651546414546546dfa581966f9d6a8"
                        "57aee681217a298a81bb726801e4faaf166a988fe4a0e15edbfb1e714b4f3f90"
                        "a5ff01a596e40809de26b335f1c74a1bd9169daa79fc848c7f92e9332f0a7ff1"
                    >>
            },
            false
        },
        {
            <<"non-subgroup-sig">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "a000000000000000000000000000000000000000000000000000000000000000"
                        "0000000000000000000000000000000100000000000000000000000000000000"
                        "0000000000000000000000000000000000000000000000000000000000000001"
                    >>
            },
            false
        },
        {
            <<"non-subgroup-key">>,
            #{
                <<"public-key">> =>
                    <<
                        "8000000000000000000000000000000000000000000000000000000000000000"
                        "00000000000000000000000000000004"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "abb36051e68131a7621b302eb8d1d99083651546414546546dfa581966f9d6a8"
                        "57aee681217a298a81bb726801e4faaf166a988fe4a0e15edbfb1e714b4f3f90"
                        "a5ff01a596e40809de26b335f1c74a1bd9169daa79fc848c7f92e9332f0a7ff0"
                    >>
            },
            false
        },
        {
            <<"infinity-key">>,
            #{
                <<"public-key">> =>
                    <<
                        "c000000000000000000000000000000000000000000000000000000000000000"
                        "00000000000000000000000000000000"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "abb36051e68131a7621b302eb8d1d99083651546414546546dfa581966f9d6a8"
                        "57aee681217a298a81bb726801e4faaf166a988fe4a0e15edbfb1e714b4f3f90"
                        "a5ff01a596e40809de26b335f1c74a1bd9169daa79fc848c7f92e9332f0a7ff0"
                    >>
            },
            false
        },
        {
            <<"infinity-sig">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "c000000000000000000000000000000000000000000000000000000000000000"
                        "0000000000000000000000000000000000000000000000000000000000000000"
                        "0000000000000000000000000000000000000000000000000000000000000000"
                    >>
            },
            false
        },
        {
            <<"garbage-sig">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    >>
            },
            false
        },
        {
            <<"zero-sig">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "0000000000000000000000000000000000000000000000000000000000000000"
                        "0000000000000000000000000000000000000000000000000000000000000000"
                        "0000000000000000000000000000000000000000000000000000000000000000"
                    >>
            },
            false
        },
        {
            <<"short-sig">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "abb36051e68131a7621b302eb8d1d99083651546414546546dfa581966f9d6a8"
                        "57aee681217a298a81bb726801e4faaf166a988fe4a0e15edbfb1e714b4f3f90"
                        "a5ff01a596e40809de26b335f1c74a1bd9169daa79fc848c7f92e9332f0a7f"
                    >>
            },
            false
        },
        {
            <<"short-key">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "abb36051e68131a7621b302eb8d1d99083651546414546546dfa581966f9d6a8"
                        "57aee681217a298a81bb726801e4faaf166a988fe4a0e15edbfb1e714b4f3f90"
                        "a5ff01a596e40809de26b335f1c74a1bd9169daa79fc848c7f92e9332f0a7ff0"
                    >>
            },
            false
        },
        {
            <<"non-hex-sig">>,
            #{
                <<"public-key">> =>
                    <<
                        "91a35f0fb83204dfd393d73d5f12f52b558a557f193bd992e012c520aff4d26a"
                        "ed07a6f9500e91d04279501928500a7f"
                    >>,
                <<"message">> => <<"Xk3_9vQ2mZpL7aRtYw4Hn1cBfGdJs8uEoVq6iT0yK5A|1">>,
                <<"signature">> =>
                    <<
                        "zzb36051e68131a7621b302eb8d1d99083651546414546546dfa581966f9d6a8"
                        "57aee681217a298a81bb726801e4faaf166a988fe4a0e15edbfb1e714b4f3f90"
                        "a5ff01a596e40809de26b335f1c74a1bd9169daa79fc848c7f92e9332f0a7ff0"
                    >>
            },
            false
        }
    ].

%% @doc A live beacon round: the device's inputs and the round value.
live_round() ->
    {
        #{
            <<"public-key">> =>
                <<
                    "8085ed55cb08a9193f3ef23ee16b3212033aa5f228402508502667d381d0b4e6"
                    "65ef32948a248634faa796a37e3a4658"
                >>,
            <<"message">> => <<"3K_0gcFJixOwe5vsewbHztrzTyfImWxjHRfHwmbtz6A|1">>,
            <<"signature">> =>
                <<
                    "ae5cd6b763bcd4da954213976bab879b0643b8cb0ec6a19df4784c27cb1e8567"
                    "bd842af437905885ba1cfc2771883f4b067fb4895b7936ca0925d794079b0de7"
                    "c61489604d1e2a543ddea5eadb707c4ce312d7b2f0c839c5c6e53459f959fb94"
                >>
        },
        <<"28d9ea02796189a835d3ca071dc4fd0534062381268eb7d1153c9bba55e25529">>
    }.

%% @doc The generated time-lock vectors, as `{Name, Key, Inputs, Expected}'.
timelock_vectors() ->
    [
        {
            <<"generator">>,
            <<"generator">>,
            #{},
            #{
                <<"valid">> => true,
                <<"g">> =>
                <<
                    "af837d388770d250471f03e561cef2f1d2793a166c01b582123ab1d418a26533"
                    "1ffbe5bcf204f7d4d1a9d880feac19f16f45e7d03a6df66a763cc524106e1b88"
                    "1afbb78b92994e69f0bcaee1ceb87688447e19e643cad5de61dcb6039d0f84a8"
                    "1ebe1372e718fdd42447b48b0df11e6a7d704279a01b2eb7083769b38fea7b1f"
                    "b35c161137e9b570ce49d6eb9185c424ad8e79b06a9e1edfc185982efc755c9e"
                    "4cdf4961ddea042df9aa94383fb8aa720df097a738e3b6b6f5a331077a8d878a"
                    "e424892a911fb0cb8030f29fb3edd3115af0fc88e1c00fafca3d6f10b0a28cf3"
                    "dbcecff52398f6b53f00ac1622a0d72696aeba05ad8d817ebc31b8e3e159a2e8"
                >>,
                <<"n">> =>
                <<
                    "c7970ceedcc3b0754490201a7aa613cd73911081c790f5f1a8726f463550bb5b"
                    "7ff0db8e1ea1189ec72f93d1650011bd721aeeacc2acde32a04107f0648c2813"
                    "a31f5b0b7765ff8b44b4b6ffc93384b646eb09c7cf5e8592d40ea33c80039f35"
                    "b4f14a04b51f7bfd781be4d1673164ba8eb991c2c4d730bbbe35f592bdef524a"
                    "f7e8daefd26c66fc02c479af89d64d373f442709439de66ceb955f3ea37d5159"
                    "f6135809f85334b5cb1813addc80cd05609f10ac6a95ad65872c909525bdad32"
                    "bc729592642920f24c61dc5b3c3b7923e56b16a4d9d373d8721f24a3fc0f1b31"
                    "31f55615172866bccc30f95054c824e733a5eb6817f7bc16399d48c6361cc7e5"
                >>
            }
        },
        {
            <<"t1024-reveal-valid">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => true,
                <<"y">> =>
                <<
                    "61a4f43f07da2652294defcb6a55589639714799863b8573864e26275047f5b7"
                    "7a38a8dc84820a8c1f4c8335eb679ef862b8dda62188e57a3f0e9d46dde32501"
                    "a5e04f0cf8d0642a32fd152b55430ffd2fb2a9cd38681d4bea66e2e96a97d245"
                    "89ea8ad301947dc4640f4e4024f72b27d8e812c9d8f0855feaa74f066bd60040"
                    "eb8236bd55cba516418d161e6f1e8ec7a283d0e8b886bed3ccb4cbbe34e263b6"
                    "f715204641be91a815e78c97d61dda47deaf8ce856fde4d459ca0cb96c2479e2"
                    "398692b217308cd08b2372afc6b7df6b1feed26e08dc41d55de559753b3d81f5"
                    "cccb45207010ee78606756092b4ee3092a5fd893edfcde96f4af3ea76fbbccec"
                >>
            }
        },
        {
            <<"t1024-reveal-valid-512-bit-alpha">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "aa1ff7341f4d3a49a45957cf651c78a90b3fb0690cf52a03efa44bbfdf2ccfdf"
                    "f9db05172e80d46b1cdb884d1c9343e185dab1ceea113e9686fd2d2dae7b2c73"
                    "39bfa11de27fc4af5fce75fa53d8895a3a78fe94b61f014dcb4d630f78da7512"
                    "e39e5c2b73977c2fd67f8df9905b146f7f6e1967d866959309f580c86b624f64"
                    "b3fab162d86a3acdc7371acda84046725991945927a20ac7cb1c013cc53fea13"
                    "4a694b58ba1d19e30b9dbff48e0aac4a5b5b8d52819587517473c6c0069db8d2"
                    "16204819b45abff770026292ad1bbd95d4f1c8e07886ac97b24485eec1527810"
                    "5006c1fc5653c72d51e82f6ba1514bd03c1fd828a33d7ef778a46fbd75255d5b"
                >>,
                <<"alpha">> =>
                <<
                    "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                >>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => true,
                <<"y">> =>
                <<
                    "58154b6df9c03d8c42ef123d10ec0afae2fe86f99333986dc48989d2d45af73e"
                    "052ad41db3e396a11b45ce9e4981234e557adcb890d7ae2738609cdab9ec8a5a"
                    "d67b1da4f8871a493ab0aa74ec2a939afd42eaf354c743331cd1b362e8a0acd3"
                    "46e67c2fd69fa9dc54f589d62ae870e21e95bf9a77619272e67172615ad39ab1"
                    "0ab5252b982bbcc5e3a86c79d77fa60fc861959c7ce83628e5bd3d4dabd23503"
                    "b4a3e7603737c9717bfb7ee99b67dabd408939e3cbf97f9c7c57ba5dff6b9331"
                    "f86c3c1921768769d17e4b58bf002df2e9aa7248916276e96e8ff8103256302d"
                    "09f15582a5803349369e57624e3a459af3d110dc65191c8a7b02edbd3fd8230e"
                >>
            }
        },
        {
            <<"t1024-reveal-valid-alpha-0">>,
            <<"verify-reveal">>,
            #{
                <<"c">> => <<"1">>,
                <<"alpha">> => <<"0">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => true,
                <<"y">> => <<"1">>
            }
        },
        {
            <<"t1024-reveal-valid-other-h">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e707"
                >>
            },
            #{
                <<"valid">> => true,
                <<"y">> =>
                <<
                    "488369e3dac0e186b9e40e30cfd92e246174943ee3087f8973b267c34d3b49f1"
                    "fb428bae4a150129694f9264e4db2b533314c62e3598d055f152529bbaa235e9"
                    "15c31d67d4c7f56e8041312d985beeff91c609a01a003bbde70bbcfc8fe5d5b2"
                    "dc4f877156e88327098e81c53c3ee2fe8a2f8cd17e5caab399f4bb266423f324"
                    "263cc0e556eb20b492a16d71c489930945e6b513c6b9bce8e00ca37977a7eafe"
                    "76d60fc208bde892b16647711737ac60180b927c1e2ddc0a598455a101c23fc2"
                    "fb499d13ec645533e9f1daf2d10fc53c9eb003a3d193adff684a2e98dd9d8ecb"
                    "7d8fca04a03d939f7774968ae9685fc0d6ec25a5e96ddfc0f311dc1a3c42847f"
                >>
            }
        },
        {
            <<"t1024-reveal-wrong-alpha">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"alpha">> => <<"45562f42bcd716d6d479d989c0f5dee9962417053422ea7300d1fb5dcccd0090">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-alpha-plus-1">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff2">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-wrong-c">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f237"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-c-is-0">>,
            <<"verify-reveal">>,
            #{
                <<"c">> => <<"0">>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-c-is-n">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "c7970ceedcc3b0754490201a7aa613cd73911081c790f5f1a8726f463550bb5b"
                    "7ff0db8e1ea1189ec72f93d1650011bd721aeeacc2acde32a04107f0648c2813"
                    "a31f5b0b7765ff8b44b4b6ffc93384b646eb09c7cf5e8592d40ea33c80039f35"
                    "b4f14a04b51f7bfd781be4d1673164ba8eb991c2c4d730bbbe35f592bdef524a"
                    "f7e8daefd26c66fc02c479af89d64d373f442709439de66ceb955f3ea37d5159"
                    "f6135809f85334b5cb1813addc80cd05609f10ac6a95ad65872c909525bdad32"
                    "bc729592642920f24c61dc5b3c3b7923e56b16a4d9d373d8721f24a3fc0f1b31"
                    "31f55615172866bccc30f95054c824e733a5eb6817f7bc16399d48c6361cc7e5"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-c-plus-n">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "cd4133154484055caf09a2001404e7d470738c4a9e05403de8c2f1ae95875a36"
                    "c6472ff4f12e15f4bd6c3cb5dbe5e9caf03c08371fdfe54bb3a3a18d4079d231"
                    "7a71dce91d7beaeaee0ba83afe2e24d9a05e1bc9254c9c0cb93c0f63bfeef83e"
                    "bacf0fa9431b4ba147bf1e31c4da8679c4c714e005f9ed417cf0eac7ad06602a"
                    "06dd040814e7633f65482af6764e71d6ca0d65dd2f85cfb33db4fdf76832b17f"
                    "49da6a20ec6fcfe4eac7007be3b88e141ba4a4a34e65fdb5b7155275db784043"
                    "b769af04937492b5e122d677e572b4e4c56cf0efbfeccac684693ecdfc96e74f"
                    "354ada9f70f87a5e27be763d1d990bc4e5995e33daecf87c9ed91fbc7f56ba1b"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-h-is-0">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> => <<"0">>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-h-is-n">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "c7970ceedcc3b0754490201a7aa613cd73911081c790f5f1a8726f463550bb5b"
                    "7ff0db8e1ea1189ec72f93d1650011bd721aeeacc2acde32a04107f0648c2813"
                    "a31f5b0b7765ff8b44b4b6ffc93384b646eb09c7cf5e8592d40ea33c80039f35"
                    "b4f14a04b51f7bfd781be4d1673164ba8eb991c2c4d730bbbe35f592bdef524a"
                    "f7e8daefd26c66fc02c479af89d64d373f442709439de66ceb955f3ea37d5159"
                    "f6135809f85334b5cb1813addc80cd05609f10ac6a95ad65872c909525bdad32"
                    "bc729592642920f24c61dc5b3c3b7923e56b16a4d9d373d8721f24a3fc0f1b31"
                    "31f55615172866bccc30f95054c824e733a5eb6817f7bc16399d48c6361cc7e5"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-alpha-513-bits">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "57faec46326d93787a9c36bba35b8f7c0d220a865891db8c975384a7c79c1f13"
                    "392f206393c44f7faa7af4cceeb5bdbb356b491720327aff24cff6f82c54add7"
                    "93fdaa2aab0483293bfd3c470605f013f4cdba33cfcf53136f5def7efe049f81"
                    "91c37bef9071e28c87658504a45c99c843c16c6556f23509e08b934a6f7e328a"
                    "19d40d27da9ddef4e2d18ebd8067b9255ea38b94a31d1f098bce278033c71ead"
                    "c7744e6248065449a4910b31500ef9c281168433cd5d0efaa3dd45579a8c92f4"
                    "5f0ca7fc786e0a64454c3d64cd20a1e67a6873833bc7276d12ed35b061e9ff9f"
                    "1b4e9ed775ab7065d5a6c958c2df82468a222a2aaba67046874962d8d4a87436"
                >>,
                <<"alpha">> =>
                <<
                    "1000000000000000000000000000000000000000000000000000000000000000"
                    "0000000000000000000000000000000000000000000000000000000000000000"
                    "0"
                >>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-garbage-alpha">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"alpha">> => <<"xyz">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-garbage-c">>,
            <<"verify-reveal">>,
            #{
                <<"c">> => <<"zz">>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-uppercase-c">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "5AA262667C054E76A7981E5995ED406FCE27BC8D6744A4C4050826860369EDB4"
                    "6565466D28CFD55F63CA8E476E5D80D7E21198A5D3307191362999CDBEDAA1DD"
                    "75281DDA615EB5FA956F13B34FAA0235973120155EE1679E52D6C273FEB59090"
                    "5DDC5A48DFBCFA3CFA339605DA921BF360D831D4122BC85BEBAF534EF170DDF0"
                    "EF42918427AFC436283B146EC78249F8AC93ED3EBE7E946521F9EB8C4B560255"
                    "3C71216F41C9B2F1FAEECCE0737C10EBB0593F6E3D050502FE8C1E0B5BA9310F"
                    "AF719722F4B71C394C0FA1CA9373BC0E001DA4AE61956EE124A1A2A0087CC1E0"
                    "355848A59D013A15B8D7CECC8D0E6DDB1F372CBC2F53C66653BD6F64939F236"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-reveal-leading-zero-c">>,
            <<"verify-reveal">>,
            #{
                <<"c">> =>
                <<
                    "05aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb"
                    "46565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1d"
                    "d75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb5909"
                    "05ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf"
                    "0ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b56025"
                    "53c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310"
                    "faf719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e"
                    "0355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"alpha">> => <<"b5f4cb336f922d3cabd589d00193854e6fa44bf7c7cc9966b7409de28dfb9ff1">>,
                <<"h">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-valid">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => true,
                <<"y">> =>
                <<
                    "61a4f43f07da2652294defcb6a55589639714799863b8573864e26275047f5b7"
                    "7a38a8dc84820a8c1f4c8335eb679ef862b8dda62188e57a3f0e9d46dde32501"
                    "a5e04f0cf8d0642a32fd152b55430ffd2fb2a9cd38681d4bea66e2e96a97d245"
                    "89ea8ad301947dc4640f4e4024f72b27d8e812c9d8f0855feaa74f066bd60040"
                    "eb8236bd55cba516418d161e6f1e8ec7a283d0e8b886bed3ccb4cbbe34e263b6"
                    "f715204641be91a815e78c97d61dda47deaf8ce856fde4d459ca0cb96c2479e2"
                    "398692b217308cd08b2372afc6b7df6b1feed26e08dc41d55de559753b3d81f5"
                    "cccb45207010ee78606756092b4ee3092a5fd893edfcde96f4af3ea76fbbccec"
                >>,
                <<"l">> => <<"a0df8b3eb1e9e0ff5537658c7b3de863">>
            }
        },
        {
            <<"t1024-crack-valid-n-minus-y">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "61a4f43f07da2652294defcb6a55589639714799863b8573864e26275047f5b7"
                    "7a38a8dc84820a8c1f4c8335eb679ef862b8dda62188e57a3f0e9d46dde32501"
                    "a5e04f0cf8d0642a32fd152b55430ffd2fb2a9cd38681d4bea66e2e96a97d245"
                    "89ea8ad301947dc4640f4e4024f72b27d8e812c9d8f0855feaa74f066bd60040"
                    "eb8236bd55cba516418d161e6f1e8ec7a283d0e8b886bed3ccb4cbbe34e263b6"
                    "f715204641be91a815e78c97d61dda47deaf8ce856fde4d459ca0cb96c2479e2"
                    "398692b217308cd08b2372afc6b7df6b1feed26e08dc41d55de559753b3d81f5"
                    "cccb45207010ee78606756092b4ee3092a5fd893edfcde96f4af3ea76fbbccec"
                >>,
                <<"proof">> =>
                <<
                    "3c18db7483321a84e16d0007d0ae46262c4b5e6962042851b74ae8ccfc234849"
                    "5ad70eee561ffe5e6e974fc03b2df48e4643842d42620451c104c38a0ac6b198"
                    "776a03ee9ce264003f072de9f30726f38c6252ba93d252ead9fc68ba5ba418d8"
                    "ef175d5dada5b5b0ad4fcf74052f49e3437a0d6118ed2d930adcc15cc36c575b"
                    "58ba204c09c1162cc24f4d4ee68472d013b85dac8eea822dbd29c4e91365fe19"
                    "28398e02d45b47f3c33ce5f6df085512650ea80f10f17f8cf21e70390d37704d"
                    "fdabf2b33e00986e1c2b6f6369449277c7240f0599e4cf5e6f8eb77a6e89c39e"
                    "0c8b4df846ed701ba8c58c299b613f60be1ae69b49c6c2c0b6df759148c65b3d"
                >>
            },
            #{
                <<"valid">> => true,
                <<"y">> =>
                <<
                    "61a4f43f07da2652294defcb6a55589639714799863b8573864e26275047f5b7"
                    "7a38a8dc84820a8c1f4c8335eb679ef862b8dda62188e57a3f0e9d46dde32501"
                    "a5e04f0cf8d0642a32fd152b55430ffd2fb2a9cd38681d4bea66e2e96a97d245"
                    "89ea8ad301947dc4640f4e4024f72b27d8e812c9d8f0855feaa74f066bd60040"
                    "eb8236bd55cba516418d161e6f1e8ec7a283d0e8b886bed3ccb4cbbe34e263b6"
                    "f715204641be91a815e78c97d61dda47deaf8ce856fde4d459ca0cb96c2479e2"
                    "398692b217308cd08b2372afc6b7df6b1feed26e08dc41d55de559753b3d81f5"
                    "cccb45207010ee78606756092b4ee3092a5fd893edfcde96f4af3ea76fbbccec"
                >>,
                <<"l">> => <<"cde722691f3d0c5a85f2e7f88d832f01">>
            }
        },
        {
            <<"t1024-crack-valid-h-proof">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "af837d388770d250471f03e561cef2f1d2793a166c01b582123ab1d418a26533"
                    "1ffbe5bcf204f7d4d1a9d880feac19f16f45e7d03a6df66a763cc524106e1b88"
                    "1afbb78b92994e69f0bcaee1ceb87688447e19e643cad5de61dcb6039d0f84a8"
                    "1ebe1372e718fdd42447b48b0df11e6a7d704279a01b2eb7083769b38fea7b1f"
                    "b35c161137e9b570ce49d6eb9185c424ad8e79b06a9e1edfc185982efc755c9e"
                    "4cdf4961ddea042df9aa94383fb8aa720df097a738e3b6b6f5a331077a8d878a"
                    "e424892a911fb0cb8030f29fb3edd3115af0fc88e1c00fafca3d6f10b0a28cf3"
                    "dbcecff52398f6b53f00ac1622a0d72696aeba05ad8d817ebc31b8e3e159a2e8"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "7ef5ebb350910493259035f808693f58ca3122d328c97c074e995a01607b0dfc"
                    "2b5b192430a0d6b65fabf76d57699a4e6f13145a03bfb44aefa10e3ddcbfeec8"
                    "478a343d175cf949c7c8bc29e0db8dbed568c74555075b3e4295d9daae6a349a"
                    "c3b3abb38b3b96d228219512d3b60f9b1087acf1a6b2cdbc4baff7b090ecac98"
                    "17dcd5b16534221521d60e0a83a98ae9a8a40617c7f4725925f5dd212d8a00ea"
                    "4c0a108e7519c934296bc410dd60566a53152cbc189584558615cef8c7915dfd"
                    "689075995f6116ce70cf49fd4f51e4f587e67f4b5b703e96dcaca8d727c2aa4c"
                    "e2799182fb24e05ce781b6f8d169f08422ebfb2a1fa8f31c182b958defe3e706"
                >>,
                <<"proof">> =>
                <<
                    "b00fda88c6f129e42d4e79b079015387040f264c723f6f2e9b3dec613ea3a844"
                    "666745b93560b0d9e8c92b5ce26c5d0ebe9c066e6d06fd9dd3e4eabb1a9d7232"
                    "50611a513d9f2c4e5257aa3d53939b3cd0d74efd21507c39cb19234b049642c2"
                    "d4ad28775a3771d2162e20e523dbc6b5de8bfd8613ee4af7c12cfeda6808cedc"
                    "9ecf74728b0d0ab3c576c2384de4ea9ab56236d8b951d0ea130b65a2b6f60950"
                    "6341502ea34d581530d52f281f7b2d0de92be8ab086d8dcc1a477f5f48ed5c34"
                    "c4d3d2ac6ed9674508db010fede25d65bcca7eb9c30be065b377d785553115de"
                    "40824e77b8615345667bc8800016869687e9631431b797c855f4e38680bae02b"
                >>
            },
            #{
                <<"valid">> => true,
                <<"y">> =>
                <<
                    "48a1213b8c32abe21effea22723cd474a95fedae9ec779ea59d91544d4d5ad5f"
                    "5495c269ee0041e867839c640d96776f0307da52beed29e7b09ff9b287cc394b"
                    "5b9526ce600906417cebfad5e857f6f7718242827a572a549178c961d1996a9a"
                    "f13d9e5129e3e52b4ffa4fbe937b551f7e31e4d11e2462ff7285fde22d02a5b2"
                    "e00c053e6d3844e6e0ee6ba5062cc24d96a020f17ba97413c59f821d75f3506f"
                    "aa09477b83396b81a1ac4f9cff20769b0d89e3f0520029100116c19c5e2c4f35"
                    "53e21ff904c80a23db92925dece9942e5d8497597e63354195727bccd44c70e4"
                    "4f7bc4921c03865fe4af4257835e346310b9f03df84ec8fa2171b3384638e0df"
                >>,
                <<"l">> => <<"841c24756904d9bd5e2aea34f796ce41">>
            }
        },
        {
            <<"t1024-crack-corrupt-proof">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f055"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-corrupt-y">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660fafa"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-n-minus-y-old-proof">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "61a4f43f07da2652294defcb6a55589639714799863b8573864e26275047f5b7"
                    "7a38a8dc84820a8c1f4c8335eb679ef862b8dda62188e57a3f0e9d46dde32501"
                    "a5e04f0cf8d0642a32fd152b55430ffd2fb2a9cd38681d4bea66e2e96a97d245"
                    "89ea8ad301947dc4640f4e4024f72b27d8e812c9d8f0855feaa74f066bd60040"
                    "eb8236bd55cba516418d161e6f1e8ec7a283d0e8b886bed3ccb4cbbe34e263b6"
                    "f715204641be91a815e78c97d61dda47deaf8ce856fde4d459ca0cb96c2479e2"
                    "398692b217308cd08b2372afc6b7df6b1feed26e08dc41d55de559753b3d81f5"
                    "cccb45207010ee78606756092b4ee3092a5fd893edfcde96f4af3ea76fbbccec"
                >>,
                <<"proof">> =>
                <<
                    "1ad0d6355842310cf8021d597ec149dc07332017bec894a7054da2314403c128"
                    "016fb56cd2c259d06f88fa31892ea6e55016c4f0367e71169b009ce20b16f54f"
                    "36d399f48d2ae530f17ba54000a367f9963271ad061c0a505d33903ab3a5a8ad"
                    "c8f989bc90fb71e0debfe92abc4fd999b11790f409aa82a767dde767f2abfe0b"
                    "a0a4ee7190e654a2cd1e498a48c14114f232a70d9edca5d5ef773ef8d20c8115"
                    "122e3e6834110b06d64e8c9f30e1a27a40da823d3eef88afdaa3a795e22c2b32"
                    "1764dcea073a74d8a6268d68b10bf52cf8900e6702f61aca34edd7f952d758bb"
                    "0be33f531028f997319a7a893d736190fc4ef48d40178f462fdff4e36df8d791"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-wrong-x">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f237"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-wrong-t">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1025">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-t-is-2^32">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"4294967296">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-t-over-2^32">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"4294967297">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-x-is-0">>,
            <<"verify-crack">>,
            #{
                <<"x">> => <<"0">>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-x-is-n">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "c7970ceedcc3b0754490201a7aa613cd73911081c790f5f1a8726f463550bb5b"
                    "7ff0db8e1ea1189ec72f93d1650011bd721aeeacc2acde32a04107f0648c2813"
                    "a31f5b0b7765ff8b44b4b6ffc93384b646eb09c7cf5e8592d40ea33c80039f35"
                    "b4f14a04b51f7bfd781be4d1673164ba8eb991c2c4d730bbbe35f592bdef524a"
                    "f7e8daefd26c66fc02c479af89d64d373f442709439de66ceb955f3ea37d5159"
                    "f6135809f85334b5cb1813addc80cd05609f10ac6a95ad65872c909525bdad32"
                    "bc729592642920f24c61dc5b3c3b7923e56b16a4d9d373d8721f24a3fc0f1b31"
                    "31f55615172866bccc30f95054c824e733a5eb6817f7bc16399d48c6361cc7e5"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-y-is-0">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> => <<"0">>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-y-is-n">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "c7970ceedcc3b0754490201a7aa613cd73911081c790f5f1a8726f463550bb5b"
                    "7ff0db8e1ea1189ec72f93d1650011bd721aeeacc2acde32a04107f0648c2813"
                    "a31f5b0b7765ff8b44b4b6ffc93384b646eb09c7cf5e8592d40ea33c80039f35"
                    "b4f14a04b51f7bfd781be4d1673164ba8eb991c2c4d730bbbe35f592bdef524a"
                    "f7e8daefd26c66fc02c479af89d64d373f442709439de66ceb955f3ea37d5159"
                    "f6135809f85334b5cb1813addc80cd05609f10ac6a95ad65872c909525bdad32"
                    "bc729592642920f24c61dc5b3c3b7923e56b16a4d9d373d8721f24a3fc0f1b31"
                    "31f55615172866bccc30f95054c824e733a5eb6817f7bc16399d48c6361cc7e5"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-proof-is-0">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> => <<"0">>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-proof-is-n">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "c7970ceedcc3b0754490201a7aa613cd73911081c790f5f1a8726f463550bb5b"
                    "7ff0db8e1ea1189ec72f93d1650011bd721aeeacc2acde32a04107f0648c2813"
                    "a31f5b0b7765ff8b44b4b6ffc93384b646eb09c7cf5e8592d40ea33c80039f35"
                    "b4f14a04b51f7bfd781be4d1673164ba8eb991c2c4d730bbbe35f592bdef524a"
                    "f7e8daefd26c66fc02c479af89d64d373f442709439de66ceb955f3ea37d5159"
                    "f6135809f85334b5cb1813addc80cd05609f10ac6a95ad65872c909525bdad32"
                    "bc729592642920f24c61dc5b3c3b7923e56b16a4d9d373d8721f24a3fc0f1b31"
                    "31f55615172866bccc30f95054c824e733a5eb6817f7bc16399d48c6361cc7e5"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-garbage-y">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> => <<"zz">>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-garbage-proof">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1024">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> => <<"not-hex">>
            },
            #{
                <<"valid">> => false
            }
        },
        {
            <<"t1024-crack-garbage-t">>,
            <<"verify-crack">>,
            #{
                <<"x">> =>
                <<
                    "5aa262667c054e76a7981e5995ed406fce27bc8d6744a4c4050826860369edb4"
                    "6565466d28cfd55f63ca8e476e5d80d7e21198a5d3307191362999cdbedaa1dd"
                    "75281dda615eb5fa956f13b34faa0235973120155ee1679e52d6c273feb59090"
                    "5ddc5a48dfbcfa3cfa339605da921bf360d831d4122bc85bebaf534ef170ddf0"
                    "ef42918427afc436283b146ec78249f8ac93ed3ebe7e946521f9eb8c4b560255"
                    "3c71216f41c9b2f1faeecce0737c10ebb0593f6e3d050502fe8c1e0b5ba9310f"
                    "af719722f4b71c394c0fa1ca9373bc0e001da4ae61956ee124a1a2a0087cc1e0"
                    "355848a59d013a15b8d7cecc8d0e6ddb1f372cbc2f53c66653bd6f64939f236"
                >>,
                <<"t">> => <<"1e3">>,
                <<"y">> =>
                <<
                    "65f218afd4e98a231b42304f1050bb373a1fc8e84155707e2224491ee508c5a4"
                    "05b832b19a1f0e12a7e3109b799872c50f621106a123f8b861326aa986a90311"
                    "fd3f0bfe7e959b6111b7a1d473f074b917385ffa96f66846e9a7c053156bccf0"
                    "2b06bf31b38afe39140c9691423a3992b5d17ef8ebe6ab5bd38ea68c5219520a"
                    "0c66a4327ca0c1e5c13763911ab7be6f9cc056208b1727991ee093806e9aeda2"
                    "fefe37c3b694a30db53087160662f2bd81ef83c41397c8912d6283dbb9993350"
                    "82ec02e04cf89421c13e69ab758399b8c57c4436d0f732031439cb2ec0d1993b"
                    "652a10f4a71778446bc9a347297941de094612d429fadd7f44ee0a1ec660faf9"
                >>,
                <<"proof">> =>
                <<
                    "acc636b984817f684c8e02c0fbe4c9f16c5df06a08c8614aa324cd14f14cfa33"
                    "7e8126214bdebece57a6999fdbd16ad8220429bc8c2e6d1c05406b0e597532c4"
                    "6c4bc116ea3b1a5a533911bfc8901cbcb0b8981ac9427b4276db1301cc5df687"
                    "ebf7c04824240a1c995bfba6aae18b20dda200cebb2cae1456580e2acb43543f"
                    "5743ec7e4186125935a6302541150c224d117ffba4c14096fc1e2045d170d044"
                    "e3e519a1c44229aef4c9870eab9f2a8b1fc48e6f2ba624b5ac88e8ff43918200"
                    "a50db8a85ceeac19a63b4ef28b2f83f6ecdb083dd6dd590e3d314caaa937c276"
                    "261216c206ff6d259a967ec71754c3563756f6dad7e02cd009bd53e2c823f054"
                >>
            },
            #{
                <<"valid">> => false
            }
        }
    ].

%% @doc The reference's challenge prime for x = cd, y = ef, T = 10.
fixed_challenge_prime() ->
    {{<<"cd">>, <<"ef">>, 10}, <<"e739a98a75dd2f77cbf4957f781343b9">>}.

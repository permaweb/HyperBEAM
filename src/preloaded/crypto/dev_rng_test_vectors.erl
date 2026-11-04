%%% @doc BLS interop vectors for `rng@1.0'. They were produced by the
%%% beacon's own threshold-BLS code (Random-Process `v2/provider/bls.mjs',
%%% noble-curves 2.4.0 `bls12_381.longSignatures'): a 2-of-3 joint-Feldman DKG,
%%% a group signature combined from two partials, one member's own signature,
%%% and invalid cases. noble agreed with every `valid' flag when they were
%%% generated. The live round is round 1 of the audited beacon process
%%% `Fvy9TgEWG9zy2JijUBqj4_kiB4xozAbNjYbKVD3o3PE', with the value it published.
-module(dev_rng_test_vectors).
-export([bls_vectors/0, live_round/0]).

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
            <<"message">> => <<"hyperbeam/rng/v1|Fvy9TgEWG9zy2JijUBqj4_kiB4xozAbNjYbKVD3o3PE|1">>,
            <<"signature">> =>
                <<
                    "b9fc48ff7c77fc984e8c2f59e11376d970cc182395a58f674bb2c83bf764a039"
                    "870b0c5484a1b0d238344849a4c8512e0fb2ec0fb5c45837912511bc4ddc8f17"
                    "6ab67b5455650a402a48b15a457ec4a470fddee95f044e3c6f8e9781f198df20"
                >>
        },
        <<"4adcfd59b919211d057229f5e26127d358f356025678f62a061651f4d00e1fef">>
    }.

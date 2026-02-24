-define(DATA_NODES, 
[
    %% Partitions 0-15
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 28_800_000_000_000,
        <<"with">> => <<"http://data-1.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 28_800_000_000_000,
        <<"with">> => <<"http://data-13.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    %% Partitions 16-31
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 86_400_000_000_000,
        <<"with">> => <<"http://data-2.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 86_400_000_000_000,
        <<"with">> => <<"http://data-3.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 86_400_000_000_000,
        <<"with">> => <<"http://data-14.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 86_400_000_000_000,
        <<"with">> => <<"http://data-15.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    %% Partitions 32-47
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 144_000_000_000_000,
        <<"with">> => <<"http://data-4.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 144_000_000_000_000,
        <<"with">> => <<"http://data-5.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 144_000_000_000_000,
        <<"with">> => <<"http://data-16.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 144_000_000_000_000,
        <<"with">> => <<"http://data-17.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    %% Partitions 48-63
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 201_600_000_000_000,
        <<"with">> => <<"http://data-6.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 201_600_000_000_000,
        <<"with">> => <<"http://data-7.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    %% Partitions 64-126
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 343_800_000_000_000,
        <<"with">> => <<"http://data-8.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    %% Partitions 75-138
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 385_200_000_000_000,
        <<"with">> => <<"http://data-9.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 385_200_000_000_000,
        <<"with">> => <<"http://data-10.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 385_200_000_000_000,
        <<"with">> => <<"http://data-11.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 385_200_000_000_000,
        <<"with">> => <<"http://data-12.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    }
]).

-define(TIP_NODES,
[
    %% Partitions 48-107 (tip nodes)
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-1.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-2.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-3.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-4.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-5.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    }
]).

-define(CHAIN_NODES,
[
    #{
        <<"match">> => <<"^/arweave">>,
        <<"with">> => <<"http://chain-1.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"with">> => <<"http://chain-2.arweave.xyz:1984">>,
        <<"opts">> => #{ http_client => httpc }
    }
]).
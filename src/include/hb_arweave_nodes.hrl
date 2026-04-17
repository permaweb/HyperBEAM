-define(ARWEAVE_BOOTSTRAP_DATA_NODES, 
[
    %% Partitions 0-15
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 0,
        <<"max">> => 57_600_000_000_000,
        <<"center">> => 28_800_000_000_000,
        <<"with">> => <<"http://data-1.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 0,
        <<"max">> => 57_600_000_000_000,
        <<"center">> => 28_800_000_000_000,
        <<"with">> => <<"http://data-13.arweave.xyz:1984">>
    },
    %% Partitions 0-3
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 0,
        <<"max">> => 14_400_000_000_000,
        <<"center">> => 7_200_000_000_000,
        <<"with">> => <<"http://data-2.arweave.xyz:1984">>
    },
    %% Partitions 4-7
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 14_400_000_000_000,
        <<"max">> => 28_800_000_000_000,
        <<"center">> => 21_600_000_000_000,
        <<"with">> => <<"http://data-3.arweave.xyz:1984">>
    },
    %% Partitions 8-11
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 28_800_000_000_000,
        <<"max">> => 43_200_000_000_000,
        <<"center">> => 36_000_000_000_000,
        <<"with">> => <<"http://data-4.arweave.xyz:1984">>
    },
    %% Partitions 12-15
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 43_200_000_000_000,
        <<"max">> => 57_600_000_000_000,
        <<"center">> => 50_400_000_000_000,
        <<"with">> => <<"http://data-5.arweave.xyz:1984">>
    },
    %% Partitions 16-31
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 57_600_000_000_000,
        <<"max">> => 115_200_000_000_000,
        <<"center">> => 86_400_000_000_000,
        <<"with">> => <<"http://data-2.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 57_600_000_000_000,
        <<"max">> => 115_200_000_000_000,
        <<"center">> => 86_400_000_000_000,
        <<"with">> => <<"http://data-3.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 57_600_000_000_000,
        <<"max">> => 115_200_000_000_000,
        <<"center">> => 86_400_000_000_000,
        <<"with">> => <<"http://data-14.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 57_600_000_000_000,
        <<"max">> => 115_200_000_000_000,
        <<"center">> => 86_400_000_000_000,
        <<"with">> => <<"http://data-15.arweave.xyz:1984">>
    },
    %% Partitions 32-47
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 115_200_000_000_000,
        <<"max">> => 172_800_000_000_000,
        <<"center">> => 144_000_000_000_000,
        <<"with">> => <<"http://data-4.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 115_200_000_000_000,
        <<"max">> => 172_800_000_000_000,
        <<"center">> => 144_000_000_000_000,
        <<"with">> => <<"http://data-5.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 115_200_000_000_000,
        <<"max">> => 172_800_000_000_000,
        <<"center">> => 144_000_000_000_000,
        <<"with">> => <<"http://data-16.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 115_200_000_000_000,
        <<"max">> => 172_800_000_000_000,
        <<"center">> => 144_000_000_000_000,
        <<"with">> => <<"http://data-17.arweave.xyz:1984">>
    }
    % Exclude these data nodes for now since their partitions are covered 
    % by the tip nodes (and the tip nodes are faster to read from).
    % %% Partitions 48-63
    % #{
    %     <<"match">> => <<"^/arweave">>,
    %     <<"min">> => 172_800_000_000_000,
    %     <<"max">> => 230_400_000_000_000,
    %     <<"center">> => 201_600_000_000_000,
    %     <<"with">> => <<"http://data-6.arweave.xyz:1984">>,
    %     <<"opts">> => #{ http_client => gun, protocol => http2 }
    % },
    % #{
    %     <<"match">> => <<"^/arweave">>,
    %     <<"min">> => 172_800_000_000_000,
    %     <<"max">> => 230_400_000_000_000,
    %     <<"center">> => 201_600_000_000_000,
    %     <<"with">> => <<"http://data-7.arweave.xyz:1984">>,
    %     <<"opts">> => #{ http_client => gun, protocol => http2 }
    % },
    % %% Partitions 64-126
    % #{
    %     <<"match">> => <<"^/arweave">>,
    %     <<"min">> => 230_400_000_000_000,
    %     <<"max">> => 457_200_000_000_000,
    %     <<"center">> => 343_800_000_000_000,
    %     <<"with">> => <<"http://data-8.arweave.xyz:1984">>,
    %     <<"opts">> => #{ http_client => gun, protocol => http2 }
    % },
    % %% Partitions 75-138
    % #{
    %     <<"match">> => <<"^/arweave">>,
    %     <<"min">> => 270_000_000_000_000,
    %     <<"max">> => 500_400_000_000_000,
    %     <<"center">> => 385_200_000_000_000,
    %     <<"with">> => <<"http://data-9.arweave.xyz:1984">>,
    %     <<"opts">> => #{ http_client => gun, protocol => http2 }
    % },
    % #{
    %     <<"match">> => <<"^/arweave">>,
    %     <<"min">> => 270_000_000_000_000,
    %     <<"max">> => 500_400_000_000_000,
    %     <<"center">> => 385_200_000_000_000,
    %     <<"with">> => <<"http://data-10.arweave.xyz:1984">>,
    %     <<"opts">> => #{ http_client => gun, protocol => http2 }
    % },
    % #{
    %     <<"match">> => <<"^/arweave">>,
    %     <<"min">> => 270_000_000_000_000,
    %     <<"max">> => 500_400_000_000_000,
    %     <<"center">> => 385_200_000_000_000,
    %     <<"with">> => <<"http://data-11.arweave.xyz:1984">>,
    %     <<"opts">> => #{ http_client => gun, protocol => http2 }
    % },
    % #{
    %     <<"match">> => <<"^/arweave">>,
    %     <<"min">> => 270_000_000_000_000,
    %     <<"max">> => 500_400_000_000_000,
    %     <<"center">> => 385_200_000_000_000,
    %     <<"with">> => <<"http://data-12.arweave.xyz:1984">>,
    %     <<"opts">> => #{ http_client => gun, protocol => http2 }
    % }
]).

-define(ARWEAVE_BOOTSTRAP_TIP_NODES,
[
    %% Partitions 48-107
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 172_800_000_000_000,
        <<"max">> => 388_800_000_000_000,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-1.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 172_800_000_000_000,
        <<"max">> => 388_800_000_000_000,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-2.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 172_800_000_000_000,
        <<"max">> => 388_800_000_000_000,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-3.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 172_800_000_000_000,
        <<"max">> => 388_800_000_000_000,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-4.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"min">> => 172_800_000_000_000,
        <<"max">> => 388_800_000_000_000,
        <<"center">> => 280_800_000_000_000,
        <<"with">> => <<"http://tip-5.arweave.xyz:1984">>
    }
]).

-define(ARWEAVE_BOOTSTRAP_CHAIN_NODES,
[
    #{
        <<"match">> => <<"^/arweave">>,
        <<"with">> => <<"http://chain-3.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"with">> => <<"http://chain-1.arweave.xyz:1984">>
    },
    #{
        <<"match">> => <<"^/arweave">>,
        <<"with">> => <<"http://chain-2.arweave.xyz:1984">>
    }
]).

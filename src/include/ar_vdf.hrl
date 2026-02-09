-ifndef(AR_VDF_HRL).
-define(AR_VDF_HRL, true).

%% 25 checkpoints * ~40ms each ~= 1 second per VDF step.
-define(VDF_CHECKPOINT_COUNT_IN_STEP, 25).

-define(VDF_BYTE_SIZE, 32).

%% Typical Ryzen 5900X iterations for 1 second.
-define(VDF_SHA_1S, 15_000_000).

-ifndef(VDF_DIFFICULTY).
-define(VDF_DIFFICULTY, ?VDF_SHA_1S div ?VDF_CHECKPOINT_COUNT_IN_STEP).
-endif.

-ifdef(AR_TEST).
%% Keep test retargeting quick while preserving protocol shape.
-define(VDF_DIFFICULTY_RETARGET, 20).
-define(VDF_HISTORY_CUT, 2).
-else.
-ifndef(VDF_DIFFICULTY_RETARGET).
-define(VDF_DIFFICULTY_RETARGET, 720).
-endif.
-ifndef(VDF_HISTORY_CUT).
-define(VDF_HISTORY_CUT, 50).
-endif.
-endif.

-endif.

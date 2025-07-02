%% jobforge_backend_queue_impl.erl
-module(jobforge_backend_queue_impl).
-callback init() -> any().
-callback enqueue(JobId :: term(), JobSpec :: term(), State :: any()) -> {ok, any()}.
-callback dequeue(State :: any()) -> {ok, {JobId :: term(), JobSpec :: term()}, any()} | {empty, any()}.
-callback pending_count(State :: any()) -> non_neg_integer().
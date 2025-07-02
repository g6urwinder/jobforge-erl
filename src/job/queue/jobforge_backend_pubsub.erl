%% jobforge_backend_pubsub.erl
-module(jobforge_backend_pubsub).
-behaviour(jobforge_backend_queue_impl).

-export([init/0, enqueue/3, dequeue/1, pending_count/1]).

init() -> [].
enqueue(JobId, JobSpec, State) ->
    %% Simulate publishing to a topic
    io:format("Publishing job ~p to pubsub~n", [JobId]),
    {ok, [{JobId, JobSpec} | State]}.
dequeue(State) ->
    case State of
        [H|T] -> {ok, H, T};
        [] -> {empty, []}
    end.
pending_count(State) -> length(State).
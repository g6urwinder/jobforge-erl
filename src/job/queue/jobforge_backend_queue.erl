%% jobforge_backend_queue.erl
-module(jobforge_backend_queue).
-behaviour(jobforge_backend_queue_impl).

-export([init/0, enqueue/3, dequeue/1, pending_count/1]).

init() -> queue:new().
enqueue(JobId, JobSpec, Q) -> {ok, queue:in({JobId, JobSpec}, Q)}.
dequeue(Q) ->
    case queue:out(Q) of
        {{value, {JobId, JobSpec}}, Q2} -> {ok, {JobId, JobSpec}, Q2};
        {empty, _} -> {empty, Q}
    end.
pending_count(Q) -> queue:len(Q).
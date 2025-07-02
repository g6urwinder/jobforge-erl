%% filepath: src/jobforge_router.erl

-module(jobforge_router).
-export([routes/0]).

routes() ->
    cowboy_router:compile([
        {'_', [
            {"/v1/job", jobforge_handler_v1, []},
            {"/v1/job/bash", jobforge_handler_v1, []},
            {"/v3/job/async", jobforge_async_handler, []},
            {"/v3/job/result/:id", jobforge_async_handler, []}
        ]}
    ]).
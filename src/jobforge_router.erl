%% filepath: src/jobforge_router.erl

-module(jobforge_router).
-export([routes/0]).

routes() ->
    cowboy_router:compile([
        {'_', [
            {"/syncjob", jobforge_handler, []},
            {"/syncjob/bash", jobforge_handler, []},
            {"/asyncjob", jobforge_async_handler, []},
            {"/asyncjob/result/:id", jobforge_async_handler, []}
        ]}
    ]).
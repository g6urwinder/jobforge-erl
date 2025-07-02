%%%-------------------------------------------------------------------
%% @doc jobforge public API
%% @end
%%%-------------------------------------------------------------------

-module(jobforge_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = jobforge_router:routes(),
    {ok, _} = cowboy:start_clear(http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    %% Start the supervisor which will start the necessary processes
    jobforge_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

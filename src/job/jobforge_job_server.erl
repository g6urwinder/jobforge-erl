-module(jobforge_job_server).
-behaviour(gen_server).

%% API
%% This is going to process sorting jobs Asynchronously.
-export([start_link/0, process/1, result/1]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TABLE, jobforge_jobs).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Submit a new job, returns Job ID
process(JobSpec) ->
    gen_server:call(?MODULE, {process, JobSpec}).

%% Get result/status for a job
result(JobId) ->
    gen_server:call(?MODULE, {result, JobId}).

init([]) ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end,
    {ok, #{}}.

handle_call({process, JobSpec}, _From, State) ->
    JobId = uuid:uuid_to_string(uuid:get_v4()),
    ets:insert(?TABLE, {JobId, #{status => pending, spec => JobSpec}}),
    %%% For now, process synchronously
    spawn(fun() -> process_job(JobId, JobSpec) end),
    {reply, JobId, State};
handle_call({result, JobId}, _From, State) ->
    case ets:lookup(?TABLE, JobId) of
        [{JobId, JobMap}] ->
            Status = maps:get(status, JobMap, undefined),
            Result = maps:get(result, JobMap, undefined),
            {reply, {Status, Result}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

process_job(JobId, JobSpec) ->
    [{_, JobMap}] = ets:lookup(?TABLE, JobId),
    RunningMap = maps:put(status, running, JobMap),
    ets:insert(?TABLE, {JobId, RunningMap}),
    %% Call your v2 sorter here
    Result = task_sorter_v2:sort(JobSpec),
    Status = case Result of
        {ok, _} -> done;
        {error, _} -> error
    end,
    [{_, JobMap2}] = ets:lookup(?TABLE, JobId),
    FinalMap = maps:merge(JobMap2, #{status => Status, result => Result}),
    ets:insert(?TABLE, {JobId, FinalMap}).
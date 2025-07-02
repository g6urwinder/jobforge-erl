-module(jobforge_job_server).
-behaviour(gen_server).

%% API
%% This is going to process sorting jobs Asynchronously.
-export([start_link/0, process/1, result/1, get_stats/0]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TABLE, jobforge_jobs).
-define(DEFAULT_LIMIT, 10).

-record(state, {
    backend_mod, %%% The module implementing the queue backend
    backend_state, %%% The state of the backend
    running = #{}, % JobId => Pid
    limit = ?DEFAULT_LIMIT,
    pending_count = 0,
    running_count = 0,
    completed_count = 0,
    total_submitted = 0
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Submit a new job, returns Job ID
process(JobSpec) ->
    gen_server:call(?MODULE, {process, JobSpec}).

%% Get result/status for a job
result(JobId) ->
    gen_server:call(?MODULE, {result, JobId}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

init([]) ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end,
    BackendMod = jobforge_backend_queue,
    BackendState = BackendMod:init(),
    {ok, #state{backend_mod = BackendMod, backend_state = BackendState}}.

handle_call({process, JobSpec}, _From, #state{running = Running, limit = Limit, pending_count = Pending, running_count = RunningCount, total_submitted = Total} = State) ->
    JobId = uuid:uuid_to_string(uuid:get_v4()),
    ets:insert(?TABLE, {JobId, #{status => pending, spec => JobSpec}}),
    ServerPid = self(),
    io:format("Running: ~p~n", [Running]),
    io:format("Limit: ~p~n", [Limit]),
    case maps:size(Running) < Limit of
        true ->
            Pid = spawn_link(fun() -> process_job(JobId, JobSpec, ServerPid) end),
            NewRunning = maps:put(JobId, Pid, Running),
            ets:insert(?TABLE, {JobId, #{status => running, spec => JobSpec}}),
            {reply, JobId, State#state{
                running = NewRunning,
                running_count = RunningCount + 1,
                total_submitted = Total + 1
            }};
        false ->
            BackendMod = State#state.backend_mod,
            BackendState = State#state.backend_state,
            {ok, NewBackendState} = BackendMod:enqueue(JobId, JobSpec, BackendState),
            {reply, JobId, State#state{
                backend_state = NewBackendState,
                pending_count = Pending + 1,
                total_submitted = Total + 1
            }}
    end;
handle_call({result, JobId}, _From, State) ->
    io:format("Finding Result for Job: ~p~n", [JobId]),
    case ets:lookup(?TABLE, JobId) of
        [{JobId, JobMap}] ->
            Status = maps:get(status, JobMap, undefined),
            Result = maps:get(result, JobMap, undefined),
            {reply, {Status, Result}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call(get_stats, _From, State) ->
    Stats = #{pending_count => State#state.pending_count,
              running_count => State#state.running_count,
              completed_count => State#state.completed_count,
              total_submitted => State#state.total_submitted},
    {reply, Stats, State};
handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info({job_done, JobId, Result}, #state{running = Running, running_count = RunningCount, completed_count = Completed, pending_count = Pending} = State) ->
    io:format("Job done: ~p, Result: ~p~n", [JobId, Result]),
    ServerPid = self(),
    %% Update ETS with result
    case ets:lookup(?TABLE, JobId) of
        [{JobId, JobMap}] ->
            Status = case Result of
                {ok, _} -> done;
                {error, _} -> error
            end,
            FinalMap = maps:merge(JobMap, #{status => Status, result => Result}),
            ets:insert(?TABLE, {JobId, FinalMap});
        [] ->
            ok
    end,
    NewRunning = maps:remove(JobId, Running),

    BackendMod = State#state.backend_mod,
    BackendState = State#state.backend_state,
    
    %% Now Pick up the new task from queue
    case BackendMod:dequeue(BackendState) of
        {ok, {NewJobId, NewJobSpec}, NewBackendState} ->
            NewPid = spawn_link(fun() -> process_job(NewJobId, NewJobSpec, ServerPid) end),
            NewRunning2 = maps:put(NewJobId, NewPid, NewRunning),
            ets:insert(?TABLE, {NewJobId, #{status => running, spec => NewJobSpec}}),
            {noreply, State#state{
                running = NewRunning2,
                backend_state = NewBackendState,
                running_count = RunningCount, % stays the same: one finished, one started
                completed_count = Completed + 1,
                pending_count = Pending - 1
            }};
        {empty, NewBackendState} ->
            {noreply, State#state{
                running = NewRunning,
                running_count = RunningCount - 1,
                completed_count = Completed + 1,
                backend_state = NewBackendState
            }}
    end;

handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

process_job(JobId, JobSpec, ServerPid) ->
    io:format("Processing job ~p~n", [JobId]),
    %% Simulate a long running job
    timer:sleep(1000), %% Sleep for 1 second for testing purposes
    Result = task_sorter:sort(JobSpec),
    io:format("Job ~p result: ~p~n", [JobId, Result]),
    ServerPid ! {job_done, JobId, Result},
    ok.
-module(jobforge_job_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

%%% In jobforge_job_SUITE.erl
all() -> [multiple_roots, linear_chain, diamond, large_acyclic, large_cycle, simple_acyclic, simple_cycle, independent_chains, missing_dependency, empty_list, single_task, self_cycle].

multiple_roots(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => []},
        #{<<"name">> => <<"B">>, <<"command">> => <<"echo B">>, <<"requires">> => []},
        #{<<"name">> => <<"C">>, <<"command">> => <<"echo C">>, <<"requires">> => [<<"A">>, <<"B">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = jobforge_job:process(Input),
    case Result of
        {ok, Sorted} ->
            Names = [maps:get(<<"name">>, T) || T <- Sorted],
            case Names of
                [<<"A">>, <<"B">>, <<"C">>] -> ok;
                [<<"B">>, <<"A">>, <<"C">>] -> ok;
                _ -> ct:fail({unexpected_order, Names})
            end;
        _ -> ct:fail({unexpected_result, Result})
    end.

linear_chain(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => []},
        #{<<"name">> => <<"B">>, <<"command">> => <<"echo B">>, <<"requires">> => [<<"A">>]},
        #{<<"name">> => <<"C">>, <<"command">> => <<"echo C">>, <<"requires">> => [<<"B">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = jobforge_job:process(Input),
    case Result of
        {ok, [A, B, C]} ->
            Names = [maps:get(<<"name">>, A), maps:get(<<"name">>, B), maps:get(<<"name">>, C)],
            case Names of
                [<<"A">>, <<"B">>, <<"C">>] -> ok;
                _ -> ct:fail({unexpected_order, Names})
            end;
        _ -> ct:fail({unexpected_result, Result})
    end.

diamond(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => []},
        #{<<"name">> => <<"B">>, <<"command">> => <<"echo B">>, <<"requires">> => [<<"A">>]},
        #{<<"name">> => <<"C">>, <<"command">> => <<"echo C">>, <<"requires">> => [<<"A">>]},
        #{<<"name">> => <<"D">>, <<"command">> => <<"echo D">>, <<"requires">> => [<<"B">>, <<"C">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = jobforge_job:process(Input),
    case Result of
        {ok, Sorted} ->
            Names = [maps:get(<<"name">>, T) || T <- Sorted],
            case Names of
                [<<"A">>, <<"B">>, <<"C">>, <<"D">>] -> ok;
                [<<"A">>, <<"C">>, <<"B">>, <<"D">>] -> ok;
                _ -> ct:fail({unexpected_order, Names})
            end;
        _ -> ct:fail({unexpected_result, Result})
    end.

simple_acyclic(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => []},
        #{<<"name">> => <<"B">>, <<"command">> => <<"echo B">>, <<"requires">> => [<<"A">>]},
        #{<<"name">> => <<"C">>, <<"command">> => <<"echo C">>, <<"requires">> => [<<"B">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = task_sorter:sort(Input),
    case Result of
        {ok, Sorted} when length(Sorted) =:= 3 -> ok;
        _ -> ct:fail({unexpected_result, Result})
    end.

simple_cycle(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => [<<"B">>]},
        #{<<"name">> => <<"B">>, <<"command">> => <<"echo B">>, <<"requires">> => [<<"A">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = task_sorter:sort(Input),
    case Result of
        {error, {cycle_detected, _}} -> ok;
        _ -> ct:fail({unexpected_result, Result})
    end.

independent_chains(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => []},
        #{<<"name">> => <<"B">>, <<"command">> => <<"echo B">>, <<"requires">> => [<<"A">>]},
        #{<<"name">> => <<"C">>, <<"command">> => <<"echo C">>, <<"requires">> => []},
        #{<<"name">> => <<"D">>, <<"command">> => <<"echo D">>, <<"requires">> => [<<"C">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = task_sorter:sort(Input),
    io:format("Result: ~p~n", [Result]),
    case Result of
        {ok, Sorted} when length(Sorted) =:= 4 -> ok;
        _ -> ct:fail({unexpected_result, Result})
    end.

missing_dependency(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => [<<"B">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = task_sorter:sort(Input),
    case Result of
        {error, _} -> ok; % You may want to check for a specific error
        _ -> ct:fail({unexpected_result, Result})
    end.

empty_list(_Config) ->
    Input = #{<<"tasks">> => []},
    Result = task_sorter:sort(Input),
    case Result of
        {ok, []} -> ok;
        _ -> ct:fail({unexpected_result, Result})
    end.

single_task(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => []}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = task_sorter:sort(Input),
    case Result of
        {ok, [Task]} ->
            case maps:get(<<"name">>, Task) of
                <<"A">> -> ok;
                _ -> ct:fail({unexpected_result, Result})
            end;
        _ -> ct:fail({unexpected_result, Result})
    end.

self_cycle(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => [<<"A">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = task_sorter:sort(Input),
    case Result of
        {error, {cycle_detected, <<"A">>}} -> ok;
        _ -> ct:fail({unexpected_result, Result})
    end.

large_cycle(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => [<<"B">>]},
        #{<<"name">> => <<"B">>, <<"command">> => <<"echo B">>, <<"requires">> => [<<"C">>]},
        #{<<"name">> => <<"C">>, <<"command">> => <<"echo C">>, <<"requires">> => [<<"A">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    Result = jobforge_job:process(Input),
    case Result of
        {error, _} -> ok;  % or {err, _} if that's your convention
        _ -> ct:fail({unexpected_result, Result})
    end.

large_acyclic(_Config) ->
    Tasks = get_or_generate_tasks(100_000, 3, false),
    Input = #{<<"tasks">> => Tasks},
    ct:pal("Generated Tasks List of length ~p~n", [length(Tasks)]),
    Start = os:timestamp(),
    Result = jobforge_job:process(Input),
    End = os:timestamp(),
    ElapsedMicroSecs = timer:now_diff(End, Start),
    ElapsedMillis = ElapsedMicroSecs div 1000,
    ct:pal("Time taken: ~pms~n", [ElapsedMillis]),
    case Result of
        {ok, _} -> ok;
        _ -> ct:fail({unexpected_result, Result})
    end.

get_or_generate_tasks(N, MaxDeps, AllowCycles) ->
    TmpDir = "/Users/gurwinder/Documents/Sort-Commands/jobforge/tmp",
    ok = ensure_tmp_dir(TmpDir),
    FileName = filename:join(TmpDir, io_lib:format("tasks_N~p_D~p_C~p.json", [N, MaxDeps, AllowCycles])),
    case file:read_file(FileName) of
        {ok, Bin} ->
            io:format("Loaded tasks from cache: ~s~n", [FileName]),
            jsx:decode(Bin, [return_maps]);
        {error, _} ->
            io:format("Generating new tasks and saving to: ~s~n", [FileName]),
            Tasks = generate_tasks(N, MaxDeps, AllowCycles),
            ok = file:write_file(FileName, jsx:encode(Tasks)),
            Tasks
    end.

ensure_tmp_dir(Dir) ->
    case file:read_file_info(Dir) of
        {ok, Info} when Info#file_info.type =:= directory -> ok;
        {ok, _} -> {error, not_a_directory};
        {error, _} -> file:make_dir(Dir)
    end.

generate_tasks(N, MaxDeps, AllowCycles) ->
    TaskNames = [<<"task-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, N)],
    Tasks = [generate_task(I, TaskNames, MaxDeps, AllowCycles, N) || I <- lists:seq(1, N)],
    io:format("~nDone generating ~p tasks.~n", [N]),
    Tasks.

generate_task(I, TaskNames, MaxDeps, AllowCycles, N) ->
    if I rem 1000 =:= 0; I =:= N ->
        io:format("\rGenerated ~p/~p tasks (~p%)", [I, N, (I*100) div N]);
       true -> ok
    end,
    Name = lists:nth(I, TaskNames),
    Command = <<"echo Task ", (integer_to_binary(I))/binary>>,
    Candidates =
        case AllowCycles of
            true -> TaskNames -- [Name];
            false -> lists:sublist(TaskNames, I-1)
        end,
    NumDeps = if Candidates =:= [] -> 0; true -> rand:uniform(min(MaxDeps, length(Candidates)) + 1) - 1 end,
    Requires = random_deps_fast(Candidates, NumDeps),
    #{<<"name">> => Name, <<"command">> => Command, <<"requires">> => Requires}.

random_deps_fast(_Candidates, 0) -> [];
random_deps_fast(Candidates, Num) when Num > 0 ->
    Indices = random_indices(length(Candidates), Num, sets:new()),
    [lists:nth(I, Candidates) || I <- Indices].

random_indices(_Len, 0, Acc) -> sets:to_list(Acc);
random_indices(Len, N, Acc) ->
    I = rand:uniform(Len),
    case sets:is_element(I, Acc) of
        true -> random_indices(Len, N, Acc);
        false -> random_indices(Len, N-1, sets:add_element(I, Acc))
    end.
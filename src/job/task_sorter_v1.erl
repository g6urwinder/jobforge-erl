%% filepath: src/job/task_sorter.erl
-module(task_sorter_v1).
-export([sort/1]).

sort(Tasks) ->
    %% Form the graph based nodes
    Graph = maps:from_list([{maps:get(<<"name">>, Task), Task} || Task <- Tasks]),

    %% Tasks names
    TaskNames = maps:keys(Graph),

    %% Let's loop over the list and while looping, if you visit any node mark it as visited
    %% and append values to sorted list
    InitialAcc = {maps:new(), []},

    Total = length(TaskNames),

    case fold_tasks(TaskNames, Graph, InitialAcc, Total, 0) of
        {ok, {_Visited, SortedNames}} ->
            % io:format("Sorted task names: ~p~n", [SortedNames]),
            %% Now we have sorted names, let's get the tasks in order
            SortedTasks = [maps:get(Name, Graph) || Name <- SortedNames],
            {ok, SortedTasks};
        {error, Reason} ->
            {error, Reason}
    end.

fold_tasks([], _Graph, Acc, _Total, _Count) ->
    {ok, Acc};
fold_tasks([TaskName | Rest], Graph, {Visited, SortedNames}, Total, Count) ->
    Progress = (Count / Total) * 100,
    print_progress(Progress),
    UpdatedCount = Count + 1,
    case maps:is_key(TaskName, Visited) of
        true ->
            %% If the task is already visited, continue
            fold_tasks(Rest, Graph, {Visited, SortedNames}, Total, UpdatedCount);
        false ->
            case dfs_visit(TaskName, Graph, {Visited, SortedNames}) of
                {ok, NewAcc} -> fold_tasks(Rest, Graph, NewAcc, Total, UpdatedCount);
                {error, Reason} -> {error, Reason}
            end
    end.

%% Depth First Search (DFS) visit
dfs_visit(TaskName, Graph, {Visited, SortedNames}) ->
    %% Mark the current task as visiting to prevent cyclic dependencies
    VisitedWithVisiting = maps:put(TaskName, visiting, Visited),

    %% Get the current task
    Task = maps:get(TaskName, Graph),
    Dependencies = maps:get(<<"requires">>, Task, []),

    % io:format("Visited With Visiting: ~p~n", [VisitedWithVisiting]),
    % io:format("Current task: ~p~n", [TaskName]),
    % io:format("Visiting task: ~p with dependencies: ~p~n", [TaskName, Dependencies]),

    %% visit all dependencies first
    case process_dependencies(Dependencies, Graph, {VisitedWithVisiting, SortedNames}) of
        {ok, {VisitedAfterDeps, SortedAfterDeps}} ->
            %% After visiting dependencies, mark the current task as visited
            FinalVisited = maps:put(TaskName, visited, VisitedAfterDeps),
            FinalSorted = [TaskName | SortedAfterDeps],
            {ok, {FinalVisited, FinalSorted}};
        {error, Reason} ->
            {error, Reason}
    end.

process_dependencies([], _Graph, Acc) ->
    {ok, Acc};
process_dependencies([Dep | Rest], Graph, Acc) ->
    {Visited, _SortedNames} = Acc,
    case maps:get(Dep, Visited, not_found) of
        not_found ->
            %% If the dependency is not visited, visit it
            case dfs_visit(Dep, Graph, Acc) of
                {ok, NewAcc} -> process_dependencies(Rest, Graph, NewAcc);
                {error, Reason} -> {error, Reason}
            end;
        visiting ->
            %% If we find a cycle
            {error, {cycle_detected, Dep}};
        visited ->
            %% If already visited, continue
            process_dependencies(Rest, Graph, Acc)
    end.

print_progress(Progress) when Progress rem 5 =:= 0 ->
    io:format("\rProgress: ~3w%%", [Progress]),
    ok;
print_progress(_) ->
    ok.
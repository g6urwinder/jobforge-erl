-module(task_sorter).
-export([sort/1]).

%% Definition of tasks is
%% - name: string
%% - command: string
%% - requires: list of strings
sort(Input)->
    case Input of
        #{<<"tasks">> := Tasks} when is_list(Tasks) ->
            do_sort(Tasks);
        Tasks when is_list(Tasks) ->
            do_sort(Tasks);
        _ ->
            {error, invalid_input}
    end.

do_sort(Tasks) when is_list(Tasks) ->
    %% Do the Detch First Search, and figure out all the dependencies of first Node
    %% Do recursively for all of rest Nodes as well.

    %% While it is performing this calculation form two objects
    %% SortedNames - This is the final output list that we will generate as we
    %%               loop over these values recursively
    %% VisitedNodes - If some node is already visited, it means there circular dependecies
    %%               are already resolved and they are already in the sorted list.
    %%               So we can skip them.

    %% We will use a map to store the visited nodes.
    %% - This will allow us to save both visiting and visited nodes states.
    %% We will use a list to store the sorted nodes.

    SortedNames = [],

    VisitedNodes = maps:new(),

    TasksMap = maps:from_list([{maps:get(<<"name">>, Task), Task} || Task <- Tasks]),

    % ct:pal("TasksMap or OriiginalTasksMap: ~p~n", [TasksMap]),
    case do_dfs(maps:keys(TasksMap), VisitedNodes, SortedNames, TasksMap) of
        {ok, _VisitedNodes, FinalSortedNames} ->
            FinalSortedTasks = [maps:get(Name, TasksMap) || Name <- FinalSortedNames],
            {ok, lists:reverse(FinalSortedTasks)};
        {error, Reason} ->
            {error, Reason}
    end.

do_dfs([], VisitedNodes, SortedNames, _OriginalTasksMap) ->
    {ok, VisitedNodes, SortedNames};
do_dfs([TaskName | Rest], VisitedNodes, SortedNames, OriginalTasksMap) ->
    % ct:pal("Task: ~p~n", [Task]),
    % ct:pal("TaskName: ~p from VisitedNodes: ~p~n", [TaskName, VisitedNodes]),
    case maps:is_key(TaskName, VisitedNodes) of
        true ->
            case maps:get(TaskName, VisitedNodes) of    
                visiting ->
                    % ct:pal("Cycle detected: ~p~n", [TaskName]),
                    {error, {cycle_detected, TaskName}};
                visited ->
                    do_dfs(Rest, VisitedNodes, SortedNames, OriginalTasksMap)
            end;
        false ->
            case dfs_visit(TaskName, VisitedNodes, SortedNames, OriginalTasksMap) of
                {ok, NewVisitedNodes, NewSortedNames} ->
                    do_dfs(Rest, NewVisitedNodes, NewSortedNames, OriginalTasksMap);
                {error, Reason} ->
                    ct:pal("Error: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

dfs_visit(TaskName, VisitedNodes, SortedNames, OriginalTasksMap) ->
    case maps:get(TaskName, VisitedNodes, undefined) of
        visiting -> 
            % ct:pal("Cycle detected: ~p~n", [TaskName]),
            {error, {cycle_detected, TaskName}};
        visited -> {ok, VisitedNodes, SortedNames};
        undefined ->
            NewVisitedNodes = maps:put(TaskName, visiting, VisitedNodes),
            case maps:is_key(TaskName, OriginalTasksMap) of
                true ->
                    case maps:get(<<"requires">>, maps:get(TaskName, OriginalTasksMap)) of
                    [] ->
                        NewSortedNames = [TaskName | SortedNames],
                        {ok, maps:put(TaskName, visited, NewVisitedNodes), NewSortedNames};
                    Requires ->
                        case do_dfs(Requires, NewVisitedNodes, SortedNames, OriginalTasksMap) of
                            {ok, NewVisitedNodes2, NewSortedNames} ->
                                % ct:pal("NewSortedNames: ~p~n", [NewSortedNames]),
                                % ct:pal("Existing SortedNames: ~p~n", [SortedNames]),
                                {ok, maps:put(TaskName, visited, NewVisitedNodes2), [TaskName | NewSortedNames]};
                            {error, Reason} ->
                                {error, Reason}
                        end
                    end;
                false ->
                    {error, {task_not_found, TaskName}}
            end
    end.


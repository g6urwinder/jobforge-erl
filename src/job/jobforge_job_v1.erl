-module(jobforge_job_v1).
-export([process/1]).

process(#{<<"tasks">> := Tasks}) when is_list(Tasks) ->
    case task_sorter_v2:sort(Tasks) of
        {error, Reason} ->
            {error, Reason};
        {ok, SortedTasks} ->
            %% You can add logic here to check for a "format" key for bash output
            {ok, SortedTasks}
    end;
process(_) ->
    {error, <<"Missing or invalid 'tasks'">>}.
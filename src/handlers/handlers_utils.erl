-module(handlers_utils).

-export([tasks_to_bash/1]).

tasks_to_bash(Tasks) ->
    lists:foldl(fun(Task, Acc) ->
        [maps:get(<<"command">>, Task) | Acc ]
     end, [], Tasks).
-module(jobforge_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req),
            %% Parse JSON and handle errors
            io:format("Received request with method: ~p~n", [Method]),
            io:format("Request body: ~p~n", [Body]),
            try jsx:decode(Body, [return_maps]) of
                Json ->
                    io:format("Body: ~p~n", [Body]),
                    io:format("Decoded JSON: ~p~n", [Json]),
                    case jobforge_job:process(Json) of
                        {ok, ResponseMap} ->
                            io:format("Processed job successfully: ~p~n", [ResponseMap]),
                            io:format("Path: ~p~n", [Path]),
                            case Path of
                                <<"/syncjob/bash">> ->
                                    BashScript = handlers_utils:tasks_to_bash(ResponseMap),
                                    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, BashScript, Req1),
                                    {ok, Req2, State};
                                _ ->
                                    RespJson = jsx:encode(ResponseMap),
                                    Req2 = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, RespJson, Req1),
                                    {ok, Req2, State}
                            end;
                        {error, {cycle_detected, TaskName}} ->
                            ErrorMsg = jsx:encode(#{error => <<"Cyclic dependency detected">>, task => TaskName}),
                            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorMsg, Req1),
                            {ok, Req2, State};
                        {error, Reason} ->
                            ErrorMsg = jsx:encode(#{error => Reason}),
                            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorMsg, Req1),
                            {ok, Req2, State}
                    end
            catch
                Class:Reason ->
                    io:format("JSON decode error: ~p:~p~n", [Class, Reason]),
                    Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Invalid JSON">>, Req1),
                    {ok, Req2, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(405, #{<<"content-type">> => <<"text/plain">>}, <<"Method Not Allowed">>, Req),
            {ok, Req2, State}
    end.
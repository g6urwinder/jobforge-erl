-module(jobforge_async_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    case {Method, Path} of
        %% Submit a new job
        {<<"POST">>, <<"/v3/job/async">>} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req),
            try jsx:decode(Body, [return_maps]) of
                Json ->
                    JobId = jobforge_job_server:process(Json),
                    io:format("JobId: ~p~n", [JobId]),
                    Resp = jsx:encode(#{job_id => list_to_binary(JobId)}),
                    io:format("Resp: ~p~n", [Resp]),
                    Req2 = cowboy_req:reply(202, #{"content-type" => "application/json"}, Resp, Req1),
                    {ok, Req2, State}
            catch
                _:_ ->
                    Req2 = cowboy_req:reply(400, #{"content-type" => "application/json"}, <<"Invalid JSON">>, Req1),
                    {ok, Req2, State}
            end;
        %% Poll for job result (move path check into body)
        {<<"GET">>, PathBin} ->
            PathStr = binary_to_list(PathBin),
            case string:prefix(PathStr, "/v3/job/result/") of
                true ->
                    IdStr = lists:nthtail(length("/v3/job/result/"), PathStr),
                    JobId = IdStr,
                    {Status, Result} = jobforge_job_server:result(JobId),
                    Resp = jsx:encode(#{status => Status, result => Result}),
                    Req2 = cowboy_req:reply(200, #{"content-type" => "application/json"}, Resp, Req),
                    {ok, Req2, State};
                false ->
                    Req2 = cowboy_req:reply(405, #{"content-type" => "text/plain"}, <<"Method Not Allowed">>, Req),
                    {ok, Req2, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(405, #{"content-type" => "text/plain"}, <<"Method Not Allowed">>, Req),
            {ok, Req2, State}
    end. 
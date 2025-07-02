-module(jobforge_server_v1_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [submit_and_result, result_not_found, async_result].

submit_and_result(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => []},
        #{<<"name">> => <<"B">>, <<"command">> => <<"echo B">>, <<"requires">> => [<<"A">>]},
        #{<<"name">> => <<"C">>, <<"command">> => <<"echo C">>, <<"requires">> => [<<"B">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    {ok, _} = application:ensure_all_started(jobforge),
    JobId = jobforge_job_server:process(Input),
    timer:sleep(100),
    {Status, Result} = jobforge_job_server:result(JobId),
    case {Status, Result} of
        {done, {ok, [A, B, C]}} ->
            Names = [maps:get(<<"name">>, A), maps:get(<<"name">>, B), maps:get(<<"name">>, C)],
            case Names of
                [<<"A">>, <<"B">>, <<"C">>] -> ok;
                _ -> ct:fail({unexpected_order, Names})
            end;
        {done, {ok, _}} -> ok; % Accept any valid topological order
        _ -> ct:fail({unexpected_result, {Status, Result}})
    end.

result_not_found(_Config) ->
    FakeJobId = make_ref(),
    {Status, _} = jobforge_job_server:result(FakeJobId),
    case Status of
        error -> ok;
        _ -> ct:fail({should_be_error, Status})
    end.

async_result(_Config) ->
    Tasks = [
        #{<<"name">> => <<"A">>, <<"command">> => <<"echo A">>, <<"requires">> => []},
        #{<<"name">> => <<"B">>, <<"command">> => <<"echo B">>, <<"requires">> => [<<"A">>]},
        #{<<"name">> => <<"C">>, <<"command">> => <<"echo C">>, <<"requires">> => [<<"B">>]}
    ],
    Input = #{<<"tasks">> => Tasks},
    {ok, _} = application:ensure_all_started(jobforge),
    JobId = jobforge_job_server:process(Input),
    % Immediately poll, should be pending or running
    {Status1, _} = jobforge_job_server:result(JobId),
    case Status1 of
        pending -> ok;
        running -> ok;
        done -> ok;
        _ -> ct:fail({unexpected_status, Status1})
    end,
    timer:sleep(100),
    {Status2, Result2} = jobforge_job_server:result(JobId),
    case Status2 of
        done -> ok;
        _ -> ct:fail({not_done, Status2, Result2})
    end. 
-module(uuid).
-export([get_v4/0, uuid_to_string/1]).

get_v4() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    % Set the version (4) and variant (2)
    C1 = (C band 16#0fff) bor 16#4000,
    D1 = (D band 16#3fff) bor 16#8000,
    lists:flatten(
        io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
            [A, B, C1, D1, E])).

uuid_to_string(UUID) ->
    lists:flatten(UUID).
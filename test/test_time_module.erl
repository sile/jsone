-module(test_time_module).
-export([localtime/0, set_localtime/1, localtime_to_universaltime/1, mock_localtime_to_universaltime/1]).


set_localtime({{_, _, _}, {_, _, _}} = LocalTime) ->
    erlang:put('__test_time_module__localtime__', LocalTime).


localtime() ->
    erlang:get('__test_time_module__localtime__').


localtime_to_universaltime({{_, _, _}, {_, _, _}} = LocalTime) ->
    LocalTimeToUniversalTimeFun = erlang:get('__test_time_module_localtime_to_universaltime__'),
    LocalTimeToUniversalTimeFun(LocalTime).


mock_localtime_to_universaltime(Fun) when is_function(Fun) ->
    erlang:put('__test_time_module_localtime_to_universaltime__', Fun).

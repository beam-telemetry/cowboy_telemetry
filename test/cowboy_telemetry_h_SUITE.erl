-module(cowboy_telemetry_h_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [hello].

init_per_suite(Config) ->
    application:ensure_all_started(ranch),
    application:ensure_all_started(telemetry),
    Dispatch = cowboy_router:compile([{"localhost", [{"/", test_h, []}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
                  env => #{dispatch => Dispatch},
                  stream_handlers => [cowboy_telemetry_h, cowboy_stream_h]
              }
    ),
    Config.

end_per_suite(_Config) ->
    application:stop(ranch),
    application:stop(telemetry).

hello(Config) ->
    Events = [
        [cowboy, request, start],
        [cowboy, request, stop],
        [cowboy, request, exception],
        [cowboy, request, early_error]
    ],
    telemetry:attach_many(?config(id, Config), Events, fun ?MODULE:echo_event/4, #{pid => self()}),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080", []}, [], []),
    receive
        {event, [cowboy, request, start], Measurements, _Metadata, _Config} ->
            ?assertEqual([system_time], maps:keys(Measurements))
    after
        1000 -> ct:fail(timeout_start_event)
    end,
    ?assertEqual(true, true).

echo_event(Event, Measurements, Metadata, #{pid := Pid} = Config) ->
        Pid ! {event, Event, Measurements, Metadata, Config}.

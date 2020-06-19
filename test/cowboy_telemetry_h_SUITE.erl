-module(cowboy_telemetry_h_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [successful_request].

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

successful_request(Config) ->
    Events = [
        [cowboy, request, start],
        [cowboy, request, stop]
    ],
    telemetry:attach_many(?config(id, Config), Events, fun ?MODULE:echo_event/4, self()),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080", []}, [], []),
    receive
        {[cowboy, request, start], StartMeasurements, StartMetadata} ->
            ?assertEqual([system_time], maps:keys(StartMeasurements)),
            ?assertEqual([req, stream_id], maps:keys(StartMetadata))
    after
        1000 -> ct:fail(start_event)
    end,
    receive
        {[cowboy, request, stop], StopMeasurements, StopMetadata} ->
            ?assertEqual([duration], maps:keys(StopMeasurements)),
            ?assertEqual([response, stream_id], maps:keys(StopMetadata))
    after
        1000 -> ct:fail(stop_event)
    end,
    ?assertEqual(true, true).

echo_event(Event, Measurements, Metadata, Pid) ->
        Pid ! {Event, Measurements, Metadata}.

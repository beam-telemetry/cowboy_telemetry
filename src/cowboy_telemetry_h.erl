-module(cowboy_telemetry_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

init(StreamID, Req, Opts) ->
    telemetry:execute(
        [cowboy, request, start],
        #{system_time => erlang:system_time()},
        #{streamid => StreamID, req => Req}),
    cowboy_metrics_h:init(StreamID, Req, add_metrics_callback(Opts)).

info(StreamID, Info, State) ->
    cowboy_metrics_h:info(StreamID, Info, State).

data(StreamID, IsFin, Data, State) ->
    cowboy_metrics_h:data(StreamID, IsFin, Data, State).

terminate(StreamID, Reason, State) ->
    cowboy_metrics_h:terminate(StreamID, Reason, State).

early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_metrics_h:early_error(StreamID, Reason, PartialReq, Resp, add_metrics_callback(Opts)).

%

add_metrics_callback(Opts) ->
    maps:put(metrics_callback, fun metrics_callback/1, Opts).

metrics_callback(#{reason := {internal_error, {'EXIT', _, {_, Stacktrace}}, _}, req_start := ReqStart, req_end := ReqEnd} = Metadata) ->
    telemetry:execute(
        [cowboy, request, exception],
        #{duration => ReqEnd - ReqStart},
        maps:merge(Metadata, #{kind => exit, stacktrace => Stacktrace}));
metrics_callback(#{reason := {ErrorType, _, _} = Reason, req_start := ReqStart, req_end := ReqEnd} = Metadata)
    when ErrorType == socket_error;
         ErrorType == connection_error ->
    telemetry:execute(
        [cowboy, request, stop],
        #{duration => ReqEnd - ReqStart},
        maps:merge(Metadata, #{error => Reason}));
metrics_callback(#{req_start := ReqStart, req_end := ReqEnd} = Metadata) ->
    telemetry:execute(
        [cowboy, request, stop],
        #{duration => ReqEnd - ReqStart},
        Metadata);
metrics_callback(#{early_error_time := _} = Metadata) ->
    telemetry:execute(
        [cowboy, request, early_error],
        #{system_time => erlang:system_time()},
        Metadata);
metrics_callback(_Metrics) ->
    unexpected.

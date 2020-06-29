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

metrics_callback(#{early_error_time := Time} = Metrics) when is_number(Time) ->
    telemetry:execute(
        [cowboy, request, early_error],
        #{system_time => erlang:system_time()},
        Metrics);
metrics_callback(#{reason := {internal_error, {'EXIT', _, {_, Stacktrace}}, _}} = Metrics) ->
    telemetry:execute(
        [cowboy, request, exception],
        measurements(Metrics),
        metadata(Metrics, #{kind => exit, stacktrace => Stacktrace}));
metrics_callback(#{reason := {ErrorType, _, _} = Reason} = Metrics)
    when ErrorType == socket_error;
         ErrorType == connection_error ->
    telemetry:execute(
        [cowboy, request, stop],
        measurements(Metrics),
        metadata(Metrics, #{error => Reason}));
metrics_callback(Metrics) ->
    telemetry:execute(
        [cowboy, request, stop],
        measurements(Metrics),
        metadata(Metrics, #{})).

measurements(Metrics) ->
    #{duration => duration(req_start, req_end, Metrics),
      req_body_duration => duration(req_body_start, req_body_end, Metrics),
      req_body_length => get(req_body_length, Metrics),
      resp_duration => duration(resp_start, resp_end, Metrics),
      resp_body_length => get(resp_body_length, Metrics)}.

metadata(Metrics, Extra) ->
    maps:merge(Extra, #{
      pid => get(pid, Metrics),
      streamid => get(streamid, Metrics),
      req => get(req, Metrics),
      resp_headers => get(resp_headers, Metrics),
      resp_status => get(resp_status, Metrics),
      reason => get(reason, Metrics),
      procs => get(procs, Metrics),
      informational => get(informational, Metrics),
      ref => get(ref, Metrics)}).

% Helpers

duration(StartKey, EndKey, Metrics) ->
    duration(get(StartKey, Metrics), get(EndKey, Metrics)).

duration(Start, End) when Start =:= undefined; End =:= undefined ->
    undefined;
duration(Start, End) ->
    End - Start.

get(Key, Map) ->
   maps:get(Key, Map, undefined).

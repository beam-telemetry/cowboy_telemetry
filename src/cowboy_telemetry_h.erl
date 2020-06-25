-module(cowboy_telemetry_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-record(state, {
    next :: any(),
    status :: undefined | active,
    start_time :: integer()
}).

init(StreamID, Req, Opts) ->
    StartTime = emit_start_event(StreamID, Req),
    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    {Commands, #state{next=Next, start_time=StartTime}}.

data(StreamID, IsFin, Data, State=#state{next=Next0}) ->
    {Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
    {Commands, State#state{next=Next}}.

info(StreamID, Info, State=#state{next=Next0, start_time=StartTime}) ->
    {Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
    Status =
        case Commands of
            [{response, _, _, _} = Response] ->
                emit_stop_event(StreamID, StartTime, Response),
                done;
            [{error_response, _, _, _} = ErrorResponse | Commands1] ->
                case lists:keyfind(internal_error, 1, Commands1) of
                    {internal_error, {'EXIT', _, Reason}, _} ->
                        emit_exception_event(StreamID, StartTime, Reason, ErrorResponse),
                        done;
                    _ ->
                        undefined
                end;
            _ ->
                undefined
        end,
    {Commands, State#state{next=Next, status=Status}}.

terminate(StreamID, Reason, #state{status=done, next=Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next);
terminate(StreamID, Reason, #state{next=Next, start_time=StartTime}) ->
    case Reason of
        {socket_error, _, _} = Reason ->
            emit_stop_error_event(StreamID, StartTime, Reason);
        {connection_error, _, _} = Reason ->
            emit_stop_error_event(StreamID, StartTime, Reason);
        _ -> ignore
    end,
    cowboy_stream:terminate(StreamID, Reason, Next).

early_error(StreamID, Reason, PartialReq, Resp0, Opts) ->
    Resp = cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp0, Opts),
    emit_early_error_event(StreamID, Reason, PartialReq, Resp),
    Resp.

% Telemetry events

emit_start_event(StreamID, Req) ->
    SystemTime = erlang:system_time(),
    StartTime = erlang:monotonic_time(),
    telemetry:execute(
        [cowboy, request, start],
        #{system_time => SystemTime},
        #{stream_id => StreamID, req => Req}
    ),
    StartTime.

emit_stop_event(StreamID, StartTime, Response) ->
    EndTime = erlang:monotonic_time(),
    telemetry:execute(
        [cowboy, request, stop],
        #{duration => EndTime - StartTime},
        #{stream_id => StreamID, response => Response}
    ).

emit_stop_error_event(StreamID, StartTime, Reason) ->
    EndTime = erlang:monotonic_time(),
    io:format("~p~n", [Reason]),
    telemetry:execute(
        [cowboy, request, stop],
        #{duration => EndTime - StartTime},
        #{stream_id => StreamID, error => Reason}
    ).

emit_exception_event(StreamID, StartTime, Reason, ErrorResponse) ->
    EndTime = erlang:monotonic_time(),
    telemetry:execute(
        [cowboy, request, exception],
        #{duration => EndTime - StartTime},
        #{stream_id => StreamID, kind => exit, reason => Reason, error_response => ErrorResponse}
    ).


emit_early_error_event(StreamID, Reason, PartialReq, Resp) ->
    SystemTime = erlang:system_time(),
    telemetry:execute(
        [cowboy, request, early_error],
        #{system_time => SystemTime},
        #{stream_id => StreamID, reason => Reason, partial_req => PartialReq, response => Resp}
    ).

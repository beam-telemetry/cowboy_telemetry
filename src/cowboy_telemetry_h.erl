-module(cowboy_telemetry_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

% Request Flows:
%
% There are multiple ways a request flows through the stream handler callbacks.
% All start with `init`, where we emit our span start event. In each flow, the location
% where we emit the span stop event is designated with ^^. All events are based on the
% Commands returned by cowboy.
%
% successful_request     == init -> info(response) ^^ -> info(stop) -> terminate(normal)
% failed_request         == init -> info(error_response) ^^ -> terminate(internal_error)
% chunked_request        == init -> info(headers) -> info(data|nofin) -> info(data|fin) ^^ -> info(stop) -> terminate(normal)
% client_timeout_request == init -> terminate(socket_error) ^^
% idle_timeout_request   == init -> terminate(connection_error) ^^
% chunk_timeout_request  == init -> info(headers) -> info(data|nofin) -> terminate(connection_error) ^^
% bad_request            == early_error

-record(state, {
    next :: any(),

    % Request identity
    streamid :: cowboy_stream:streamid(),
    request_process :: pid(),

    % Response info
    start_time :: integer(),
    response :: undefined | {response, cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body()},
    error_response :: undefined | {error_response, cowboy:http_status(), cowboy:http_headers(), iodata()},
    reason :: undefined | any(),

    % Span stop tracking
    emit :: undefined | stop | exception | done
}).

init(StreamID, Req, Opts) ->
    StartTime = erlang:monotonic_time(),
    SystemTime = erlang:system_time(),
    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    State = fold(Commands, #state{next=Next, streamid=StreamID, start_time=StartTime}),
    span_start(State, SystemTime, Req),
    {Commands, State}.


data(StreamID, IsFin, Data, State=#state{next=Next0}) ->
    {Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
    {Commands, State#state{next=Next}}.


info(StreamID, Info, State0=#state{next=Next0}) ->
    {Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
    State1 = fold(Commands, State0#state{next=Next}),
    span_stop(State1),
    {Commands, State1}.


terminate(StreamID, Reason, #state{emit=done, next=Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next);

terminate(StreamID, {ErrorType, _, _} = Reason, #state{next=Next} = State)
    when ErrorType == socket_error; ErrorType == connection_error ->
    span_stop(State#state{emit=error, reason=Reason}),
    cowboy_stream:terminate(StreamID, Reason, Next);

terminate(StreamID, Reason, #state{next=Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next).


early_error(StreamID, Reason, PartialReq, Resp0, Opts) ->
    Resp = cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp0, Opts),
    emit_early_error_event(StreamID, Reason, PartialReq, Resp),
    Resp.

%

fold([], State) ->
    State;

fold([{response, _, _, _} = Response | Tail], State0) ->
    State = State0#state{emit=stop, response=Response},
    fold(Tail, State);

fold([{data, fin, _} | Tail], State0) ->
    State = State0#state{emit=stop},
    fold(Tail, State);

fold([{internal_error, {'EXIT', _, Reason}, _} | Tail], State0) ->
    State = State0#state{emit=exception, reason=Reason},
    fold(Tail, State);

fold([{headers, RespStatus, RespHeaders} | Tail], State0) ->
    State = State0#state{response={response, RespStatus, RespHeaders, nil}},
    fold(Tail, State);

fold([{error_response, _, _, _} = ErrorResponse | Tail], State0) ->
    State = State0#state{error_response=ErrorResponse},
    fold(Tail, State);

fold([{spawn, Pid, _} | Tail], State0) ->
    State = State0#state{request_process=Pid},
    fold(Tail, State);

fold([_ | Tail], State) ->
    fold(Tail, State).


span_start(#state{streamid=StreamID, request_process=RequestProcess}, SystemTime, Req) ->
    emit_start_event(StreamID, SystemTime, Req, RequestProcess).


span_stop(#state{emit=stop, streamid=StreamID, start_time=StartTime, response=Response} = State) ->
    emit_stop_event(StreamID, StartTime, Response),
    State#state{emit=done};

span_stop(#state{emit=error, streamid=StreamID, start_time=StartTime, reason=Reason} = State) ->
    emit_stop_error_event(StreamID, StartTime, Reason),
    State#state{emit=done};

span_stop(#state{emit=exception, streamid=StreamID, start_time=StartTime, error_response=ErrorResponse, reason=Reason} = State) ->
    emit_exception_event(StreamID, StartTime, Reason, ErrorResponse),
    State#state{emit=done};

span_stop(State) ->
    State.


emit_start_event(StreamID, SystemTime, Req, RequestProcess) ->
    telemetry:execute(
        [cowboy, request, start],
        #{system_time => SystemTime},
        #{stream_id => StreamID, req => Req, request_process => RequestProcess}
    ).

emit_stop_event(StreamID, StartTime, Response) ->
    EndTime = erlang:monotonic_time(),
    telemetry:execute(
        [cowboy, request, stop],
        #{duration => EndTime - StartTime},
        #{stream_id => StreamID, response => Response}
    ).

emit_stop_error_event(StreamID, StartTime, Reason) ->
    EndTime = erlang:monotonic_time(),
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

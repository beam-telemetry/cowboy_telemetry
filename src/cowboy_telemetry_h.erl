-module(cowboy_telemetry_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-record(state, {
    next :: any(),

    streamid :: cowboy_stream:streamid(),
    connection_process :: pid(),
    request_process :: pid(),

    start_time :: integer(),

    emit :: undefined | stop | exception | done,

    response :: undefined | {response, cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body()},
    error_response :: undefined | {error_response, cowboy:http_status(), cowboy:http_headers(), iodata()},
    reason :: undefined | any()
}).

init(StreamID, Req, Opts) ->
    StartTime = emit_start_event(StreamID, Req),
    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    % io:format("~ninit ~p~n~p~n", [Commands, Req]),
    {Commands, fold(Commands, #state{
        next=Next,
        streamid=StreamID,
        connection_process=self(),
        start_time=StartTime
    })}.

data(StreamID, IsFin, Data, State=#state{next=Next0}) ->
    {Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
    io:format("~ndata ~p~n", [Commands]),
    {Commands, State#state{next=Next}}.

% Request Flows:
%
% init -> response^^ -> stop -> terminate(normal)         == successful_request
% init -> error_response^^ -> terminate(internal_error)   == failed_request
% init -> terminate^^(connection_error)                   == idle_timeout_request
% init -> terminate^^(socket_error)                        == client_timeout_request
% init -> headers -> data -> datap^^(fin) -> stop -> terminate(normal)  == stream_request
%
% early_error

info(StreamID, Info, State=#state{next=Next0}) ->
    {Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
    io:format("~ninfo ~p~n", [Commands]),
    State1 = fold(Commands, State#state{next=Next}),
    emit_at_end(State1),
    {Commands, State1}.


terminate(StreamID, Reason, #state{emit=done, next=Next}) ->
    io:format("~nterminate(done) ~p~n", [Reason]),
    cowboy_stream:terminate(StreamID, Reason, Next);

terminate(StreamID, {socket_error, _, _} = Reason, #state{next=Next} = State) ->
    io:format("~nterminate^^(socket_error) ~p~n", [Reason]),
    emit_at_end(State#state{emit=error, reason=Reason}),
    cowboy_stream:terminate(StreamID, Reason, Next);

terminate(StreamID, {connection_error, _, _} = Reason, #state{next=Next} = State) ->
    io:format("~nterminate^^(connection_error) ~p~n", [Reason]),
    emit_at_end(State#state{emit=error, reason=Reason}),
    cowboy_stream:terminate(StreamID, Reason, Next);

terminate(StreamID, Reason, #state{next=Next}) ->
    io:format("~nterminate ~p~n", [Reason]),
    cowboy_stream:terminate(StreamID, Reason, Next).


early_error(StreamID, Reason, PartialReq, Resp0, Opts) ->
    io:format("~nearly_error ~p~n", [Reason]),
    Resp = cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp0, Opts),
    emit_early_error_event(StreamID, Reason, PartialReq, Resp),
    Resp.

% fold over Commands

fold([], State) ->
    State;

fold([{response, _, _, _} = Response | Tail], State0) ->
    State = State0#state{response=Response, emit=stop},
    fold(Tail, State);

fold([{headers, RespStatus, RespHeaders} | Tail], State0) ->
    State = State0#state{response={response, RespStatus, RespHeaders, nil}},
    fold(Tail, State);

fold([{data, fin, _} | Tail], State0) ->
    State = State0#state{emit=stop},
    fold(Tail, State);

fold([{error_response, _, _, _} = ErrorResponse | Tail], State0) ->
    State = State0#state{error_response=ErrorResponse},
    fold(Tail, State);

fold([{internal_error, {'EXIT', _, Reason}, _} | Tail], State0) ->
    State = State0#state{reason=Reason, emit=exception},
    fold(Tail, State);

fold([{spawn, Pid, _} | Tail], State0) ->
    State = State0#state{request_process=Pid},
    fold(Tail, State);

fold([_ | Tail], State) ->
    fold(Tail, State).

% Emit when signaled

emit_at_end(#state{emit=stop, streamid=StreamID, start_time=StartTime, response=Response} = State) ->
    emit_stop_event(StreamID, StartTime, Response),
    State#state{emit=done};

emit_at_end(#state{emit=error, streamid=StreamID, start_time=StartTime, reason=Reason} = State) ->
    emit_stop_error_event(StreamID, StartTime, Reason),
    State#state{emit=done};

emit_at_end(#state{emit=exception, streamid=StreamID, start_time=StartTime, error_response=ErrorResponse, reason=Reason} = State) ->
    emit_exception_event(StreamID, StartTime, Reason, ErrorResponse),
    State#state{emit=done};

emit_at_end(State) ->
    State.

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

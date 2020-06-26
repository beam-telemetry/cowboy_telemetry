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
% Commands returned by cowboy in `info`, or data in `terminate`.
%
% successful_request     == init -> info(response) ^^ -> info(stop) -> terminate(normal)
% failed_request         == init -> info(error_response) ^^ -> terminate(internal_error)
% client_timeout_request == init -> terminate(socket_error) ^^
% idle_timeout_request   == init -> terminate(connection_error) ^^
% chunked_request        == init -> info(headers) -> info(data|nofin) -> info(data|fin) ^^ -> info(stop) -> terminate(normal)
% chunk_timeout_request  == init -> info(headers) -> info(data|nofin) -> terminate(connection_error) ^^

% Data that needs to be accumulated across handler callbacks
-record(state, {
    next :: any(),

    % Request info
    streamid :: cowboy_stream:streamid(),
    start_time :: integer(),

    % Span stop tracking
    emit :: undefined | done,

    % Chunked response data
    chunked_resp_status :: undefined | cowboy:http_status(),
    chunked_resp_headers :: undefined | cowbo:http_headers()
}).

% Data that needs to be accumulated while we fold over Commands
-record(acc, {
    req :: undefined | cowboy_req:req(),
    system_time :: undefined | erlang:system_time(),
    error_response :: undefined | {error_response, cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body()}
}).

init(StreamID, Req, Opts) ->
    StartTime = erlang:monotonic_time(),
    SystemTime = erlang:system_time(),
    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    {Commands, fold(Commands,
                    #state{next=Next, streamid=StreamID, start_time=StartTime},
                    #acc{req=Req, system_time=SystemTime})}.

info(StreamID, Info, State=#state{next=Next0}) ->
    {Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
    {Commands, fold(Commands,
                    State#state{next=Next},
                    #acc{})}.

data(StreamID, IsFin, Data, State=#state{next=Next0}) ->
    {Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
    {Commands, State#state{next=Next}}.

terminate(StreamID, Reason, #state{emit=done, next=Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next);
terminate(StreamID, {ErrorType, _, _} = Reason, #state{next=Next, start_time=StartTime})
    when ErrorType == socket_error;
         ErrorType == connection_error ->
    emit_stop_error_event(StreamID, StartTime, Reason),
    cowboy_stream:terminate(StreamID, Reason, Next);
terminate(StreamID, Reason, #state{next=Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

early_error(StreamID, Reason, PartialReq, Resp0, Opts) ->
    Resp = cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp0, Opts),
    emit_early_error_event(StreamID, Reason, PartialReq, Resp),
    Resp.

%

fold([], State, _Acc) ->
    State;

fold([{response, _, _, _} = Response | Tail],
     #state{streamid=StreamID, start_time=StartTime} = State,
     Acc) ->
    emit_stop_event(StreamID, StartTime, Response),
    fold(Tail, State#state{emit=done}, Acc);

fold([{data, fin, _} | Tail],
     #state{streamid=StreamID, start_time=StartTime, chunked_resp_status=RespStatus, chunked_resp_headers=RespHeaders} = State,
     Acc) ->
    emit_stop_event(StreamID, StartTime, {response, RespStatus, RespHeaders, nil}),
    fold(Tail, State#state{emit=done, chunked_resp_status=undefined, chunked_resp_headers = undefined}, Acc);

fold([{internal_error, {'EXIT', _, Reason}, _} | Tail],
     #state{streamid=StreamID, start_time=StartTime} = State,
     #acc{error_response=ErrorResponse}) ->
    emit_exception_event(StreamID, StartTime, Reason, ErrorResponse),
    fold(Tail, State#state{emit=done}, #acc{});

fold([{headers, RespStatus, RespHeaders} | Tail], State, Acc) ->
    fold(Tail, State#state{chunked_resp_status=RespStatus, chunked_resp_headers=RespHeaders}, Acc);

fold([{error_response, _, _, _} = ErrorResponse | Tail], State, Acc) ->
    fold(Tail, State, Acc#acc{error_response=ErrorResponse});

fold([{spawn, RequestProcess, _} | Tail],
     #state{streamid=StreamID} = State,
     #acc{req=Req, system_time=SystemTime}) ->
    emit_start_event(StreamID, SystemTime, Req, RequestProcess),
    fold(Tail, State, #acc{});

fold([_ | Tail], State, Acc) ->
    fold(Tail, State, Acc).

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

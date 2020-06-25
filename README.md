cowboy_telemetry
=====

[![Hex.pm Version](https://img.shields.io/hexpm/v/cowboy_telemetry.svg)](https://hex.pm/packages/cowboy_telemetry)
[![Erlang CI](https://github.com/beam-telemetry/cowboy_telemetry/workflows/Erlang%20CI/badge.svg?branch=main)](https://github.com/beam-telemetry/cowboy_telemetry/actions)

[Telemetry](https://github.com/beam-telemetry/telemetry) instrumentation for the [Cowboy](https://github.com/ninenines/cowboy) HTTP server.

This package contains a [`cowboy_stream`](https://ninenines.eu/docs/en/cowboy/2.8/manual/cowboy_stream/) handler that will instrument each request and emit `telemetry` events.

## Usage

Configure your cowboy server with the `cowboy_telemetry_h` stream handler first.

```erlang
cowboy:start_clear(http, [{port, Port}], #{
    env => #{dispatch => Dispatch},
    stream_handlers => [cowboy_telemetry_h, cowboy_stream_h]
}.
```

## Telemetry Events

#### `[cowboy, request, start]`

A span event emitted at the beginning of a request.

* `measurements`: `#{system_time => erlang:system_time()}`
* `metadata`: `#{stream_id => cowboy_stream:streamid(), req => cowboy_req:req()}`

#### `[cowboy, request, stop]`

A span event emitted at the end of a request.

* `measurements`: `#{duration => native_time}`
* `metadata`: `#{stream_id => cowboy_stream:streamid(), response => cowboy_stream:resp_command()}`

If the request is terminated early - by the client or by the server - before a response is sent, the metadata contains an `error` instead of a `response`,

* `metadata`: `#{stream_id => cowboy_stream:streamid(), error => early_termination_error()}`

#### `[cowboy, request, exception]`

A span event emitted if the request process exits.

* `measurements`: `#{duration => native_time}`
* `metadata`: `#{stream_id => cowboy_stream:streamid(), kind => exit, reason => any(), response => error_response()}`

#### `[cowboy, request, early_error]`

A single event emitted when Cowboy itself returns an `early_error` response before executing any handlers.

* `measurements`: `#{duration => native_time}`
* `metadata`: `#{stream_id => cowboy_stream:streamid(), reason => cowboy_stream:reason(), partial_req => cowboy_stream:partial_req()}`

Additional types for reference:

```erlang
- type error_response() :: {error_response, cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body()}.

- type early_termination_error() :: {socket_error, closed | atom(), cowboy_stream:human_reason()}
                                    | {connection_error, timeout, cowboy_stream:human_reason()}.
```

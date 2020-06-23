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

Span events emitted:

* `[cowboy, request, start]`
  * `measurements`: `#{system_time => erlang:system_time()}`
  * `metadata`: `#{stream_id => cowboy_stream:streamid(), req => cowboy_req:req()}`

* `[cowboy, request, stop]`
  * `measurements`: `#{duration => native_time}`
  * `metadata`: `#{stream_id => cowboy_stream:streamid(), response => response()}`

* `[cowboy, request, exception]`
  * `measurements`: `#{duration => native_time}`
  * `metadata`: `#{stream_id => cowboy_stream:streamid(), kind => exit, reason => any(), response => error_response()}`

A single event is emitted when `cowboy` returns an `early_error` response:

* `[cowboy, request, early_error]`
  * `measurements`: `#{duration => native_time}`
  * `metadata`: `#{stream_id => cowboy_stream:streamid(), reason => cowboy_stream:reason(), partial_req => cowboy_stream:partial_req()}`

Additional types for reference:

```erlang
- type response() :: {response, cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body()}.
- type error_response() :: {error_response, cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body()}.
```

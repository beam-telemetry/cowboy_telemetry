-module(test_h).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, Opts) ->
  {ok, cowboy_req:reply(200, #{}, <<"Hello world!">>, Req), Opts}.

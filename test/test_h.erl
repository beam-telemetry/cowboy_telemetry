-module(test_h).
-behaviour(cowboy_handler).

-export([init/2]).

init(_, failure) ->
    error(failure);
init(Req, slow = Opts) ->
    timer:sleep(100),
    {ok, cowboy_req:reply(200, #{}, <<"I'm slow">>, Req), Opts};
init(Req, success = Opts) ->
    {ok, cowboy_req:reply(200, #{}, <<"Hello world!">>, Req), Opts}.

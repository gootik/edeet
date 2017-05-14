%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Vungle
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(edeet_api_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
         {{<<"application">>, <<"json">>, []}, json_response}
     ], Req, State}.
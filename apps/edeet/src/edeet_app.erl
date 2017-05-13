%%%-------------------------------------------------------------------
%% @doc edeet public API
%% @end
%%%-------------------------------------------------------------------

-module(edeet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                                         {'_', [
                                             {"/", cowboy_static, {priv_file, edeet, "index.html"}},
                                             {"/websocket", edeet_ws_handler, []},
                                             {"/static/[...]", cowboy_static, {priv_dir, edeet, "static"}}
                                         ]}
                                     ]),

    cowboy:start_clear(http, 100, [{port, 8099}], #{
        env => #{dispatch => Dispatch}
    }),

    edeet_sup:start_link().

stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%% @doc edeet public API
%% @end
%%%-------------------------------------------------------------------

-module(edeet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(edeet, port),

    Dispatch = cowboy_router:compile([{'_', [{"/", cowboy_static, {priv_file, edeet, "index.html"}},
                                             {"/api", edeet_api_handler, []},
                                             {"/websocket", edeet_ws_handler, []},
                                             {"/static/[...]", cowboy_static, {priv_dir, edeet, "static"}},
                                             {"/bower_components/[...]", cowboy_static, {priv_dir, edeet, "bower_components"}}

                                            ]}
                                     ]),

    cowboy:start_clear(http, 100, [{port, Port}], #{
        env => #{
            dispatch => Dispatch
        }
    }),

    lager:info("Started server on port ~p", [Port]),

    edeet_sup:start_link().

stop(_State) ->
    ok.

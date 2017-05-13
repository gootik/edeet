%%%-------------------------------------------------------------------
%% @doc edeet top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(edeet_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

    edeet_document:init(),

    ConnectionManager = #{
        id => edeet_connection_manager,
        start => {edeet_connection_manager, start_link, []},
        type => worker,
        restart => permanent
    },

    {ok, { {one_for_all, 0, 1}, [ConnectionManager]} }.
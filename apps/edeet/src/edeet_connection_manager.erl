%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Vungle
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(edeet_connection_manager).

-behavior(gen_server).

-export([
    start_link/0,
    add_connection/1,
    broadcast/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    connections = []
}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

add_connection(Connection) ->
    gen_server:cast({global, ?MODULE}, {add_connection, Connection}).

broadcast(Message) ->
    gen_server:cast({global, ?MODULE}, {broadcast, self(), Message}).

init(_) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({add_connection, Connection}, #state{connections = Connections} = State) ->
    _ = monitor(process, Connection),
    {noreply, State#state{connections = [Connection | Connections]}};

handle_cast({broadcast, From, Message}, #state{connections = Connections} = State) ->
    lists:foreach(
        fun
            (Connection) when Connection =/= From ->
                Connection ! {send, Message};
            (_) ->
                ok
        end, Connections),

    {noreply, State}.


handle_info({'DOWN', _, process, Connection, normal}, #state{connections = Connections} = State) ->
    NewState = State#state{
        connections = lists:delete(Connection, Connections)
    },

    {noreply, NewState};

handle_info(Info, State) ->
    io:format(user, "Connection manager got ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
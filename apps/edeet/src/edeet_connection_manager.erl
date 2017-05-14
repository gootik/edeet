%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(edeet_connection_manager).

-behavior(gen_server).

-export([
    start_link/0,
    add_connection/2,
    broadcast/2
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
    connections = #{},
    monitors = #{}
}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

add_connection(DocId, Connection) ->
    gen_server:cast({global, ?MODULE}, {add_connection, DocId, Connection}).

broadcast(DocId, Message) ->
    gen_server:cast({global, ?MODULE}, {broadcast, self(), DocId, Message}).

init(_) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({add_connection, DocId, Connection}, #state{connections = Connections, monitors = Monitors} = State) ->
    MonRef = monitor(process, Connection),

    ConList = maps:get(DocId, Connections, []),

    {noreply, State#state{connections = Connections#{DocId => [Connection | ConList]},
                          monitors    = Monitors#{MonRef => DocId}}};

handle_cast({broadcast, From, DocId, Message}, #state{connections = Connections} = State) ->
    ConList = maps:get(DocId, Connections, []),

    lists:foreach(
        fun
            (Connection) when Connection =/= From ->
                Connection ! {send, Message};
            (_) ->
                ok
        end, ConList),

    {noreply, State}.


handle_info({'DOWN', MonRef, process, Connection, _}, #state{connections = Connections, monitors = Monitors} = State) ->

    case maps:get(MonRef, Monitors, no_document) of
        no_document ->
            NewMonitors = maps:remove(MonRef, Monitors),
            {noreply, State#state{monitors = NewMonitors}};

        DocId ->
            ConList = maps:get(DocId, Connections, []),
            NewConList = lists:delete(Connection, ConList),

            NewState = State#state{
                connections = Connections#{DocId => NewConList}
            },

            ConnectionClosedMessage = jsone:encode(#{broadcast => true,
                                                     lost => list_to_binary(pid_to_list(Connection))}),

            handle_cast({broadcast, self(), DocId, ConnectionClosedMessage}, NewState)
    end;

handle_info(Info, State) ->
    io:format(user, "Connection manager got ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
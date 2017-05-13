%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Vungle
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(edeet_ws_handler).


-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).


-record(state, {
    text = <<>>,
    name = <<>>
}).

init(Req, _Opts) ->
    {cowboy_websocket, Req, #state{}, #{
        idle_timeout => 60000
    }}.


websocket_init(State) ->
    edeet_connection_manager:add_connection(self()),

    {ok, State}.

websocket_handle({text, Message}, State) ->
    DecodedMessage = jsone:decode(Message, [{object_format, map}]),
    handle_message(DecodedMessage, State).

websocket_info({send, Message}, State) ->
    {reply, {text, Message}, State}.


handle_message(#{<<"type">> := <<"init">>, <<"username">> := Username}, State) ->
    Text = edeet_document:get_document(),
    JsonMessage = jsone:encode(#{init => true,
                                 broadcast => true,
                                 text => Text}),

    ConnectionMessage = jsone:encode(#{connection => true,
                                       id => list_to_binary(pid_to_list(self())),
                                       name => Username}),

    edeet_connection_manager:broadcast(ConnectionMessage),

    {reply, {text, JsonMessage}, State#state{name = Username}};

handle_message(#{<<"type">> := <<"edit">>, <<"message">> := Text}, State) ->
    JsonMessage = jsone:encode(#{broadcast => true,
                                 text => Text}),

    edeet_document:edit_document(Text),
    edeet_connection_manager:broadcast(JsonMessage),

    {ok, State#state{text = Text}}.
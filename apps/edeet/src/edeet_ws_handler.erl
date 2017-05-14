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
    doc_id = undefined,
    name = <<>>
}).

init(Req, _Opts) ->
    {cowboy_websocket, Req, #state{}, #{
        idle_timeout => 60000
    }}.


websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Message}, State) ->
    DecodedMessage = jsone:decode(Message, [{object_format, map}]),
    handle_message(DecodedMessage, State).

websocket_info({send, Message}, State) ->
    {reply, {text, Message}, State}.


handle_message(#{<<"type">> := <<"init">>, <<"username">> := Username, <<"document">> := Document}, State) ->
    DocInfo = case Document of
        null ->
            edeet_document:new();
        Document ->
            edeet_document:get_document(Document)
    end,

    case DocInfo of
        no_document ->
            JsonMessage = jsone:encode(#{error => no_document}),

            {stop, {text, JsonMessage}, State#state{name = Username}};

        {DocId, Text} ->
            JsonMessage = jsone:encode(#{init => true,
                                         broadcast => true,
                                         doc_id => DocId,
                                         text => Text}),

            ConnectionMessage = jsone:encode(#{connection => true,
                                               id => list_to_binary(pid_to_list(self())),
                                               name => Username}),

            edeet_connection_manager:broadcast(DocId, ConnectionMessage),
            edeet_connection_manager:add_connection(DocId, self()),

            {reply, {text, JsonMessage}, State#state{name = Username, doc_id = DocId}}
    end;


handle_message(#{<<"type">> := <<"edit">>, <<"message">> := Text}, #state{doc_id = DocId} = State) ->
    JsonMessage = jsone:encode(#{broadcast => true,
                                 text => Text}),

    edeet_document:edit_document(DocId, Text),
    edeet_connection_manager:broadcast(DocId, JsonMessage),

    {ok, State}.
%%%-------------------------------------------------------------------
%%% @doc Handles connections on the /websocket endpoint and promotes them
%%%      to a websocket connection. Handles the messaging between the
%%%      server and the socket on the client side.
%%% @end
%%%-------------------------------------------------------------------
-module(edeet_ws_handler).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

%% Which document this socket has opened and who it is.
%%
%% TODO(shezarkhani): This should be a user id of some sort
%%                    not just name...
-record(state, {
    doc_id = undefined,
    name = <<>>
}).

%% @doc Promote and let the person stay connected for 10 minutes.
init(Req, _Opts) ->
    {cowboy_websocket, Req, #state{}, #{
        idle_timeout => 600000
    }}.

websocket_init(State) ->
    {ok, State}.

%% @doc We assume a JSON message on the socket. This method decodes and
%%      handle the message.
%%
%%      TODO(shezarkhani): The format of the message is not uniform.
%%                         Need to rethink the message structure.
%% @end
websocket_handle({text, Message}, State) ->
    DecodedMessage = jsone:decode(Message, [{object_format, map}]),
    handle_message(DecodedMessage, State).

%% @doc If the server wants to communicate with the client, it needs to
%%      send a `{send, Message}' tuple message to this process.
%% @end
websocket_info({send, Message}, State) ->
    {reply, {text, Message}, State}.

%% @doc As soon as the client connects, it will send a init message to get the
%%      initial state of the document it is trying to open.
%% @end
handle_message(#{<<"type">> := <<"init">>, <<"username">> := Username, <<"doc_id">> := DocId} = Message, State) ->
    % If no document id has been passed, create a new one. Otherwise try and
    % load the document.
    DocInfo = case DocId of
        null ->
            DocName = maps:get(<<"doc_name">>, Message),
            edeet_document:new(DocName);
        _ ->
            edeet_document:get_document(DocId)
    end,

    case DocInfo of
        no_document ->
            % If we could not find the document, error and close the connection. We
            % don't want spammers to overload the server.
            {stop, State#state{name = Username}};

        {InternalDocId, _, Text} ->

            lager:info("~p joined document ~p", [Username, InternalDocId]),

            notify_join(InternalDocId, Username),

            % We let the client know what the initial state of the document is.
            JsonMessage = jsone:encode(#{init => true,
                                         broadcast => true,
                                         doc_id => InternalDocId,
                                         text => Text}),

            {reply, {text, JsonMessage}, State#state{name = Username, doc_id = InternalDocId}};

        UnknownMessage ->
            lager:error("There was an error on socket init: ~p", [UnknownMessage]),
            ErrorMessage = jsone:encode(#{error => 0}),
            {reply, {text, ErrorMessage}, State}

    end;


%% @doc When the client edits a document it will send an edit message and expects
%%      that to be broadcasted to everyone else.
%%
%%      TODO(shezarkhani): This method needs to send back the latest version, if
%%                         it has changed.
%% @end
handle_message(#{<<"type">> := <<"edit">>, <<"message">> := Text}, #state{doc_id = DocId} = State) ->

    edeet_document:edit_document(DocId, Text),

    JsonMessage = jsone:encode(#{broadcast => true,
                                 text => Text}),
    edeet_connection_manager:broadcast(DocId, JsonMessage),

    {ok, State};

handle_message(Msg, State) ->
    lager:warning("Websocket got an unknown message: ~p", [Msg]),
    {ok, State}.

notify_join(DocId, Username) ->
    ConnectionMessage = jsone:encode(#{connection => true,
                                       id => list_to_binary(pid_to_list(self())),
                                       name => Username}),

    edeet_connection_manager:broadcast(DocId, ConnectionMessage),
    edeet_connection_manager:add_connection(DocId, self()).
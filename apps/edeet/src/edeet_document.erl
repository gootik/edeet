%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Vungle
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(edeet_document).

-export([
    init/0,
    edit_document/1,
    get_document/0
]).

init() ->
    _ = ets:new(edeet_documents, [public,
                                  named_table,
                                  set,
                                  {read_concurrency, true},
                                  {write_concurrency, true}]),

    ets:insert(edeet_documents, {doc1, <<>>}).

get_document() ->
    [{_, Bin}] = ets:lookup(edeet_documents, doc1),
    Bin.

edit_document(Bin) ->
    true = ets:insert(edeet_documents, {doc1, Bin}).

%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(edeet_document).

-export([
    init/0,
    edit_document/2,
    get_document/1,
    new/0,
    get_all/0
]).

init() ->
    _ = ets:new(edeet_documents, [public,
                                  named_table,
                                  set,
                                  {read_concurrency, true},
                                  {write_concurrency, true}]).
new() ->
    DocId = generate_doc_id(),
    true = ets:insert(edeet_documents, {DocId, <<>>}),

    {DocId, <<>>}.

get_all() ->
    get_keys(ets:first(edeet_documents), []).

get_keys('$end_of_table', Acc) ->
    Acc;
get_keys(Key, Acc) ->
    get_keys(ets:next(edeet_documents, Key), [Key | Acc]).

get_document(DocId) ->
    case ets:lookup(edeet_documents, DocId) of
        [{DocId, Bin}] ->
            {DocId, Bin};
        _ ->
            no_document
    end.

edit_document(DocId, Bin) ->
    true = ets:insert(edeet_documents, {DocId, Bin}).

-spec generate_doc_id() -> binary().
generate_doc_id() ->
    integer_to_binary(rand:uniform(100)).
%%% @doc Wrapper module for Jira's REST API
-module(jira).

%% API
-export([ search/3
        , transitions/3
        , transition/4
        ]).

%% =====================================================================
%% API

search(Server, Cookie, Query) ->
    Args = [{<<"jql">>, Query}, {<<"fields">>, "summary,description"}],
    get(Server, Cookie, "/search", Args).

transitions(Server, Cookie, IssueId) ->
    Req = "/issue/" ++ IssueId ++ "/transitions",
    get(Server, Cookie, Req, []). 

transition(Server, Cookie, IssueId, TransitionNames) ->
    %% Figure out the id of the transition
    Transitions = proplists:get_value(<<"transitions">>,
                                      transitions(Server, Cookie, IssueId)),
    Ids = [proplists:get_value(<<"id">>, Transition)
           || Transition <- Transitions,
              lists:member(proplists:get_value(<<"name">>, Transition),
                           TransitionNames)
          ],
    case Ids of
        [Id | _] ->
            Body = [ {update,
                      [ {comment,
                         [[ {add, [{body, <<"I'm feeling lucky!">>}]}]]}]}
                   , {transition, [{id, Id}]}
                   ],
            Req = "/issue/" ++ IssueId ++ "/transitions",
            post(Server, Cookie, Req, [], Body);
        [] ->
            throw({no_such_transition, IssueId, TransitionNames})
    end.

%% =====================================================================
%% Internal helper functions

get(Server, Cookie, Req, Args) ->
    Json = call(get, Server, Cookie, Req, Args, []),
    jsx:decode(Json).

post(Server, Cookie, Req, Args, Json) ->
    call(post, Server, Cookie, Req, Args, jsx:encode(Json)),
    ok.

call(Method, Server, Cookie, Req, Args, Body) ->
    Url = rest_api_url(Server, Req, Args),
    Headers = [{"Content-Type", "application/json"},
               {"Cookie", Cookie}],
    Options = [{response_format, binary}],
    {ok, "20" ++ _, _RespHeaders, RespBody} =
        ibrowse:send_req(Url, Headers, Method, Body, Options),
    RespBody.

rest_api_url(Server, Req, Args) ->
    "https://" ++ Server ++ "/rest/api/2" ++ Req ++ qs(Args).

qs([]) ->
    [];
qs(L) ->
    "?" ++ unicode:characters_to_list(
             cow_qs:qs([{K, val_to_binary(V)} || {K, V} <- L])).

val_to_binary(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N));
val_to_binary(A) when is_atom(A) ->
    atom_to_binary(A, unicode);
val_to_binary(S) when is_list(S) ->
    unicode:characters_to_binary(S).

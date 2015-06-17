%%% @doc Wrapper module for Jira's REST API
-module(jira).

%% API
-export([ search/2
        , transitions/2
        , transition/3
        ]).

%% Constants
-define(cookie, "<put here the actual secret>").

%% =====================================================================
%% API

search(Server, Query) ->
    Fields = "summary,description",
    get(Server, "/search", [{<<"jql">>, Query}, {<<"fields">>, Fields}]).

transitions(Server, IssueId) ->
    get(Server, "/issue/" ++ IssueId ++ "/transitions", []). 

transition(Server, IssueId, TransitionNames) ->
    %% Figure out the id of the transition
    Transitions = proplists:get_value(<<"transitions">>,
                                      transitions(Server, IssueId)),
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
            post(Server, "/issue/" ++ IssueId ++ "/transitions", [], Body);
        [] ->
            throw({no_such_transition, IssueId, TransitionNames})
    end.

%% =====================================================================
%% Internal helper functions

get(Server, Req, Args) ->
    Json = call(get, Server, Req, Args, []),
    jsx:decode(Json).

post(Server, Req, Args, Json) ->
    call(post, Server, Req, Args, jsx:encode(Json)),
    ok.

call(Method, Server, Req, Args, Body) ->
    Url = rest_api_url(Server, Req, Args),
    Headers = [{"Content-Type", "application/json"},
               {"Cookie", ?cookie}],
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

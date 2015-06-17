-module(feeling_lucky_4_jira_issue_controller, [Req]).

%% Actions
-export([ top/2
        , all/2
        , complete/2
        , reject/2
        , return/2
        ]).

%% =====================================================================
%% Actions

top('POST', []) ->
    {ok, Issue} = issue_lib:from_jira(candidate_for_assign()),
    ok = issue_lib:transition(start_work, Issue),
    {json, issue_lib:to_json(Issue)};
top('GET', []) ->
    case issue_lib:issues() of
        []          -> not_found;
        [Issue | _] -> {json, issue_lib:to_json(Issue)}
    end.

all('GET', []) ->
    Issues = [issue_lib:to_json(Issue) || Issue <- issue_lib:issues()],
    {json, [{issues, Issues}]}.

complete('POST', []) ->
    remove(complete);
complete('POST', [Id]) ->
    remove(complete, Id).

reject('POST', []) ->
    remove(reject);
reject('POST', [Id]) ->
    remove(reject, Id).

return('POST', []) ->
    remove(return);
return('POST', [Id]) ->
    remove(return, Id).

%% =====================================================================
%% Internal helper functions

remove(Reason, Id) ->
    case boss_db:find(issue_lib:internal_id(Id)) of
        undefined -> not_found;
        Issue     -> do_remove(Reason, Issue)
    end.

remove(Reason) ->
    case issue_lib:issues() of
        []          -> not_found;
        [Issue | _] -> do_remove(Reason, Issue)
    end.

do_remove(Reason, Issue) ->
    ok = issue_lib:transition(Reason, Issue),
    ok = boss_db:delete(Issue:id()),
    {json, [{time, Issue:elapsed_time()} | issue_lib:to_json(Issue)]}.

candidate_for_assign() ->
    List = candidates_for_assign(),
    lists:nth(crypto:rand_uniform(1, length(List) + 1), List).

candidates_for_assign() ->
    Json = jira:search(issue_lib:server(),
                       issue_lib:cookie(),
                       issue_lib:backlog()),
    proplists:get_value(<<"issues">>, Json, []).

-module(issue_lib).

-export([ server/0
        , from_jira/1
        , to_json/1
        , jira_id/1
        , internal_id/1
        , issues/0
        , transition/2
        ]).

-define(skip_transitions, false).

server() ->
    "jira-dev.internal.machines".

transition(_, _) when ?skip_transitions ->
    ok;
transition(Transition, Issue) ->
    TransitionNames =
        case Transition of
            start_work -> [<<"Start Work">>, <<"Start work">>];
            complete   -> [<<"Release">>, <<"Deliver">>];
            reject     -> [<<"Reject">>];
            return     -> [<<"Stop Work">>, <<"Stop work">>]
        end,
    Id = unicode:characters_to_list(Issue:jira_id()),
    try jira:transition(server(), Id, TransitionNames) of
        ok -> ok
    catch
        _:E ->
            error_logger:error_msg("Transition failed!~n~p~n", [E]),
            ok
    end.

issues() ->
    lists:sort(fun (I1, I2) -> I1:stack_time() >= I2:stack_time() end,
               boss_db:find(issue, [], [])).

from_jira(JiraIssue) ->
    Id     = internal_id(proplists:get_value(<<"key">>, JiraIssue)),
    Fields = proplists:get_value(<<"fields">>, JiraIssue),
    Title  = null_to_no_string(proplists:get_value(<<"summary">>, Fields)),
    Body   = null_to_no_string(proplists:get_value(<<"description">>, Fields)),
    Now    = os:timestamp(),
    Issue  = issue:new(Id, Title, Body, Now, Now),
    Issue:save().

to_json(Issue) ->
    [ {title, Issue:title()}
    , {id,    Issue:jira_id()}
    , {url,   Issue:url()}
    , {body,  Issue:body()}
    ].

internal_id(JiraId) when is_binary(JiraId) ->
    [_GBL, Id] = binary:split(JiraId, <<$->>),
    "issue-" ++ unicode:characters_to_list(Id);
internal_id(JiraId) ->
    internal_id(unicode:characters_to_binary(JiraId)).

jira_id(IssueId) ->
    "issue-" ++ Id = IssueId,
    <<"GBL-", (unicode:characters_to_binary(Id))/binary>>.

null_to_no_string(null)                -> <<>>;
null_to_no_string(B) when is_binary(B) -> B. 

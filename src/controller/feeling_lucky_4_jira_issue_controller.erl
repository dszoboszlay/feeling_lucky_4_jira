-module(feeling_lucky_4_jira_issue_controller, [Req]).

-export([ assign_new/2
        ]).

assign_new('POST', []) ->
    {json, [ {title, <<"Hello World!">>}
           , {id,    <<"DEMO-1">>}
           , {url,   <<"https://jira.atlassian.com/browse/DEMO-1">>}
           ]}.


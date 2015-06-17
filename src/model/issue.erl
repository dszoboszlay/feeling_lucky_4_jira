-module(issue, [ Id        :: string()
               , Title     :: binary()
               , Body      :: binary()
               , StartTime :: timestamp()
               , StackTime :: timestamp()
               ]).

-export([ jira_id/0
        , url/0
        , elapsed_time/0
        ]).

jira_id() ->
    issue_lib:jira_id(Id).

url() ->
    <<(unicode:characters_to_binary(
         "https://" ++ issue_lib:server() ++ "/browse/"))/binary,
      (issue_lib:jira_id(Id))/binary
    >>.

elapsed_time() ->
    Now = calendar:now_to_datetime(os:timestamp()),
    T0 = calendar:datetime_to_gregorian_seconds(StartTime),
    T1 = calendar:datetime_to_gregorian_seconds(Now),
    T1 - T0.

# Feeling Lucky 4 Jira

A [klic hackathon](http://www.klarnahackathons.com/klic/) project.

The Feeling Lucky Button for Jira would assign a random issue to you
from your team's backlog by a simple click of your
[Flic](http://www.flic.io/).

Why would you do that? To get outside your comfort zone! Let's admit,
whenever we pick the next task to work on, we unconsciously avoid
choosing tasks we don't prefer (_"Oh, that one much better suits
Joe!"_ or simply _"Too boring!"_ and sometimes maybe _"No way I could
solve that hard thing!"_). The Flic would give a fair chance to every
issue, and hey, you may even learn something valuable or discover
something fun!

## Disclaimer

This project was developed during a 1 day hackathon. It is definitely
**far from being production quality**.

## Components

The solution consists of the following components:

* The Flic button: offers three input actions (click, double click and
  long hold).

* The iOS frontend: responds to the Flic actions by sending REST
  requests to the backend to assign a new issue to the user, complete
  (or reject) the last assigned issue or return it to the
  backlog. (But don't use return too often, it is considered cheating!
  :wink:)

  The app also offers a simple GUI for seeing which issues were
  assigned to the user.

* The Erlang backend: it receives the REST requests from the frontend
  and talks to the Jira server via its REST API. It maps the assign,
  complete, reject and return actions to state transitions in Jira. In
  addition it is also possible to query the issues assigned to the
  user via this service.

  Of course the backend can be used with different frontends too.

This repository contains the backend code only. It is a standard
[Chicago Boss](http://chicagoboss.org/) project.

## Keeping track of issues

The backend will keep track of issues assigned to the user in a
stack. The assign action chooses an issue randomly from the Jira
backlog and puts it on top of the stack.

The complete, reject and return actions by default pop the topmost
element of the stack and perform the action on it. There are also
parametric versions of these actions that can operate on any issue in
the stack (the issue will be removed nevertheless).

As a rule of thumb Feeling Lucky 4 Jira will only act on issues it has
assigned to the user on its own.

Please note that the stack of issues is stored in an in-memory DB. All
the information is lost when the backend is stopped. (At least it
would be quite trivial to change the DB engine to something durable
like Mnesia in the Chicago Boss framework.)

## REST API

The following endpoints are available:

* `POST /issue`: assign a new issue to the user.

* `GET /issue`: get the topmost issue from the stack.

* `POST /complete`: complete the topmost issue on the stack (e.g. put
  it into "Solved" state or something equivalent).

* `POST /reject`: reject the topmost issue on the stack (e.g. put it
  into "Rejected" state or something equivalent).

* `POST /return`: return the topmost issue on the stack (e.g. move it
  back into the backlog).

* `GET /issues`: get all issues in the stack.

* `POST /complete/<id>`: complete the specified issue (e.g. put it
  into "Solved" state or something equivalent).

* `POST /reject/<id>`: reject the specified issue (e.g. put it into
  "Rejected" state or something equivalent).

* `POST /return/<id>`: return the specified issue (e.g. move it back
  into the backlog).

The last three actions taking an issue id will only work on issues
listed by `GET /issues`, not on arbitrary issues assigned to the user
in Jira.

Except for `GET /issues` all API calls will return an issue in JSON
format:

```json
{
   "id"    : "JIRA-1234",
   "title" : "The title of the issue",
   "body"  : "The description of the issue. Beware of the Markdown syntax!",
   "url"   : "https://localhost/browse/JIRA-1234",
   "time"  : 60
}
```

The `"time"` field contains the time elapsed (in seconds) since the
issue was assigned (with `POST /issue`). Only responses from the
complete, reject and return actions contain this field.

`GET /issues` will return a list of these issue objects:

```json
{
   "issues" : [
      {
         "id"    : "JIRA-1234",
         "title" : "The title of the issue",
         "body"  : "The description of the issue. Beware of the Markdown syntax!",
         "url"   : "https://localhost/browse/JIRA-1234"
      }
   ]
}
```

## Configuration

The backend needs to be configured by setting some application
parameters in the `boss.config` file.

* `jira_host`: the address of the Jira server.

* `auth_cookie`: the session cookie to authenticate on the Jira
  server. (Log in to the Jira from a browser and copy the cookie
  headers sent by your browser afterwards here. Did I mention this is
  not production ready code? :trollface:)

* `backlog`: the Jira search query that shall return the unassigned
  issues in the backlog.

* `start_work`, `complete`, `reject` and `return`: the **list** of
  Jira transition names that correspond to the actions the backend
  offers.

  The reason for using a list instead of a single value is that these
  transition names are specific to the issue type, and user
  configurable. For example, you may _"Deliver"_ a Work Item but call
  _"Ready to Ship"_ on a Story. And people are not consistent, so be
  prepared for _"Ready to ship"_ as well for yet another issue type...

## The roughest corners

So once again, this code was written in a hackathon, and should be
improved in practically every aspect. The below list is only the tip
of the iceberg.

### Jira authentication

The code simply relies on using a session cookie. The proper solution
would be to use OAuth.

### User authentication

The user of the backend is not authenticated in any way (and the
connection isn't encrypted either, so much for privacy). This means
anybody who can connect to the server can also make changes in Jira in
the name of the Jira user who's cookie is used by backend.

### Single-user operation

The backend can only act in the name of a single user at the
moment. It cannot serve requests coming from multiple frontends that
belong to different Jira users.

Besides implementing user authentication the DB would have to change
too, to keep track which issue belongs to which user.

### Transitions

In Jira each issue type can have its own, custom workflow. The Feeling
Lucky service needs to know which workflow transitions map to its
actions. This is currently the responsibility of the user configuring
the service, and the tool does not give any kind of help to the task.

There can be three kind of problems (at least):

* The backend cannot find out which transition to use for an issue at
  hand. In that case it will silently ignore doing any change at
  all. (Actually an error gets logged, but that's not visible to the
  user of the REST API.)

* Jira may require to fill in some fields when doing a transition
  (e.g. write a comment, name a git branch or whatever). The backend
  cannot do that and the operation may fail if these fields are
  mandatory.

* The user is never explicitly assigned to the issue by the
  backend. It relies on the `start_work` transition to implicitly
  assign the issue to the user performing the operation and the
  `return` transition to unassign it. This is not necessary the case
  in Jira.


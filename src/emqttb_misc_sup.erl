-module(emqttb_misc_sup).

-behaviour(supervisor).

-export([start_link/0, start_worker/2]).

-export([init/1]).

-include("emqttb.hrl").

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_worker(Id, Start) ->
  {ok, _} = supervisor:start_child(?SERVER,
                                   #{ id => Id
                                    , type => worker
                                    , start => Start
                                    , shutdown => timer:seconds(1)
                                    }).

init([]) ->
  SupFlags = #{ strategy => one_for_one
              , intensity => 10
              , period => 1
              },
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions

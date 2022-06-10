-module(emqttb_group_sup).

-behaviour(supervisor).

-export([start_link/0, ensure/1]).

-export([init/1]).

-include("emqttb.hrl").

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec ensure(emqttb_group:group_config()) -> ok.
ensure(Options = #{id := Id}) ->
  Result = supervisor:start_child(?SERVER,
                                  #{ id => Id
                                   , type => worker
                                   , start => {emqttb_group, start_link, [Options]}
                                   , shutdown => timer:seconds(1)
                                   }),
  case Result of
    {ok, _} ->
      ok;
    {error, {already_started, _}} ->
      ok;
    {error, already_present} ->
      supervisor:restart_child(?SERVER, Id)
  end.

init([]) ->
  SupFlags = #{ strategy => one_for_one
              , intensity => 1
              , period => 10
              },
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions

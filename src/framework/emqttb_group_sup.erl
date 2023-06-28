-module(emqttb_group_sup).

-behaviour(supervisor).

-export([start_link/0, ensure/1, stop/1, list/0]).

-export([init/1]).

-include("emqttb.hrl").

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

list() ->
  [{Id, Pid} || {Id, Pid, _Type, _Modules} <- supervisor:which_children(?SERVER),
                is_pid(Pid)].

-spec ensure(emqttb_group:group_config()) -> ok.
ensure(Options = #{id := Id}) ->
  Result = supervisor:start_child(?SERVER,
                                  #{ id => Id
                                   , type => worker
                                   , restart => temporary
                                   , start => {emqttb_group, start_link, [Options]}
                                   , shutdown => timer:seconds(300)
                                   }),
  case Result of
    {ok, _} ->
      ok;
    {error, {already_started, _}} ->
      ok;
    {error, already_present} ->
      supervisor:restart_child(?SERVER, Id)
  end.

-spec stop(atom()) -> ok.
stop(ID) ->
  supervisor:terminate_child(?SERVER, ID).

init([]) ->
  SupFlags = #{ strategy => one_for_one
              , intensity => 0
              , period => 1
              },
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions

-module(emqttb_autorate_sup).

-behaviour(supervisor).

-export([start_link/0, ensure/1, list/0]).

-export([init/1]).

-include("emqttb.hrl").

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

list() ->
  [Id || {Id, _Child, _Type, _Modules} <- supervisor:which_children(?SERVER)].

-spec ensure(emqttb_autorate:config()) -> {ok, pid()}.
ensure(Options = #{id := Id}) ->
  Result = supervisor:start_child(?SERVER,
                                  #{ id => Id
                                   , type => worker
                                   , restart => temporary
                                   , start => {emqttb_autorate, start_link, [Options]}
                                   , shutdown => timer:seconds(1)
                                   }),
  case Result of
    {ok, _} = Ok ->
      Ok;
    {error, {already_started, Pid}} ->
      {ok, Pid};
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

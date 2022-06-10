-module(emqttb_autorate_sup).

-behaviour(supervisor).

-export([start_link/0, start_worker/1]).

-export([init/1]).

-include("emqttb.hrl").

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_worker(Options = #{id := Id}) ->
  {ok, _} = supervisor:start_child(?SERVER,
                                   #{ id => Id
                                    , type => worker
                                    , start => {emqttb_autorate, start_link, [Options]}
                                    , shutdown => timer:seconds(1)
                                    }).

init([]) ->
  SupFlags = #{ strategy => one_for_one
              , intensity => 1
              , period => 10
              },
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions

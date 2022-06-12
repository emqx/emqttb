%%%-------------------------------------------------------------------
%% @doc emqttb public API
%% @end
%%%-------------------------------------------------------------------

-module(emqttb_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("typerefl/include/types.hrl").
-include("emqttb.hrl").

start(_StartType, _StartArgs) ->
  Sup = emqttb_sup:start_link(),
  maybe_increase_fd_limit(),
  emqttb_conf:load_conf(),
  post_init(),
  Sup.

stop(_State) ->
  ok.

%% internal functions

%% Start misc. processes that depend on configuration
post_init() ->
  ?CFG([restapi, enabled]) andalso
    emqttb_misc_sup:start_worker( emqttb_http
                                , {emqttb_http, start_link, []}
                                ),
  ?CFG([metrics, pushgateway, enabled]) andalso
    emqttb_misc_sup:start_worker( emqttb_pushgw
                                , {emqttb_pushgw, start_link, []}
                                ),
  ok.

maybe_increase_fd_limit() ->
  case os:type() of
    {unix, linux} ->
      %% Best effort to increase the soft limit
      os:cmd("prlimit --nofile=$(ulimit -Hn): --pid " ++ os:getpid()),
      ok;
    _ ->
      ok
  end.

%%--------------------------------------------------------------------
%% Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(emqttb_group).

-behavior(gen_server).

%% API:
-export([ensure/1, set_target/3, set_target_async/3]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).

%% Internal exports
-export([start_link/1]).

-export_type([group_config/0]).

-include_lib("snabbkaffe/include/trace.hrl").
-include("emqttb_internal.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type group_config() ::
        #{ id            := atom()
         , client_config := atom()
         , behavior      := {module(), map()}
         }.

%%================================================================================
%% API funcions
%%================================================================================

-spec ensure(group_config()) -> ok.
ensure(Conf) ->
  emqttb_group_sup:ensure(Conf).

%% @doc Autoscale the group to the target number of workers. Returns
%% value when the target or a ratelimit has been reached, or error
%% when the new target has been set.
%%
%% The group will try to maintain the last target number of workers
%% even after set_target returns.
%%
%% Note: this implementation has been optimized for scaling up very
%% fast, not scaling down. Scaling down is rather memory-expensive.
%%
%% Order of workers' removal during ramping down is not specified.
-spec set_target(emqttb:group(), NClients, emqttb:interval()) ->
             {ok, NClients} | {error, new_target | {ratelimited, atom(), NClients}}
          when NClients :: emqttb:n_clients().
set_target(Id, Target, Interval) ->
  gen_server:call(Id, {set_target, Target, Interval}, infinity).


%% @doc Async version of `set_target'
-spec set_target_async(emqttb:group(), emqttb:n_clients(), emqttb:interval()) -> ok.
set_target_async(Id, Target, Interval) ->
  gen_server:cast(Id, {set_target, Target, Interval}).

%%================================================================================
%% behavior callbacks
%%================================================================================

%% Currently running scaling operation:
-record(r,
        { direction   :: up | down
        , on_complete :: fun((_Result) -> _)
        }).

-record(s,
        { id           :: atom()
        , behavior     :: module()
        , conf_prefix  :: lee:key()
        , scaling      :: #r{} | undefined
        , target       :: non_neg_integer() | undefined
        , interval     :: emqttb:interval() | undefined
        , scale_timer  :: reference() | undefined
        , tick_timer   :: reference()
        , next_id = 0  :: non_neg_integer()
        }).

-define(TICK_TIME, 1000).

init([Conf]) ->
  process_flag(trap_exit, true),
  #{ id := ID
   , client_config := ConfID
   , behavior := {Behavior, BehSettings}
   } = Conf,
  logger:info("Starting group ~p with client configuration ~p", [ID, ConfID]),
  persistent_term:put(?GROUP_LEADER_TO_GROUP_ID(self()), ID),
  persistent_term:put(?GROUP_BEHAVIOR(self()), Behavior),
  persistent_term:put(?GROUP_CONF_ID(self()), ConfID),
  BehSharedState = emqttb_worker:create_settings(Behavior, ID, BehSettings),
  persistent_term:put(?GROUP_BEHAVIOR_SHARED_STATE(self()), BehSharedState),
  emqttb_metrics:new_counter(?GROUP_N_WORKERS(ID),
                             [ {help, <<"Number of workers in the group">>}
                             , {labels, [group]}
                             ]),
  emqttb_metrics:new_counter(?GROUP_N_CONNECTIONS(ID),
                             [ {help, <<"Number of connections">>}
                             , {labels, [group]}
                             ]),
  S = #s{ id          = ID
        , behavior    = Behavior
        , conf_prefix = [groups, ConfID]
        , tick_timer  = set_tick_timer()
        },
  {ok, S}.

handle_call({set_target, Target, Interval}, From, S) ->
  OnComplete = fun(Result) -> gen_server:reply(From, Result) end,
  {noreply, do_set_target(Target, Interval, OnComplete, S)};
handle_call(_, _, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast({set_target, Target, Interval}, S) ->
  OnComplete = fun(Result) -> ok end,
  {noreply, do_set_target(Target, Interval, OnComplete, S)};
handle_cast(_, S) ->
  {noreply, S}.

handle_info(tick, S) ->
  {noreply, do_tick(S#s{tick_timer = set_tick_timer()})};
handle_info(do_scale, S)->
  {noreply, do_scale(S#s{scale_timer = undefined})};
handle_info(_, S) ->
  {noreply, S}.

terminate(_Reason, #s{id = Id}) ->
  _ = fold_workers(fun(Pid, Acc) ->
                       stop_worker(Id, Pid)
                   end,
                   [],
                   Id),
  ok.

%%================================================================================
%% Internal exports
%%================================================================================

-spec start_link(group_config()) -> {ok, pid()}.
start_link(Conf = #{id := ID}) ->
  gen_server:start_link({local, ID}, ?MODULE, [Conf], []).

%%================================================================================
%% Internal functions
%%================================================================================

do_set_target(Target, Interval, OnComplete, S = #s{scaling = Scaling}) ->
  N = n_clients(S),
  Direction = if Target > N   -> up;
                 Target =:= N -> stay;
                 true         -> down
              end,
  maybe_cancel_previous(Scaling),
  case Direction of
    stay ->
      OnComplete({ok, N}),
      S#s{scaling = undefined, target = Target, interval = Interval};
    _ ->
      start_scale(S, Direction, Target, Interval, OnComplete)
  end.

start_scale(S0, Direction, Target, Interval, OnComplete) ->
  Scaling = #r{ direction   = Direction
              , on_complete = OnComplete
              },
  S = S0#s{scaling = Scaling, target = Target, interval = Interval},
  logger:info("Group ~p is scaling ~p...", [S0#s.id, Direction]),
  set_scale_timer(S).

do_tick(S = #s{id = Id, target = Target}) ->
  N = n_clients(S),
  if N < Target -> %% Some clients died on us?
      logger:info("[~p]: ~p -> ~p", [Id, N, Target]),
      do_scale(S);
     true ->
      S
  end.

do_scale(S0) ->
  #s{ target    = Target
    , interval  = Interval
    , id        = ID
    } = S0,
  N = n_clients(S0),
  logger:debug("Scaling ~p; ~p -> ~p.", [ID, N, Target]),
  S = maybe_notify(N, S0),
  if N < Target ->
      S1 = set_scale_timer(S),
      scale_up(N, S1);
     N > Target ->
      S1 = set_scale_timer(S),
      scale_down(N, S1);
     true ->
      S
  end.

scale_up(N, S = #s{behavior = Behavior, id = Id, next_id = WorkerId}) ->
  _Pid = emqttb_worker:start(Behavior, self(), WorkerId),
  ?tp(start_worker, #{group => Id, pid => _Pid}),
  S#s{next_id = WorkerId + 1}.

scale_down(N, S0) ->
  S0.

set_tick_timer() ->
  erlang:send_after(?TICK_TIME, self(), tick).

set_scale_timer(S = #s{scale_timer = undefined, interval = Interval}) ->
  S#s{scale_timer = erlang:send_after(Interval, self(), do_scale)};
set_scale_timer(S) ->
  S.

maybe_notify(_, S = #s{scaling = undefined}) ->
  S;
maybe_notify(N, S) ->
  #s{ scaling   = #r{direction = Direction, on_complete = OnComplete}
    , target    = Target
    , id        = Id
    } = S,
  if Direction =:= up   andalso N >= Target;
     Direction =:= down andalso N =< Target ->
      logger:info("[~p]: Reached the target number of clients", [Id]),
      OnComplete({ok, N}),
      S#s{scaling = undefined};
     true ->
      S
  end.

maybe_cancel_previous(undefined) ->
  ok;
maybe_cancel_previous(#r{on_complete = OnComplete}) ->
  OnComplete({error, new_target}).

n_clients(#s{id = Id}) ->
  emqttb_metrics:get_counter(?GROUP_N_WORKERS(Id)).

stop_worker(GroupId, Pid) ->
  exit(Pid, shutdown).

-spec fold_workers( fun((pid(), Acc) -> Acc)
                  , Acc
                  , pid() | atom()
                  ) -> Acc.
fold_workers(Fun, Acc, GroupId) when is_atom(GroupId) ->
  GL = whereis(GroupId),
  is_pid(GL) orelse error({group_is_not_alive, GroupId}),
  fold_workers(Fun, Acc, GL);
fold_workers(Fun0, Acc, GL) ->
  PIDs = erlang:processes(),
  Fun = fun(Pid, Acc) ->
            case process_info(Pid, [group_leader]) of
              [{group_leader, GL}] -> Fun0(Pid, Acc);
              _                    -> Acc
            end
        end,
  lists:foldl(Fun, Acc, PIDs).

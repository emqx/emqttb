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
-export([ensure/1, ramp_up/3, ramp_down/3, foreach_children/2]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).

%% Internal exports
-export([start_link/1]).

-export_type([group_config/0]).

%%================================================================================
%% Type declarations
%%================================================================================

-type group_config() ::
        #{ id            := atom()
         , client_config := atom()
         , behavior      := module()
         }.

-record(r,
        { direction   :: up | down
        , target      :: non_neg_integer()
        , rate        :: emqttb:rate_key() | non_neg_integer()
        , on_complete :: fun(() -> _)
        }).

-record(s,
        { id            :: atom()
        , behavior      :: module()
        , pids          :: queue:queue(pid())
        , conf_prefix   :: lee:key()
        , scaling       :: #r{} | undefined
        , n_clients = 0 :: integer()
        }).

%%================================================================================
%% API funcions
%%================================================================================

-spec ensure(group_config()) -> ok.
ensure(Conf) ->
  emqttb_group_sup:ensure(Conf).

%% Autoscale the group up
-spec ramp_up(emqttb:group(), non_neg_integer(), emqttb:rate_key() | non_neg_integer()) -> ok.
ramp_up(Id, Target, Rate) ->
  gen_server:call(Id, {up, Target, Rate}, infinity).

%% Autoscale the group down
-spec ramp_down(emqttb:group(), non_neg_integer(), emqttb:rate_key() | non_neg_integer()) -> ok.
ramp_down(Id, Target, Rate) ->
  gen_server:call(Id, {down, Target, Rate}, infinity).

%%================================================================================
%% behavior callbacks
%%================================================================================

init([Conf]) ->
  process_flag(trap_exit, true),
  #{ id := ID
   , client_config := ConfID
   , behavior := Behavior
   } = Conf,
  ConfPrefix = case ConfID of
                 default -> [client];
                 _       -> [groups, {ConfID}]
               end,
  logger:info("Starting group ~p with client configuration ~p", [ID, ConfID]),
  S = #s{ id = ID
        , behavior = Behavior
        , pids = queue:new()
        , conf_prefix = ConfPrefix
        },
  {ok, S}.

handle_call({Direction, Target, Rate}, From, S0) when Direction =:= up;
                                                      Direction =:= down ->
  case S0#s.scaling of
    undefined ->
      S = start_scale( S0, Direction, Target, Rate
                     , fun() -> gen_server:reply(From, ok) end
                     ),
      {noreply, S};
    _ ->
      {reply, {error, already_scaling}, S0}
  end;
handle_call(_, _, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_, S) ->
  {noreply, S}.

handle_info(do_scale, S)->
  {noreply, do_scale(S)};
handle_info(_, S) ->
  {noreply, S}.

terminate(_Reason, State) ->
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

start_scale(S0, Direction, Target, Rate, OnComplete) ->
  Scaling = #r{ direction   = Direction
              , target      = Target
              , rate        = Rate
              , on_complete = OnComplete
              },
  self() ! do_scale,
  logger:info("Group ~p is scaling ~p...", [S0#s.id, Direction]),
  S0#s{scaling = Scaling}.

do_scale(S = #s{n_clients = N, scaling = Scaling}) ->
  logger:debug("Scaling ~p.", [S#s.id]),
  #r{ direction   = Direction
    , target      = Target
    , rate        = Rate
    , on_complete = OnComplete
    } = Scaling,
  if Direction =:= up   andalso N >= Target;
     Direction =:= down andalso N =< Target ->
      OnComplete(),
      S#s{scaling = undefined};
     true ->
      erlang:send_after(10, self(), do_scale),
      S
  end.

-spec foreach_children(fun((pid()) -> _), pid() | atom()) -> ok.
foreach_children(Fun, Id) when is_atom(Id) ->
  GL = whereis(Id),
  is_pid(GL) orelse throw({group_is_not_alive, Id}),
  foreach_children(Fun, GL);
foreach_children(Fun, GL) ->
  PIDs = erlang:processes(),
  Go = fun(Pid) ->
           case process_info(Pid, [group_leader]) of
             GL -> Fun(Pid);
             _  -> ok
           end
       end,
  lists:foreach(Go, PIDs).

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
-module(emqttb_autorate).

%% API:
-export([ensure/1, get_counter/1]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% internal exports:
-export([start_link/1]).

-include("emqttb_internal.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type config() ::
        #{ id     := atom()
         , parent => pid()
         , min    => integer()
         , max    => integer()
         , max_control => number()
         , k_p    => number()
         , t_i    => number()
         , t_d    => number()
         , error  => fun(() -> number())
         }.

-define(TICK_TIME, 100).

%%================================================================================
%% API funcions
%%================================================================================

-spec ensure(config()) -> {auto, counter:counters_ref()}.
ensure(Conf) ->
  {ok, Pid} = emqttb_autorate_sup:ensure(Conf#{parent => self()}),
  {auto, get_counter(Pid)}.

-spec get_counter(atom() | pid()) -> counters:counters_ref().
get_counter(Id) ->
  gen_server:call(Id, get_counter).


%%================================================================================
%% Internal exports
%%================================================================================

start_link(Conf = #{id := Id}) ->
  gen_server:start_link({local, Id}, ?MODULE, Conf, []).

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(s,
        { id       :: atom()
        , parent   :: reference() | undefined
        , current  :: float()
        , config   :: config()
        , last_t   :: integer()
        , integral :: number()
        , last_err :: number()
        }).

init(Config = #{id := Id}) ->
  Min = maps:get(min, Config, 0),
  Default = #{ k_p   => 1
             , t_i   => 1000000
             , t_d   => 0
             , error => fun() -> 0 end
             , min   => 0
             , max   => 10_000
             , max_control => 100
             },
  emqttb_metrics:new_gauge(?AUTORATE_RATE(Id),
                           [ {help, <<"Dynamic rate">>}
                           , {labels, [id]}
                           ]),
  [emqttb_metrics:new_gauge(?AUTORATE_CONTROL(Id, I),
                            [ {help, <<"Change of rate">>}
                            , {labels, [id, term]}
                            ]) || I <- [sum, p, i, d]],
  MRef = case Config of
           #{parent := Parent} -> monitor(process, Parent);
           _                   -> undefined
         end,
  Conf = maps:merge(Default, Config),
  set_timer(),
  {ok, #s{ id       = Id
         , parent   = MRef
         , config   = Conf
         , integral = 0
         , last_err = apply(maps:get(error, Conf), [])
         , current  = Min
         , last_t   = os:system_time(millisecond)
         }}.

handle_call(get_counter, _From, S) ->
  {reply, emqttb_metrics:gauge_ref(?AUTORATE_RATE(S#s.id)), S};
handle_call(_, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_, S) ->
  {noreply, S}.

handle_info(tick, S) ->
  set_timer(),
  {noreply, update_rate(S)};
handle_info({'DOWN', MRef, _, _, _}, S = #s{parent = MRef}) ->
  {stop, normal, S};
handle_info(_, S) ->
  {noreply, S}.

%%================================================================================
%% Internal functions
%%================================================================================

update_rate(S = #s{last_t = LastT, current = Curr0, integral = I0, last_err = LastErr, config = Conf, id = Id}) ->
  #{k_p := Kp, t_i := Ti, t_d := Td, error := ErrF, min := Min, max := Max, max_control := MaxControl} = Conf,
  T = os:system_time(millisecond),
  Dt = (T - LastT) / timer:seconds(1),
  Err = ErrF(),
  I1 = clamp(I0 + Dt * Err, MaxControl / Kp),
  D = (LastErr - Err) / Dt,
  CP = Err,
  CI = I1 / Ti,
  CD = Td * D,
  {Control, I} = case Kp * (CP + CI + CD) of
                    Ctr when Ctr > MaxControl ->
                      {MaxControl, I0};
                    Ctr when Ctr < -MaxControl ->
                      {-MaxControl, I0};
                    Ctr ->
                      {Ctr, I1}
                  end,
  Curr = case Curr0 + Control * Dt of
           C when C < Min ->
             Min;
           C when C > Max ->
             Max;
           C -> C
         end,
  emqttb_metrics:gauge_set(?AUTORATE_RATE(Id), round(Curr)),
  emqttb_metrics:gauge_set(?AUTORATE_CONTROL(Id, sum), round(Control / Kp)),
  emqttb_metrics:gauge_set(?AUTORATE_CONTROL(Id, p), round(CP)),
  emqttb_metrics:gauge_set(?AUTORATE_CONTROL(Id, d), round(CD)),
  emqttb_metrics:gauge_set(?AUTORATE_CONTROL(Id, i), round(CI)),
  S#s{last_t = T, current = Curr, integral = I, last_err = Err}.

set_timer() ->
  erlang:send_after(?TICK_TIME, self(), tick).

clamp(Val, Max) when Val > Max ->
  Max;
clamp(Val, Max) when Val < -Max ->
  -Max;
clamp(Val, _) ->
  Val.

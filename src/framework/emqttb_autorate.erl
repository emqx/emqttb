%%--------------------------------------------------------------------
%% Copyright (c) 2022-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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
%% This module uses velocity PI controller to automatically adjust
%% rate of something to minimize error function.
%%
%% See for the explanation of the theory:
%% https://controlguru.com/integral-reset-windup-jacketing-logic-and-the-velocity-pi-form/

%% API:
-export([ensure/1, get_counter/1, reset/2]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% internal exports:
-export([start_link/1, model/0]).

-export_type([config/0, scram_fun/0]).

-include("emqttb_internal.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%% Emergency override:
%%
%% Argument: boolean that returns whether the system is currently
%% overloaded (it may be used to introduce hysteresis)
%%
%% Return value: `{true, Value}' if the system is overloaded
%% and the autorate value should be overridden to `Value'.
%%
%% `false' if everything is normal.
-type scram_fun() :: fun((_IsOverride :: boolean()) -> {true, integer()} | false).

-type config() ::
        #{ id        := atom()
         , conf_root := atom()
         , error     := fun(() -> number())
         , scram     => scram_fun()
         , parent    => pid()
         , init_val  => integer()
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

%% Set the current value to the specified value
-spec reset(atom() | pid(), integer()) -> ok.
reset(Id, Val) ->
  gen_server:call(Id, {reset, Val}).

%%================================================================================
%% Internal exports
%%================================================================================

start_link(Conf = #{id := Id}) ->
  gen_server:start_link({local, Id}, ?MODULE, Conf, []).

model() ->
  #{ id =>
       {[value, cli_param],
        #{ type        => atom()
         , default     => default
         , cli_operand => "autorate"
         , cli_short   => $a
         }}
   , min               =>
       {[value, cli_param],
        #{ oneliner    => "Minimum value of the controlled parameter"
         , type        => integer()
         , default     => 0
         , cli_operand => "min"
         , cli_short   => $m
         }}
   , max =>
       {[value, cli_param],
        #{ oneliner    => "Maximum value of the controlled parameter"
         , type        => integer()
         , default     => 100_000_000 % 100s
         , cli_operand => "max"
         , cli_short   => $M
         }}
   , speed =>
       {[value, cli_param],
        #{ type        => integer()
         , default     => 0
         , cli_operand => "speed"
         , cli_short   => $V
         }}
   , k_p =>
       {[value, cli_param],
        #{ oneliner    => "Controller gain"
         , type        => number()
         , default     => 0.05
         , cli_operand => "Kp"
         , cli_short   => $p
         }}
   , t_i =>
       {[value, cli_param],
        #{ oneliner    => "Controller reset time"
         , type        => number()
         , default     => 1
         , cli_operand => "Ti"
         , cli_short   => $I
         }}
   , update_interval =>
       {[value, cli_param],
        #{ type        => emqttb:duration_ms()
         , default     => 100
         , cli_operand => "update-interval"
         , cli_short   => $u
         }}
   }.

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(s,
        { id        :: atom()
        , parent    :: reference() | undefined
        , current   :: float()
        , conf_root :: atom()
        , error     :: fun(() -> number())
        , scram_fun :: scram_fun()
        , meltdown  :: boolean()
        , last_t    :: integer()
        , last_err  :: number()
        }).

init(Config = #{id := Id, conf_root := ConfRoot, error := ErrF}) ->
  MRef = case Config of
           #{parent := Parent} -> monitor(process, Parent);
           _                   -> undefined
         end,
  %% Create metrics:
  emqttb_metrics:new_gauge(?AUTORATE_RATE(Id),
                           [ {help, <<"Controlled value">>}
                           , {labels, [id]}
                           ]),
  [emqttb_metrics:new_gauge(?AUTORATE_CONTROL(Id, I),
                            [ {help, <<"Control output delta">>}
                            , {labels, [id, term]}
                            ]) || I <- [sum, p, i]],
  %% Init PID:
  Min = my_cfg(ConfRoot, [min]),
  Current = maps:get(init_val, Config, Min),
  Err = ErrF(),
  ScramFun = maps:get(scram, Config, fun(_) -> false end),
  set_timer(ConfRoot),
  {ok, update_rate(#s{ id        = Id
                     , parent    = MRef
                     , current   = Current
                     , conf_root = ConfRoot
                     , error     = ErrF
                     , scram_fun = ScramFun
                     , meltdown  = false
                     , last_err  = Err
                     , last_t    = os:system_time(millisecond)
                     })}.

handle_call(get_counter, _From, S) ->
  {reply, emqttb_metrics:gauge_ref(?AUTORATE_RATE(S#s.id)), S};
handle_call({reset, Val}, _From, S) ->
  {reply, ok, update_rate(S#s{current = Val})};
handle_call(_, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_, S) ->
  {noreply, S}.

handle_info(tick, S) ->
  set_timer(S#s.conf_root),
  {noreply, update_rate(S)};
handle_info({'DOWN', MRef, _, _, _}, S = #s{parent = MRef}) ->
  {stop, normal, S};
handle_info(_, S) ->
  {noreply, S}.

%%================================================================================
%% Internal functions
%%================================================================================

update_rate(S = #s{ last_t    = LastT
                  , error     = ErrF
                  , current   = CO0
                  , last_err  = LastErr
                  , conf_root = ConfRoot
                  , id        = Id
                  , scram_fun = Scram
                  , meltdown  = Meltdown0
                  }) ->
  %% 0. Get current error
  T = os:system_time(millisecond),
  Dt = (T - LastT) / timer:seconds(1),
  Err = ErrF(),
  %% 1. Get config
  Min = my_cfg(ConfRoot, [min]),
  Max = my_cfg(ConfRoot, [max]),
  MaxSpeed = my_cfg(ConfRoot, [speed]),
  Kp = my_cfg(ConfRoot, [k_p]),
  Ti = my_cfg(ConfRoot, [t_i]),
  %% 2. Calculate the control output using velocity form:
  %%    Per Euler method CO = CO_0 + DeltaC * Dt = CO_0 + (CO + CI) * Dt =
  %%    = CO_0 + (Kp * De / Dt + Kp / Ti * Err) * Dt =
  %%    = CO_0 + Kp * De + Kp * Err / Ti * Dt
  De = LastErr - Err,
  CP = Kp * De, %% Note: delta t is reduced here, see above comment
  CI = Kp * Err / Ti * Dt,
  DeltaC = clamp(CP + CI, MaxSpeed * Dt),
  CO = clamp(CO0 + DeltaC, Min, Max),
  %% 3. Update metrics
  {Meltdown, Value} = case Scram(Meltdown0) of
                        false            -> {false, round(CO)};
                        {true, Override} -> {true, Override}
                      end,
  emqttb_metrics:gauge_set(?AUTORATE_RATE(Id), Value),
  %% Note: Kp tends to be small, and the below metrics are only used for grafana,
  %% so for aesthetic reasons we divide by Kp here:
  emqttb_metrics:gauge_set(?AUTORATE_CONTROL(Id, sum), round(DeltaC / Kp)),
  emqttb_metrics:gauge_set(?AUTORATE_CONTROL(Id, p), round(CP / Kp)),
  emqttb_metrics:gauge_set(?AUTORATE_CONTROL(Id, i), round(CI / Kp)),
  S#s{last_t = T, current = CO, last_err = Err, meltdown = Meltdown}.

set_timer(ConfRoot) ->
  TickTime = my_cfg(ConfRoot, [update_interval]),
  erlang:send_after(TickTime, self(), tick).

clamp(Val, Max) ->
  clamp(Val, -Max, Max).

clamp(Val, _Min, Max) when Val > Max ->
  Max;
clamp(Val, Min, _Max) when Val < Min ->
  Min;
clamp(Val, _, _) ->
  Val.

my_cfg(ConfRoot, Key) ->
  ?CFG([autorate, {ConfRoot} | Key]).

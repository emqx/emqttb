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

-behavior(gen_server).
-behavior(lee_metatype).
%% This module uses velocity PI controller to automatically adjust
%% rate of something to minimize error function.
%%
%% See for the explanation of the theory:
%% https://controlguru.com/integral-reset-windup-jacketing-logic-and-the-velocity-pi-form/

%% API:
-export([ensure/1, get_counter/1, reset/2, info/0, create_autorates/0]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% lee_metatype callbacks:
-export([names/1, metaparams/1, meta_validate_node/4, meta_validate/2, read_patch/2, validate_node/5]).

%% internal exports:
-export([start_link/1, model/0, from_model/1]).

-export_type([config/0, scram_fun/0, info/0]).

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
        #{ id        := emqttb:autorate()
         , conf_root := atom()
         , error     := fun(() -> number())
         , scram     => scram_fun()
         , parent    => pid()
         , init_val  => integer()
         }.

-type info() ::
        #{ '$id' := atom()
         , value := number()
         , error := number()
         , i := number()
         , p := number()
         , sum := number()
         }.

-define(id(ID), {n, l, {?MODULE, ID}}).
-define(via(ID), {via, gproc, ?id(ID)}).

%%================================================================================
%% API funcions
%%================================================================================

-spec ensure(config()) -> {auto, counter:counters_ref()}.
ensure(Conf) ->
  {ok, Pid} = emqttb_autorate_sup:ensure(Conf#{parent => self()}),
  {auto, get_counter(Pid)}.

-spec get_counter(lee:key() | emqttb:autorate() | pid()) -> counters:counters_ref().
get_counter(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, get_counter);
get_counter(Id) when is_atom(Id) ->
  gen_server:call(?via(Id), get_counter);
get_counter(Key) ->
  #mnode{metaparams = #{autorate_id := Id}} = lee_model:get(Key, ?MYMODEL),
  gen_server:call(?via(Id), get_counter).

%% Set the current value to the specified value
-spec reset(atom() | pid() | lee:ley(), integer()) -> ok.
reset(Key, Val) when is_list(Key) ->
  #mnode{metaparams = #{autorate_id := Id}} = lee_model:get(Key, ?MYMODEL),
  reset(Id, Val);
reset(Id, Val) ->
  gen_server:call(?via(Id), {reset, Val}).

-spec info() -> [info()].
info() ->
  [#{ '$id' => Id
    , value => counters:get(get_counter(Id), 1)
    , error => prometheus_gauge:value(?AUTORATE_CONTROL, [Id, err])
    , p => prometheus_gauge:value(?AUTORATE_CONTROL, [Id, p])
    , i => prometheus_gauge:value(?AUTORATE_CONTROL, [Id, i])
    , sum => prometheus_gauge:value(?AUTORATE_CONTROL, [Id, sum])
    } || Id <- emqttb_autorate_sup:list()].

%%================================================================================
%% Internal exports
%%================================================================================

start_link(Conf = #{id := Id}) ->
  gen_server:start_link(?via(Id), ?MODULE, Conf, []).

model() ->
  #{ id =>
       {[value, cli_param, autorate_id],
        #{ type        => atom()
         , default     => default
         , cli_operand => "autorate"
         , cli_short   => $a
         }}
   , set_point         =>
       {[value, cli_param],
        #{ oneliner    => "Value that the autorate will try to approach"
         , type        => number()
         , default     => 0
         , cli_operand => "setpoint"
         , cli_short   => $s
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
%% lee_metatype callbacks and helpers
%%================================================================================

names(_) ->
  [autorate, autorate_id].

metaparams(autorate) ->
  %% TBD: make it possible to configure alternative targets.
  [ {mandatory, autorate_id, atom()}
  , {mandatory, process_variable, lee:model_key()}
  , {optional, error_coeff, number()}
  ];
metaparams(autorate_id) ->
  [].


meta_validate_node(autorate, Model, _Key, #mnode{metaparams = #{process_variable := ProcVarKey}}) ->
  Error = {["process_variable parameter must point at a node of `metric' metatype"], []},
  try lee_model:get(ProcVarKey, Model) of
    #mnode{metatypes = MTs} ->
      case lists:member(metric, MTs) of
        true  -> {[], []};
        false -> Error
      end
  catch
    _:_ -> Error
  end;
meta_validate_node(autorate_id, _, _, _) ->
  {[], []}.

meta_validate(autorate, Model) ->
  Ids = [begin
           #mnode{metaparams = #{autorate_id := Id}} = lee_model:get(Key, Model),
           Id
         end || Key <- lee_model:get_metatype_index(autorate, Model)],
  case length(lists:usort(Ids)) =:= length(Ids) of
    false -> {["Autorate IDs must be unique"], [], []};
    true  -> {[], [], [{set, autorate_ids, Ids}]}
  end;
meta_validate(autorate_id, _) ->
  {[], [], []}.


-spec validate_node(lee:metatype(), lee:model(), _Staging :: lee:data(), lee:key(), #mnode{}) ->
    lee_lib:check_result().
validate_node(autorate_id, Model, Data, Key, _) ->
  Id = lee:get(Model, Data, Key),
  {ok, Ids} = lee_model:get_meta(autorate_ids, Model),
  case lists:member(Id, Ids) of
    true  -> {[], []};
    false -> {["Unknown autorate " ++ atom_to_list(Id)], []}
  end;
validate_node(autorate, _, _, _, _) ->
  {[], []}.

read_patch(autorate, Model) ->
  %% Create default instances:
  Prio = -99999,
  {ok, Prio,
   lists:flatmap(
     fun(Key) ->
         #mnode{metaparams = MPs} = lee_model:get(Key, ?MYMODEL),
         Id = ?m_attr(autorate, autorate_id, MPs),
         lee_lib:make_nested_patch(Model, [autorate],
                                   #{ [id] => Id
                                    })
     end,
     lee_model:get_metatype_index(autorate, ?MYMODEL))};
read_patch(autorate_id, _) ->
  {ok, 0, []}.

create_autorates() ->
  [begin
     #mnode{metaparams = MPs} = lee_model:get(Key, ?MYMODEL),
     Id = ?m_attr(autorate, autorate_id, MPs),
     {ok, _} = emqttb_autorate_sup:ensure(#{ id        => Id
                                           , error     => make_error_fun(Key)
                                           , init_val  => ?CFG(Key)
                                           , conf_root => Id
                                           })
   end || Key <- lee_model:get_metatype_index(autorate, ?MYMODEL)],
  ok.

-spec from_model(lee:key()) -> atom().
from_model(Key) ->
  #mnode{metaparams = #{autorate_id := Id}} = lee_model:get(Key, ?MYMODEL),
  Id.

%%================================================================================
%% gen_server callbacks
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
  logger:info("Starting autorate ~p", [Id]),
  MRef = case Config of
           #{parent := Parent} -> monitor(process, Parent);
           _                   -> undefined
         end,
  %% Create metrics:
  emqttb_metrics:new_gauge(?AUTORATE_RATE(Id),
                           [ {help, <<"Controlled value">>}
                           , {labels, [id]}
                           ]),
  prometheus_gauge:declare([ {name, ?AUTORATE_CONTROL}
                           , {labels, [id, term]}
                           , {help, <<"Control output delta">>}
                           ]),
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
  prometheus_gauge:set(?AUTORATE_CONTROL, [Id, sum], DeltaC),
  prometheus_gauge:set(?AUTORATE_CONTROL, [Id, p], CP),
  prometheus_gauge:set(?AUTORATE_CONTROL, [Id, i], CI),
  prometheus_gauge:set(?AUTORATE_CONTROL, [Id, err], Err),
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

-spec make_error_fun(lee:key()) -> fun(() -> number()).
make_error_fun(Key) ->
  ModelKey = lee_model:get_model_key(Key),
  #mnode{metaparams = MP} = lee_model:get(ModelKey, ?MYMODEL),
  ProcessVarKey = ?m_attr(autorate, process_variable, MP),
  Id = ?m_attr(autorate, autorate_id, MP),
  Coeff = ?m_attr(autorate, error_coeff, MP, 1),
  MetricKey = emqttb_metrics:from_model(ProcessVarKey),
  %%
  #mnode{metaparams = PVarMPs} = lee_model:get(ProcessVarKey, ?MYMODEL),
  case ?m_attr(metric, metric_type, PVarMPs) of
    counter ->
      fun() ->
          SetPoint = my_cfg(Id, [set_point]),
          Coeff * (SetPoint - emqttb_metrics:get_counter(MetricKey))
      end;
    rolling_average ->
      fun() ->
          AvgWindow = 250,
          SetPoint = my_cfg(Id, [set_point]),
          Coeff * (SetPoint - emqttb_metrics:get_rolling_average(MetricKey, AvgWindow))
      end
  end.

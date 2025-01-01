%%--------------------------------------------------------------------
%% Copyright (c) 2022-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-export([ls/1, ensure/1, get_counter/1, reset/2, info/0, create_autorates/0, activate/1, deactivate/1]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% lee_metatype callbacks:
-export([names/1, metaparams/1, meta_validate/2, validate_node/5, description/3, doc_refer/4]).

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
         , error     => fun(() -> number())
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

-spec ls(lee:model()) -> [{emqttb:autorate(), lee:model_key()}].
ls(Model) ->
  [begin
     #mnode{metaparams = #{autorate_id := Id}} = lee_model:get(Key, Model),
     {Id, Key}
   end || Key <- lee_model:get_metatype_index(autorate, Model)].

-spec ensure(config()) -> {auto, counters:counters_ref()}.
ensure(Conf) ->
  {ok, Pid} = emqttb_autorate_sup:ensure(Conf#{parent => self()}),
  {auto, get_counter(Pid)}.

-spec get_counter(lee:key() | emqttb:autorate() | pid()) -> counters:counters_ref().
get_counter(Autorate) ->
  gen_server:call(server(Autorate), get_counter).

%% Set the current value to the specified value
-spec reset(atom() | pid() | lee:key(), integer()) -> ok.
reset(Autorate, Val) ->
  gen_server:call(server(Autorate), {reset, Val}).

%% Freeze the value of autorate
deactivate(Autorate) ->
  gen_server:call(server(Autorate), deactivate).

%% Thaw the value of autorate
activate(Autorate) ->
  gen_server:call(server(Autorate), activate).

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
        #{ oneliner    => "ID of the autorate configuration"
         , doc         => "Autorate identifier.
                           This value must be equal to one of the elements returned by @code{emqttb @@ls autorate} command.
                           Full list is also available in FIXME <<autorate>>
                           "
         , type        => atom()
         , default     => default
         , cli_operand => "autorate"
         , cli_short   => $a
         }}
   , process_variable  =>
       {[value, cli_param, metric_id],
        #{ oneliner    => "Process variable"
         , doc         => "This parameter specifies ID of the metric that senses pressure on the SUT and serves as the process variable (PV).
                           Its value must be equal to one of the metric IDs returned by @code{emqttb @@ls metric} command.

                           Full list can be also found in FIXME <<metrics>>.
                           "
         , type        => lee:model_key()
         , cli_operand => "pvar"
         }}
   , error_coeff       =>
       {[value, cli_param],
        #{ oneliner    => "Multiply error by this coefficient"
         , type        => number()
         , default     => -1
         , cli_operand => "error-coeff"
         }}
   , set_point         =>
       {[value, cli_param],
        #{ oneliner    => "Set point"
         , doc         => "The desired value of the process variable (PV) is called the setpoint.
                           Autorate adjusts the value of the control variable (CV) to bring the PV close to the setpoint.
                           "
         , type        => number()
         , default     => 0
         , cli_operand => "setpoint"
         , cli_short   => $s
         }}
   , min               =>
       {[value, cli_param],
        #{ oneliner    => "Minimum value of the controlled parameter"
         , type        => number()
         , default     => 0
         , cli_operand => "min"
         , cli_short   => $m
         }}
   , max =>
       {[value, cli_param],
        #{ oneliner    => "Maximum value of the controlled parameter"
         , type        => number()
         , default     => 100_000_000 % 100s
         , cli_operand => "max"
         , cli_short   => $M
         }}
   , speed =>
       {[value, cli_param],
        #{ oneliner    => "Speed"
         , doc         => "Maximum rate of change of the controlled parameter.

                           Note: by default this parameter is set to 0 for each autorate, effectively locking the control parameter in place."
         , type        => non_neg_integer()
         , default     => 0
         , cli_operand => "speed"
         , cli_short   => $V
         }}
   , k_p =>
       {[value, cli_param],
        #{ oneliner    => "Controller gain, @math{k_p}"
         , type        => number()
         , default     => 0.05
         , cli_operand => "Kp"
         , cli_short   => $p
         }}
   , t_i =>
       {[value, cli_param],
        #{ oneliner    => "Controller reset time, @math{t_i}"
         , type        => number()
         , default     => 1
         , cli_operand => "Ti"
         , cli_short   => $I
         }}
   , update_interval =>
       {[value, cli_param],
        #{ oneliner    => "Autorate update interval"
         , doc         => "This parameter governs how often error is calculated and control parameter is updated.\n"
         , type        => emqttb:duration_ms()
         , default     => 100
         , cli_operand => "update-interval"
         , cli_short   => $u
         }}
   , scram =>
       #{ enabled =>
            {[value, cli_param],
             #{ oneliner    => "Enable SCRAM"
              , doc         => "@doc-scram"
              , type        => boolean()
              , default     => false
              , cli_operand => "olp"
              }}
        , threshold =>
            {[value, cli_param],
             #{ type        => non_neg_integer()
              , default     => 1000
              , cli_operand => "olp-threshold"
              }}
        , hysteresis =>
            {[value, cli_param],
             #{ oneliner    => "SCRAM hysteresis"
              , doc         => "Hysteresis is defined as percent of the threshold.

                                It is used to prevent frequent switching between normal and SCRAM modes of operation.
                                "
              , type        => typerefl:range(1, 100)
              , default     => 50
              , cli_operand => "olp-hysteresis"
              }}
        , override =>
            {[value, cli_param],
             #{ oneliner    => "SCRAM rate override"
              , doc         => "Replace configured (or calculated via autorate) value of the control variable with this value
                                when the system under test is not keeping up with the load.
                                "
              , type        => emqttb:duration_us()
              , default_str => "10s"
              , cli_operand => "olp-override"
              }}
        }
   }.

%%================================================================================
%% lee_metatype callbacks and helpers
%%================================================================================

names(_) ->
  [autorate, autorate_id].

metaparams(autorate) ->
  [{mandatory, autorate_id, atom()}];
metaparams(autorate_id) ->
  [].

meta_validate(autorate, Model) ->
  {Ids, _} = lists:unzip(ls(Model)),
  case length(lists:usort(Ids)) =:= length(Ids) of
    false -> {["Autorate IDs must be unique"], [], []};
    true  -> {[], [], []}
  end;
meta_validate(autorate_id, _) ->
  {[], [], []}.


-spec validate_node(lee:metatype(), lee:model(), _Staging :: lee:data(), lee:key(), #mnode{}) ->
    lee_lib:check_result().
validate_node(autorate_id, Model, Data, Key, _) ->
  %% Check that ID matches with some of the autorates in the model:
  Id = lee:get(Model, Data, Key),
  {Ids, _} = lists:unzip(ls(Model)),
  case lists:member(Id, Ids) of
    true  -> {[], []};
    false -> {["Unknown autorate " ++ atom_to_list(Id)], []}
  end;
validate_node(autorate, Model, Data, _Key, #mnode{metaparams = #{autorate_id := Id}}) ->
  %% Check that the configuration is present:
  case lee:list(Model, Data, [autorate, {Id}]) of
    [_] ->
      {[], []};
    [] ->
      {[lee_lib:format("Configuration for autorate ~p is missing", [Id])], []}
  end.

create_autorates() ->
  [begin
     {ok, _} = emqttb_autorate_sup:ensure(#{ id        => Id
                                           , init_val  => ?CFG(Key)
                                           , conf_root => Id
                                           })
   end || {Id, Key} <- ls(?MYMODEL)],
  ok.

-spec from_model(lee:key()) -> atom().
from_model(Key) ->
  #mnode{metaparams = #{autorate_id := Id}} = lee_model:get(Key, ?MYMODEL),
  Id.

description(autorate = MT, Model, Options) ->
  Content = [begin
               #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
               Title = atom_to_list(?m_attr(autorate, autorate_id, Attrs)),
               [ #doclet{mt = autorate, tag = autorate, key = Key, data = Title}
               , lee_doc:get_oneliner(autorate, Model, Key)
               , lee_metatype:doc_refer(value, Model, Options, Key)
               ]
             end || Key <- lee_model:get_metatype_index(MT, Model)
            ],
  [#doclet{mt = autorate, tag = container, data = Content}];
description(_, _, _) ->
  [].

doc_refer(autorate, _Model, _Options, Key) ->
  [#doclet{mt = autorate, tag = see_also, data = #doc_xref{mt = autorate, key = Key}}];
doc_refer(_, _Model, _Options, _Key) ->
  [].

%%================================================================================
%% gen_server callbacks
%%================================================================================

-record(s,
        { id        :: atom()
        , parent    :: reference() | undefined
        , active    :: boolean()
        , current   :: float()
        , conf_root :: atom()
        , error     :: fun(() -> number())
        , scram_fun :: scram_fun()
        , meltdown  :: boolean()
        , last_t    :: integer()
        , last_err  :: number()
        , init_val  :: number()
        }).

init(Config = #{id := Id, conf_root := ConfRoot}) ->
  logger:info("Starting autorate ~p", [Id]),
  case Config of
    #{error := ErrF} ->
      ok;
    _ ->
      ErrF = make_error_fun(Id, ConfRoot)
  end,
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
  set_timer(ConfRoot),
  {ok, update_rate(#s{ id        = Id
                     , active    = true
                     , parent    = MRef
                     , current   = Current
                     , conf_root = ConfRoot
                     , error     = ErrF
                     , scram_fun = make_scram_fun(Id, ConfRoot)
                     , meltdown  = false
                     , last_err  = Err
                     , last_t    = os:system_time(millisecond)
                     , init_val  = Current
                     })}.

handle_call(activate, _From, S) ->
  {reply, ok, S#s{active = true}};
handle_call(deactivate, _From, S) ->
  {reply, ok, S#s{active = false}};
handle_call(get_counter, _From, S) ->
  {reply, emqttb_metrics:gauge_ref(?AUTORATE_RATE(S#s.id)), S};
handle_call({reset, Val}, _From, S) ->
  {reply, ok, update_rate(S#s{current = Val})};
handle_call(_, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_, S) ->
  {noreply, S}.

handle_info(tick, S = #s{active = Active}) ->
  set_timer(S#s.conf_root),
  if Active -> {noreply, update_rate(S)};
     true   -> {noreply, S}
  end;
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
                  , init_val  = InitVal
                  }) ->
  %% 0. Get current error
  T = os:system_time(millisecond),
  Dt = (T - LastT) / timer:seconds(1),
  Err = ErrF(),
  %% 1. Get config
  %% Make sure the initial value is always in the min/max range:
  Min = min(InitVal, my_cfg(ConfRoot, [min])),
  Max = max(InitVal, my_cfg(ConfRoot, [max])),
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

-spec make_error_fun(emqttb:autorate(), lee:key()) -> fun(() -> number()).
make_error_fun(Id, ConfRoot) ->
  fun() ->
      SetPoint = my_cfg(Id, [set_point]),
      Coeff = my_cfg(ConfRoot, [error_coeff]),
      ProcessVar = read_pvar(ConfRoot),
      %% logger:error(#{autorate => Id, pvar => ProcessVar, setpoint => SetPoint, pvar_key => ProcessVarKey}),
      Coeff * (SetPoint - ProcessVar)
  end.

make_scram_fun(Id, ConfRoot) ->
  fun(Meltdown) ->
      Enabled = my_cfg(ConfRoot, [scram, enabled]),
      Threshold = my_cfg(ConfRoot, [scram, threshold]),
      Hysteresis = my_cfg(ConfRoot, [scram, hysteresis]),
      Override = my_cfg(ConfRoot, [scram, override]),
      PVar = read_pvar(ConfRoot),
      if not Enabled ->
          false;
         Meltdown andalso PVar >= (Threshold * Hysteresis / 100) ->
          {true, Override};
         PVar >= Threshold ->
          logger:warning("SCRAM is activated for autorate ~p. PV=~p. CV=~p.",
                         [Id, PVar, Override]),
          {true, Override};
         Meltdown ->
          logger:warning("SCRAM is deactivated for autorate ~p. PV=~p.",
                         [Id, PVar]),
          false;
         true ->
          false
      end
  end.

read_pvar(ConfRoot) ->
  ProcessVarKey = my_cfg(ConfRoot, [process_variable]),
  #mnode{metaparams = PVarMPs} = lee_model:get(ProcessVarKey, ?MYMODEL),
  MetricKey = emqttb_metrics:from_model(ProcessVarKey),
  case ?m_attr(metric, metric_type, PVarMPs) of
    counter ->
      emqttb_metrics:get_counter(MetricKey);
    rolling_average ->
      AvgWindow = 5000,
      emqttb_metrics:get_rolling_average(MetricKey, AvgWindow)
  end.

server(Pid) when is_pid(Pid) ->
  Pid;
server(Id) when is_atom(Id) ->
  ?via(Id);
server(Key) when is_list(Key) ->
  #mnode{metaparams = #{autorate_id := Id}} = lee_model:get(Key, ?MYMODEL),
  ?via(Id).

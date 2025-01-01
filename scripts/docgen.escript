#!/usr/bin/escript
%%! -pa _build/default/lib/lee/ebin -pa _build/default/lib/typerefl/ebin  -pa _build/default/lib/emqttb/ebin -pa ./_build/default/checkouts/lee/ebin
%% -*- erlang -*-

main(OutDir) ->
  ExtractorConfig = #{ output_dir => OutDir
                     , extension => ".texi"
                     , formatter => fun lee_doc:texinfo/3
                     , metatypes => [cli_param, value, os_env]
                     },
  ok  = emqttb_conf:load_model(),
  [_|_] = lee_doc:make_docs(emqttb_conf:model(), ExtractorConfig).

%% main(OutFile) ->
%%   AsciidocOptions = #{},
%%   ok = filelib:ensure_dir(OutFile),
%%   io:format(user, "Enriching model...~n", []),
%%   RawModel = lee_asciidoc:enrich_model(AsciidocOptions, emqttb_conf_model:model()),
%%   io:format(user, "Compiling model...~n", []),
%%   {ok, Model} = emqttb_conf:compile_model(RawModel),
%%   io:format(user, "Dumping XML...~n", []),
%%   ok = lee_doc:make_docs(Model, #{output_file => OutFile}),
%%   io:format(user, "Done, exiting.~n", []).

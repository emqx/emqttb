#!/usr/bin/escript
%%! -pa _build/default/lib/lee/ebin -pa _build/default/lib/typerefl/ebin  -pa _build/default/lib/emqttb/ebin
%% -*- erlang -*-
main(OutFile) ->
  AsciidocOptions = #{},
  ok = filelib:ensure_dir(OutFile),
  io:format(user, "Enriching model...~n", []),
  RawModel = lee_asciidoc:enrich_model(AsciidocOptions, emqttb_conf_model:model()),
  io:format(user, "Compiling model...~n", []),
  {ok, Model} = emqttb_conf:compile_model(RawModel),
  io:format(user, "Dumping XML...~n", []),
  ok = lee_doc:make_docs(Model, #{output_file => OutFile}),
  io:format(user, "Done, exiting.~n", []).

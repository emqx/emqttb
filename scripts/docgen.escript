#!/usr/bin/escript
%%! -pa _build/default/lib/lee/ebin -pa _build/default/lib/typerefl/ebin  -pa _build/default/lib/emqttb/ebin
%% -*- erlang -*-

-include_lib("lee/include/lee.hrl").

main(OutDir) ->
  ExtractorConfig = #{ output_dir => OutDir
                     , extension => ".texi"
                     , formatter => fun texinfo/3
                     , metatypes => [cli_param, value, os_env, autorate, metric]
                     },
  ok  = emqttb_conf:load_model(),
  [_|_] = lee_doc:make_docs(emqttb_conf:model(), ExtractorConfig).

texinfo(Options, FD, L) when is_list(L) ->
  [texinfo(Options, FD, I) || I <- L],
  ok;
texinfo(Options, FD, Doclet) ->
  P = fun(L) -> io:put_chars(FD, L) end,
  case Doclet of
    %% Autorate
    #doclet{mt = autorate, tag = container, data = Children} ->
      P(["@table @code\n"]),
      texinfo(Options, FD, Children),
      P(["@end table\n"]);
    #doclet{mt = autorate, tag = autorate, key = Key, data = Title} ->
      P(["@anchor{", lee_doc:texi_key([autorate | Key]), "}\n"]),
      P(["@item ", Title, $\n]);
    %% Metric
    #doclet{mt = metric, tag = container, data = Children} ->
      P(["@itemize\n"]),
      texinfo(Options, FD, Children),
      P(["@end itemize\n"]);
    #doclet{mt = metric, tag = metric, key = Key} ->
      P(["@item\n@anchor{", lee_doc:texi_key([metric | Key]), "}\n"]),
      P(["@verbatim\n", lee_lib:term_to_string(Key), "\n@end verbatim\n"]);
    #doclet{mt = metric, tag = type, data = Data} ->
      P(["@b{Type}: ", Data, "\n\n"]);
    #doclet{mt = metric, tag = labels, data = Data} ->
      P("@b{Prometheus labels}: "),
      P(lists:join(", ", Data)),
      P("\n\n");
    #doclet{mt = metric, tag = prometheus_id, data = Data} ->
      P(["@b{Prometheus name}: @code{", Data, "}\n\n"]);
    _ ->
      lee_doc:texinfo(Options, FD, Doclet)
  end.

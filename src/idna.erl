-module(idna).

-export([start/0, test/0, test/1, to_ascii/1, utf8_to_ascii/1]).

-define(ACE_PREFIX, "xn--").

%%============================================================================
%% API
%%============================================================================

start() ->
  idna_unicode_data:start(),
  File = code:priv_dir('erlang') ++ "/UnicodeData.txt",
  case file:read_file(File) of
    {'ok', Data} -> idna_unicode_data:load(Data);
	{'error', _} ->
      {ok, {{_, 200, _}, _, GetData}} = http:request(get, {"http://www.unicode.org/Public/UNIDATA/UnicodeData.txt", []}, [], [{body_format, binary}]),
	  file:write_file(File, GetData),
	  idna_unicode_data:load(GetData)
  end.

to_ascii(Domain) ->
  to_ascii(string:tokens(idna_unicode:downcase(Domain), "."), []).

utf8_to_ascii(Domain) ->
  to_ascii(xmerl_ucs:from_utf8(Domain)).

%%============================================================================
%% Helper functions
%%============================================================================

to_ascii([], Acc) ->
  lists:reverse(Acc);
to_ascii([Label|Labels], []) ->
  to_ascii(Labels, lists:reverse(label_to_ascii(Label)));
to_ascii([Label|Labels], Acc) ->
  to_ascii(Labels, lists:reverse(label_to_ascii(Label), [$.|Acc])).

label_to_ascii(Label) ->
  case lists:all(fun(C) -> xmerl_ucs:is_ascii(C) end, Label) of
    true ->
      Label;
    false ->
      ?ACE_PREFIX ++ punycode:encode(idna_unicode:normalize_kc(Label))
  end.

%%============================================================================
%% Test functions
%%============================================================================

test() ->
  lists:foreach(fun(Path) -> test(Path) end, filelib:wildcard("test/idna_*")).

test(Path) ->
  {ok, Test} = file:consult(Path),
  Input = xmerl_ucs:from_utf8(proplists:get_value(input, Test)),
  Output = proplists:get_value(output, Test),
  case to_ascii(Input) of
    Output ->
      ok;
    ReturnValue ->
      erlang:error({test_failed, to_ascii, Path, ReturnValue})
  end.

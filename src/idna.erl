-module(idna).

-export([start/0, test/0, test/1, to_ascii/1, utf8_to_ascii/1]).

-define(ACE_PREFIX, "xn--").

%%============================================================================
%% API
%%============================================================================

start() ->
  idna_unicode_data:start(),
  idna_unicode_data:load("http://www.unicode.org/Public/UNIDATA/UnicodeData.txt").

to_ascii(Domain) ->
  to_ascii(string:tokens(idna_unicode:downcase(Domain), "."), []).

utf8_to_ascii(Domain) ->
  to_ascii(xmerl_ucs:from_utf8(Domain)).

%%============================================================================
%% Helper functions
%%============================================================================

to_ascii([], Acc) ->
  lists:reverse(Acc);
to_ascii([Part|Parts], []) ->
  to_ascii(Parts, lists:reverse(part_to_ascii(Part)));
to_ascii([Part|Parts], Acc) ->
  to_ascii(Parts, lists:reverse(part_to_ascii(Part), [$.|Acc])).

part_to_ascii(Part) ->
  case lists:all(fun(C) -> xmerl_ucs:is_ascii(C) end, Part) of
    true ->
      Part;
    false ->
      ?ACE_PREFIX ++ punycode:encode(idna_unicode:normalize_kc(Part))
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

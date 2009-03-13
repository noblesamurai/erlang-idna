-module(punycode).

-export([encode/1, test/0, test/1]).

%%============================================================================
%% Constants
%%============================================================================

-define(BASE, 36).

-define(TMIN, 1).

-define(TMAX, 26).

-define(SKEW, 38).

-define(DAMP, 700).

-define(INITIAL_BIAS, 72).

-define(INITIAL_N, 128).

-define(DELIMITER, $-).

%%============================================================================
%% Encoding algorithm state
%%============================================================================

-record(encode, {n=?INITIAL_N, delta=0, bias=?INITIAL_BIAS, h, b}).

%%============================================================================
%% API
%%============================================================================

encode(Input) ->
  encode(Input, lists:reverse(lists:filter(fun(C) -> C < 16#80 end, Input))).

%%============================================================================
%% Helper functions
%%============================================================================

encode(Input, Basic) ->
  case length(Basic) of
    0 -> encode_whileloop(Input, [], #encode{h=0, b=0});
    N -> encode_whileloop(Input, [?DELIMITER|Basic], #encode{h=N, b=N})
  end.

encode_whileloop(Input, Output, State=#encode{h=H}) when H < length(Input) ->
  N = State#encode.n,
  M = lists:min(lists:filter(fun(C) -> C >= N end, Input)),
  Delta = State#encode.delta + (M - N) * (H + 1),
  {Output2, State2=#encode{delta=Delta2, n=N2}} = encode_foreachloop(Input, Output, State#encode{delta=Delta, n=M}),
  encode_whileloop(Input, Output2, State2#encode{delta=Delta2 + 1, n=N2 + 1});
encode_whileloop(_, Output, _) ->
  lists:reverse(Output).

encode_foreachloop([], Output, State) ->
  {Output, State};
encode_foreachloop([C|Input], Output, State=#encode{n=N, delta=Delta}) when C < N ->
  encode_foreachloop(Input, Output, State#encode{delta=Delta + 1});
encode_foreachloop([C|Input], Output, State=#encode{n=N, delta=Delta, h=H, b=B, bias=Bias}) when C =:= N ->
  {Output2, Q} = encode_forloop(Output, ?BASE, Delta, Bias),
  Bias2 = adapt(Delta, H + 1, H =:= B),
  encode_foreachloop(Input, [encode_digit(Q)|Output2], State#encode{delta=0, h=H + 1, bias=Bias2});
encode_foreachloop([_|Input], Output, State) ->
  encode_foreachloop(Input, Output, State).

encode_forloop(Output, K, Q, Bias) ->
  T = case K =< Bias of
    true ->
      ?TMIN;
    false ->
      case K >= (Bias + ?TMAX) of true -> ?TMAX; false -> (K - Bias) end
  end,
  case Q < T of
    true ->
      {Output, Q};
    false ->
      Digit = encode_digit(T + ((Q - T) rem (?BASE - T))),
      encode_forloop([Digit|Output], K + ?BASE, (Q - T) div (?BASE - T), Bias)
  end.

encode_digit(N) when N < 26 ->
  N + 22 + 75;
encode_digit(N) ->
  N + 22.

adapt(Delta, Numpoints, Firsttime) ->
  Delta2 = case Firsttime of true -> Delta div ?DAMP; false -> Delta bsr 1 end,
  adapt_whileloop(Delta2 + (Delta2 div Numpoints), 0).

adapt_whileloop(Delta, K) ->
  case Delta > (((?BASE - ?TMIN) * ?TMAX) bsr 1) of
    true ->
      adapt_whileloop(Delta div (?BASE - ?TMIN), K + ?BASE);
    false ->
      K + (((?BASE - ?TMIN + 1) * Delta) div (Delta + ?SKEW))
  end.

%%============================================================================
%% Test functions
%%============================================================================

test() ->
  lists:foreach(fun(Path) -> test(Path) end, filelib:wildcard("test/punycode_*")).

test(Path) ->
  {ok, Test} = file:consult(Path),
  Encoded = encode(proplists:get_value(unicode, Test)),
  Punycode = proplists:get_value(punycode, Test),
  case Encoded of
    Punycode ->
      ok;
    ReturnValue ->
      erlang:error({test_failed, encode, Path, ReturnValue})
  end.

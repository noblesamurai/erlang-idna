-module(idna_unicode_data).

-behaviour(gen_server).

-export([combining_class/1, compat/1, composition/2, load/1, lowercase/1, start/0, start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%============================================================================
%% Constants
%%============================================================================

-define(SERVER, ?MODULE).

-define(COMBINING_CLASS, 4).

-define(DECOMPOSITION, 6).

-define(LOWERCASE_MAPPING, 14).

%%============================================================================
%% API
%%============================================================================

combining_class(C) ->
  gen_server:call(?SERVER, {combining_class, C}).

compat(C) ->
  gen_server:call(?SERVER, {compat, C}).

composition(A, B) ->
  gen_server:call(?SERVER, {composition, A, B}).

load(Data) ->
  gen_server:call(?SERVER, {load, Data}, 10000).

lowercase(C) ->
  gen_server:call(?SERVER, {lowercase, C}).

start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, noargs, []).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, noargs, []).

%%============================================================================
%% gen_server callbacks
%%============================================================================

init(noargs) ->
  {ok, undefined}.

handle_call({combining_class, C}, _From, Data) ->
  case lookup(C, Data) of
    {value, Props} ->
      {reply, erlang:list_to_integer(element(?COMBINING_CLASS, Props)), Data};
    false ->
      {reply, 0, Data}
  end;
handle_call({compat, C}, _From, Data) ->
  lookup(C, Data, fun(Props) ->
    case element(?DECOMPOSITION, Props) of
      [] ->
        {reply, undefined, Data};
      Value ->
        Tokens = string:tokens(Value, " "),
        Codepoints = dehex(case hd(Value) of $< -> tl(Tokens); _ -> Tokens end),
        {reply, Codepoints, Data}
    end
  end);
handle_call({composition, A, B}, _From, Data) ->
  Key = lists:flatten([hex(A), " ", hex(B)]),
  case lists:keysearch(Key, ?DECOMPOSITION, Data) of
    {value, Props} ->
      {reply, erlang:list_to_integer(element(1, Props), 16), Data};
    false ->
      {reply, undefined, Data}
  end;
handle_call({load, Source}, _From, _State) when is_binary(Source) ->
  {reply, ok, parse(Source)};
handle_call({load, Source}, _From, State) when is_list(Source) ->
  case lists:prefix("http://", Source) of
    true ->
      case http:request(get, {Source, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Data}} ->
          {reply, ok, parse(Data)};
        _ ->
          {reply, {error, bad_http_response}, State}
      end;
    false ->
      case file:read_file(Source) of
        {ok, Data} ->
          {reply, ok, parse(Data)};
        Error ->
          {reply, Error, State}
      end
  end;
handle_call({lowercase, C}, _From, Data) ->
  lookup(C, Data, fun(Props) ->
    case element(?LOWERCASE_MAPPING, Props) of
      [] ->
        {reply, C, Data};
      Hex ->
        {reply, erlang:list_to_integer(Hex, 16), Data}
    end
  end).

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok.

%%============================================================================
%% Helper functions
%%============================================================================

parse(Data) ->
  parse(Data, []).

parse(<<>>, Acc) ->
  lists:reverse(Acc);
parse(Data, Acc) ->
  {Line, Etc} = break_at($\n, Data),
  Values = re:split(Line, ";", [{return, list}]),
  parse(Etc, [list_to_tuple(Values)|Acc]).

break_at(C, Data) ->
  break_at(C, Data, []).

break_at(_, <<>>, Prefix) ->
  {lists:reverse(Prefix), <<>>};
break_at(C, <<C, T/bytes>>, Prefix) ->
  {lists:reverse(Prefix), T};
break_at(C, <<H, T/bytes>>, Prefix) ->
  break_at(C, T, [H | Prefix]).

hex(Codepoint) ->
  string:right(erlang:integer_to_list(Codepoint, 16), 4, $0).

dehex(Strings) ->
  [erlang:list_to_integer(String, 16) || String <- Strings].

lookup(Codepoint, Data) ->
  lists:keysearch(hex(Codepoint), 1, Data).

lookup(Codepoint, Data, Fun) ->
  case lookup(Codepoint, Data) of
    {value, Props} ->
      Fun(Props);
    false ->
      {reply, {error, bad_codepoint}, Data}
  end.

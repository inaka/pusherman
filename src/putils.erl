-module(putils).

-type datetime() :: {{pos_integer(), 1..12, 1..31}, {0..23, 0..59, 0..59}}.
-export_type([datetime/0]).

-export([binary_to_integer/1,binary_to_atom/1,integer_to_atom/1,proplist_to_json/1,json_decode/1,json_encode/1, json_field/2,
         safe_list_to_float/1,safe_term_to_binary/1,integer_to_binary/1,to_string/1,datetime_for_now/0,
				 make_pairs/1,pad_to16/1,now/0,now_to_gregorian_seconds/1,dateadd/2,get_all_env/0,
				 get_env/1,set_env/2,stop_timer/1,first/3,positive/1,to_lower/1, uuid_utc/0,
				 random_string/1 ]).

-spec integer_to_atom(integer()) -> atom().
integer_to_atom(I) ->
	list_to_atom(integer_to_list(I)).

-spec integer_to_binary(integer()) -> binary().
integer_to_binary(I) ->
  list_to_binary(integer_to_list(I)).

-spec binary_to_integer(binary()) -> integer().
binary_to_integer(B) ->
  list_to_integer(binary_to_list(B)).

-spec binary_to_atom(binary()) -> atom().
binary_to_atom(B) ->
	binary_to_atom(B,utf8).

-spec proplist_to_json(list()) -> term().
proplist_to_json(PropList) ->
	jsx:encode(PropList).

-spec json_decode(binary()) -> list() | {error,term()}.
json_decode(Json) ->
	try jsx:decode(Json)
	catch _:Err -> {error,Err}
	end.

uuid_utc() ->
  Now = {_, _, Micro} = erlang:now(),
  Nowish = calendar:now_to_universal_time(Now),
  Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
  Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
  list_to_binary(Prefix ++ integer_to_list(Micro) ++ mochihex:to_hex(crypto:rand_bytes(9))).

-spec json_encode([proplists:property()]) -> iodata().
json_encode(Props) -> jsx:encode(Props).

-spec json_field(binary()|string(), binary()) -> term().
json_field(Field, Json) ->
  case itweet_mochijson2:get_value(Field, Json) of
    undefined -> null;
    Value -> Value
  end.

-spec datetime_for_now() -> string().
datetime_for_now() ->
	{{Year,Month,Day},{Hours,Mins,Secs}} = calendar:universal_time(),
	string:join(lists:map(fun erlang:integer_to_list/1,[Year,Month,Day]),"-") ++ " " ++
	string:join(lists:map(fun erlang:integer_to_list/1,[Hours,Mins,Secs]),":").

-spec safe_list_to_float(string()) -> float().
safe_list_to_float(String) ->
  try erlang:list_to_float(String)
  catch _:badarg -> erlang:list_to_integer(String) * 1.0
  end.

-spec safe_term_to_binary(float() | integer() | tuple() | atom() | binary() | iolist()) -> binary().
safe_term_to_binary(F) when is_float(F) ->
  iolist_to_binary(io_lib:format("~f", [F]));
safe_term_to_binary(I) when is_integer(I) ->
  list_to_binary(integer_to_list(I));
safe_term_to_binary(L) when is_tuple(L) ->
  list_to_binary([]);
safe_term_to_binary(L) when is_list(L) ->
  unicode:characters_to_binary(L);
safe_term_to_binary(undefined) ->
  <<>>;
safe_term_to_binary(A) when is_atom(A) ->
  list_to_binary(atom_to_list(A));
safe_term_to_binary(A) when is_binary(A) -> A.

-spec to_string(atom() | integer() | binary() | float() | string())  -> string().
to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_string(Float) when is_float(Float) ->
    iolist_to_binary(io_lib:format("~f", [Float]));
to_string(String) ->
    String.

-spec make_pairs([X]) -> [{X, X}].
make_pairs(List) ->
  make_pairs(List,[]).

make_pairs([],Pairs) ->
  lists:reverse(Pairs);

make_pairs([_Odd],Pairs) ->
  lists:reverse(Pairs);

make_pairs([L1,L2|Rem],Pairs) ->
  make_pairs(Rem,[{L1,L2}] ++ Pairs).

-spec pad_to16(binary()) -> binary().
pad_to16(Bin) ->
	Padding_bits = (16 - (size(Bin) rem 16)) * 8,
	<<Bin/binary,0:Padding_bits>>.

-spec now() -> integer().
now() ->
  {_, _, MicroSecs} = erlang:now(),
  Millis = erlang:trunc(MicroSecs/1000),
  calendar:datetime_to_gregorian_seconds(
    calendar:universal_time()) * 1000 + Millis.

-spec now_to_gregorian_seconds(integer()) -> integer().
now_to_gregorian_seconds(Now) ->
	round(Now/1000).

-spec dateadd(datetime(), integer()) -> datetime().
dateadd(Date, Seconds) ->
  calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(Date) + Seconds).

-spec get_all_env() -> [{atom(), term()}].
get_all_env() ->
  application:get_all_env(pusherman).

-spec get_env(atom()) -> term().
get_env(Field) ->
  case application:get_env(pusherman, Field) of
    {ok, Value} ->
      %lager:debug("~p := ~p~n", [Field, Value]),
      Value;
    _ ->
      Value = get_env_default(Field),
      %lager:debug("~p := ~p~n", [Field, Value]),
      Value
  end.

-spec get_env_default(atom()) -> term().
get_env_default(Field) ->
  case Field of
    backend      -> pusherman_memory_backend;
    backend_opts -> ["queue.db",[{create_if_missing,true}]];
    web_host     -> "127.0.0.1";
    web_port     -> 4200;
    apns_pools   -> [ {apns_main, [ {size, 2}, {max_overflow, 2} ], []} ];
    _ -> throw({env_undefined, Field})
  end.

-spec set_env(atom(), term()) -> ok.
set_env(Field, Value) ->
  application:set_env(whisper, Field, Value).

-spec stop_timer(undefined | reference() | timer:tref()) -> ok.
stop_timer(undefined) -> ok;
stop_timer(Ref) when is_reference(Ref) ->
  case erlang:cancel_timer(Ref) of
    false -> ok;
    _Time -> ok
  end;
stop_timer(Timer) ->
  case timer:cancel(Timer) of
    {ok, cancel} -> ok;
    {error, Reason} ->
      lager:warning("Couldn't stop timer ~p: ~p~n", [Timer, Reason]),
      ok
  end.

-spec first(X, X, [X]) -> none | X.
first(_X, _Y, []) -> none;
first(X, _Y, [X|_]) -> X;
first(_X, Y, [Y|_]) -> Y;
first(X, Y, [_Z|Rest]) -> first(X, Y, Rest).

-spec positive(integer()) -> non_neg_integer().
positive(X) when X < 0 -> 0;
positive(X) -> X.

-spec to_lower(binary()) -> binary().
to_lower(Atom) when is_atom(Atom) ->
  binary_to_atom(to_lower(atom_to_binary(Atom,utf8)));
to_lower(Bin) ->
  to_lower(Bin, <<>>).

to_lower(<<>>, Acc) ->
  Acc;
to_lower(<<C, Rest/binary>>, Acc) when $A =< C, C =< $Z ->
  to_lower(Rest, <<Acc/binary, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 128 =< C, C =< 150 -> %% A-0 with tildes plus enye
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 152 =< C, C =< 158 -> %% U and Y with tilde plus greeks
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<C, Rest/binary>>, Acc) ->
  to_lower(Rest, <<Acc/binary, C>>).

-spec random_string(pos_integer()) -> [char()].
random_string(Len) ->
  Chrs = list_to_tuple("abcdefghijklmnopqrstuvwxyz0123456789"),
  ChrsSize = size(Chrs),
  F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
  lists:foldl(F, "", lists:seq(1, Len)).


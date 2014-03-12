-module(push_handler_api).
-behaviour(cowboy_http_handler).

-include_lib("apns/include/apns.hrl").
-include_lib("apns/include/localized.hrl").
-include_lib("include/pusherman_types.hrl").

-export([init/3, handle/2, terminate/3]).

-define(WELCOME, {200, <<"I'm your momma i'm your daddy i'm that web server in the alley.">>}).
-define(OK, {200, <<"ok">>}).
-define(CREATED, {201, <<"ok">>}).
-define(BAD_REQ, {400,<<"400 Bad Request">>}).
-define(BAD_REQ(Error), try {400,<<<<"400 Bad Request ">>/binary,(list_to_binary(Error))/binary>>}
                        catch _:_ -> {400,<<"400 Bad Request">>} end).
-define(UNAUTHORIZED, {401, <<"<body> 401, Ain't I clean? Bad machine / Super cool, super mean</body>">>}).
-define(NOT_FOUND, {404, <<"<body> 404, Insecure from the past / How long can a good thing last? </body>">>}).
%%
%% API Functions
%%

init({tcp, http}, Req, Opts) ->
  {ok, Req, Opts}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  {Path,_} = cowboy_req:path(Req),
	
	{Status, Response} = case {Method,Path} of
    {<<"GET">>,<<"/status">>} -> handle_status(Req);
    {<<"POST">>,<<"/push">>} -> handle_push(Req);
    _ ->  handle_unknown(Method,Path)
  end,
  {ok, Req2} = cowboy_req:reply(Status, [{<<"Content-Type">>, <<"application/json">>}], Response, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%%
%% Local Functions
%%

handle_unknown(Method,Path) ->
  lager:debug("method: ~p path ~p",[Method,Path]),
  {404, list_to_binary(" not found " ++ Path)}.

handle_status(_Req) -> 
  ?OK.

handle_push(Req) -> 
  {ok, [{JsonParams,_}], _} = cowboy_req:body_qs(Req),
  [Params] = jsx:decode(JsonParams),
  lager:debug("json: ~p, decoded  ~p", [JsonParams, Params]),
  [MsgId, DeviceToken, Message, ActionLocKey, Badge, SoundFileName, Expiration, Extra, Type, Callback] = get_params(Params, [
    {binary, <<"msg_id">>, apns:message_id()},
    {string, <<"device_token">>},
    {string, <<"message">>},
    {string, <<"action_loc_key">>, none},
    {integer, <<"badge">>, none},
    {string, <<"sound_file_name">>, none},
    {integer, <<"expiration">>, apns:expiry(86400)},
    {raw, <<"extra">>, []},
    {string, <<"push_type">>,"unknown"},
    {string, <<"callback">>, "undefined"}
  ]),
  SoundFileName2 = case SoundFileName of
    "none" -> none;
    Other -> Other
  end,

  LocalizedMessage = case ActionLocKey of
    none -> Message;
    JsonStruct ->
      JsonContent = jsx:decode(unicode:characters_to_binary(JsonStruct)),
      [Action, Key, Args, Image] = get_params(JsonContent, [
        {string, <<"action">>, none},
        {string, <<"key">>, none},
        {list, <<"args">>, []},
        {string, <<"image">>, none}
      ]),
      #loc_alert{
        body = Message,
        action = Action,
        key = Key,
        args = Args,
        image = Image
      }
  end,

  Msg = #apns_msg{
    id     = MsgId,
    device_token = DeviceToken,
    alert  = LocalizedMessage,
    badge  = Badge,
    sound  = SoundFileName2,
    expiry = Expiration,
    extra  = Extra},
  Push = #push{
    type=Type,
    push=Msg,
    callback=Callback},
  (putils:get_env(backend)):queue(Push),
  ?OK.

%%
%% Utility functions
%%

get_params(Qs, Params) ->
  lists:map(fun(P) -> get_parameter(P, Qs) end, Params).

get_parameter({T,K}, Qs) ->
  case get_parameter({T,K,undefined}, Qs) of
    undefined -> throw({missing_parameter, K});
    V -> V
  end;

get_parameter({list, K, Default}, Qs) ->
  case proplists:get_value(K, Qs) of
    undefined -> Default;
    L when is_list(L) -> L;
    _V -> Default
  end;

get_parameter({boolean, K, Default}, Qs) ->
  case proplists:get_value(K, Qs) of
    true  -> true;
    false -> false;
    <<"true">> -> true;
    <<"false">> -> false;
    _V -> Default
  end;
get_parameter({integer, K, Default}, Qs) ->
  case proplists:get_value(K, Qs) of
    undefined -> Default;
    I when is_integer(I) -> I;
    B when is_binary(B)  -> 
      putils:binary_to_integer(B)
  end;
get_parameter({string, K, Default}, Qs) ->
  case proplists:get_value(K, Qs) of
    undefined -> Default;
    S when is_list(S) -> S;
    B when is_binary(B)  -> 
      putils:to_string(B)
  end;
get_parameter({start_time, K, Default}, Qs) ->
  case proplists:get_value(K, Qs) of
    undefined -> Default;
    <<"now">> -> now;
    I when is_integer(I) -> I;
    V -> list_to_integer(V)
  end;
get_parameter({binary, K, Default}, Qs) ->
  case proplists:get_value(K, Qs) of
    X when is_binary(X) -> X;
    _V -> Default
  end;
get_parameter({raw, K, Default}, Qs) ->
  case proplists:get_value(K, Qs) of
    undefined -> Default;
    X -> X
  end.

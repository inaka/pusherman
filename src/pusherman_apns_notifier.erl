%%%-------------------------------------------------------------------
%%% @doc APNS notifier
%%% @end
%%%-------------------------------------------------------------------
-module(pusherman_apns_notifier).

-behaviour(gen_server).
-include_lib("apns/include/apns.hrl").
-include_lib("include/pusherman_types.hrl").

-define(APPLE_CONNECTION, 'apple-connection').
-define(APPLE_CONNECTION_RESET_TIMEOUT,60 * 60 * 1000).
-define(SLEEP_TIME, 1000).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([]).

-type datetime() :: {{pos_integer(), 1..12, 1..31}, {0..23, 0..59, 0..59}}.

-record(state, { backend :: term(), db :: term()}).
-type state() :: #state{ backend :: term(), 
                        db :: term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec init(_) -> {ok, state(), hibernate}.
init(_) ->
  try
    ensure_started()
  catch
    _:Error -> lager:error("error initializing apns: ~p~n.",[Error])
  end,
  {ok,_} = timer:send_interval(?APPLE_CONNECTION_RESET_TIMEOUT, self(), reset_connection),
  timer:send_after(?SLEEP_TIME, dequeue),
  {ok, #state{}, hibernate}.

%% @hidden
-spec handle_call(X, reference(), state()) -> {stop, {unknown_request, X}, {unknown_request, X}, state()}.
handle_call(X, _From, State) ->
  {stop, {unknown_request, X}, {unknown_request, X}, State}.

handle_cast(_,State) -> 
  {noreply, State}.

%% @hidden
handle_info(reset_connection,State) ->
  lager:debug("Resetting Apple Connection"),
  apns:disconnect(?APPLE_CONNECTION),
  {noreply,State};

handle_info(dequeue,State) ->
  Push = (putils:get_env(backend)):dequeue(),
  case Push of
    {error, queue_empty} -> timer:send_after(?SLEEP_TIME, dequeue);
    P -> 
      ensure_started(),
      apns:send_message(?APPLE_CONNECTION, P#push.push), 
      erlstatsd:increment("pusherman.pushes-" ++ P#push.type, 1, 1.0),
      self() ! dequeue
  end,
  {noreply,State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(Reason, _State) ->
  lager:error("~p terminating notifier: ~p~n", [?MODULE, Reason]).

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state(), pos_integer()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_apns_error(binary(), apns:status()) -> ok.
handle_apns_error(MsgId, Status) ->
  gen_server:cast(?MODULE, {error, MsgId, Status}).

handle_uninstall({_,ApnsToken}) ->
  whisper_user:delete_apns_token(ApnsToken).

-spec ensure_started() -> ok.
ensure_started() ->
  try apns:connect(?APPLE_CONNECTION,
      fun handle_apns_error/2,
      fun handle_uninstall/1) of
    {ok, _} ->
      erlstatsd:increment("pusherman.apns-connected", 1, 1.0),
      lager:info("Connected to apple",[]), ok;
    {error, {already_started, _}} ->
      lager:info("Already connected to apple",[]),
      ok;
    {error, Reason} ->
      lager:error("Couldn't start APNS4ERL: ~p. Apple Push Notifications will not work until the admin fixes this~n", [Reason]),
      throw({error,apns_error})
    catch
      _:{noproc,Reason} ->
        lager:error("Apns application is down: ~p, trying to restart~n", [Reason]),
        apns:start();
      _:Reason ->
        lager:error("Couldn't start APNS4ERL: ~p. Apple Push Notifications will not work until the admin fixes this~n", [Reason]),
        throw({error,apns_error})
    end.

do_send(ApnsToken,Message,SoundFileName,Expiration,Extra,Type) -> 
  try
    ensure_started(),
    MsgId = apns:message_id(),
    Response = apns:send_message(?APPLE_CONNECTION, MsgId, binary_to_list(ApnsToken), Message, 1,
      SoundFileName,
      apns:expiry(Expiration), Extra), 
    lager:debug("notifier sent message_id: ~p with message ~p. response: ~p type: ~p", [MsgId, Message,Response,Type])
  catch
    _:Error ->
      lager:debug("error ~p sending: ~p with message ~p. response: ~p type: ~p", [Error, Message,Type])
  end.


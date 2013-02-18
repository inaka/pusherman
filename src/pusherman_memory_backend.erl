%%%-------------------------------------------------------------------
%%% @author Chad DePue <chad@inakanetworks.com>
%%% @doc in-memory backend for testing
%%% @end
%%%-------------------------------------------------------------------
-module(pusherman_memory_backend).
-author('Chad DePue <chad@inakanetworks.com>').

-behaviour(pusherman_backend).

-record(state, {db = [] :: list()}).

-export([start_link/2, queue/1,dequeue/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

queue(Item) ->
  gen_server:cast(?MODULE,{queue,Item}).

dequeue() -> 
  gen_server:call(?MODULE,dequeue).

start_link(File, Options) -> 
  State = #state{db = []},
  gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

init(State) -> 
  {ok, State}.

handle_call(dequeue, _From, State = #state{db=Db}) -> 
  {Reply,NewState} = case Db of
    [] -> {{error, queue_empty},State};
    [I|Rest] -> {I,#state{db=Rest}}
  end,
  {reply, Reply, NewState};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({queue,Item}, State = #state{db=Db}) -> 
  NewState = State#state{db= Db ++ [Item]},
  {noreply, NewState};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%-------------------------------------------------------------------
%%% @author Chad DePue <chad@inakanetworks.com>
%%% @doc leveldb based backend
%%% @end
%%%-------------------------------------------------------------------
-module(pusherman_leveldb_backend).
-author('Chad DePue <chad@inakanetworks.com>').

-behaviour(pusherman_backend).

-record(state, {db    :: eleveldb:db_ref(),
                file  :: string()}).

-export([start_link/2, queue/1,dequeue/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

queue(Item) ->
  gen_server:cast(?MODULE,{queue,Item}).

dequeue() -> 
  gen_server:call(?MODULE,dequeue).

start_link(File, Options) -> 
  case eleveldb:open(File, Options) of
    {ok, Db} -> State = #state{db = Db, file = File},
                gen_server:start_link({local, ?MODULE}, ?MODULE, State, []);
    Error -> throw({error,Error})
  end.

init(State) -> 
  {ok, State}.

handle_call(dequeue, _From, State = #state{db=Db}) -> 
  Reply = case eleveldb:is_empty(Db) of
    true -> {error, queue_empty};
    false -> eleveldb:iterator(Db,[])
  end,
  {reply, Reply, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({queue,Item}, State = #state{db=Db}) -> 
  eleveldb:write(Db, {put, putils:uuid_utc(), erlang:term_to_binary(Item)}, []),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

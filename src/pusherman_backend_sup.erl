-module(pusherman_backend_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  %% Backend is defined as {Module, Opts} in config. Default is just leveldb called "queue.db"
  Backend = putils:get_env(backend),
  Opts = putils:get_env(backend_opts),
  BackendSup = [ {Backend, {Backend, start_link, Opts}, permanent, brutal_kill, worker, [Backend]} ],
  {ok, {{one_for_one, 5, 10}, BackendSup}}.




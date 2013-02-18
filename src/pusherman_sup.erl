
-module(pusherman_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  lager:info("starting pusherman..."),

  Children = [
    {pusherman_web,{pusherman_web,start_link,[]},permanent, brutal_kill, worker,[pusherman_web]},
    {pusherman_backend_sup,{pusherman_backend_sup,start_link,[]},permanent, brutal_kill, supervisor,[pusherman_backend_sup]},
    {pusherman_apns_notifier,{pusherman_apns_notifier,start_link,[]},permanent, brutal_kill, worker, [pusherman_apns_notifier]}
  ],
  {ok, { {one_for_one, 5, 10}, Children} }.


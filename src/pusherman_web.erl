-module(pusherman_web).

-export([start_link/0]).

-define(WWW_PATH(Root, Paths), string:join([WwwRoot|Paths], "/")).

%%
%% API Functions
%%
-spec start_link() -> {ok,pid()}.
start_link() ->
  WebPort = putils:get_env(web_port),
	Dispatch = cowboy_router:compile([
		{'_', [
      {'_', push_handler_api, []}
		]}
	]),
	cowboy:start_http(http, 100, [{port, WebPort}], [{env, [{dispatch, Dispatch}]}]).


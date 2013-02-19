-module(pusherman_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("apns/include/apns.hrl").

-define(API_URL, "http://" ++ putils:get_env(web_host) ++ ":" ++ integer_to_list(putils:get_env(web_port))).

groups() -> [].

all() -> [test_push].

test_push(Config) ->
  Url = ?API_URL ++ "/push",
  {ok, Reply} = httpc:request(post, {Url, [],
      "application/x-www-form-urlencoded",
      "{ ""msg_id"":1, ""device_token"":""1234"", ""message"":""curtis""}"
      }, [], []),
  io:format("Reply: ~p\n", [Reply]).



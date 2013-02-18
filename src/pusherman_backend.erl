%%%-------------------------------------------------------------------
%%% @author Chad DePue <chad@inaka.net>
%%% @doc DB backend behaviour for pusherman
%%% @end
%%%-------------------------------------------------------------------
-module(pusherman_backend).
-author('Chad DePue <chad@inakanetworks.com>').

-export([behaviour_info/1]).

%% @hidden
-spec behaviour_info(callbacks|term()) -> undefined | [{atom(), non_neg_integer()}].
behaviour_info(callbacks) -> [{start_link,2}, {queue, 1}, {dequeue, 0}];
behaviour_info(_) -> undefined.


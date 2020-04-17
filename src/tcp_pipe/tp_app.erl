%%%-------------------------------------------------------------------
%%% @doc 应用模块
%%%         负责启动顶层监督进程
%%% @end
%%%-------------------------------------------------------------------

-module(tp_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%%%===================================================================
%%% Application回调
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 启动应用
%% @spec start(StartType, StartArgs) -> {ok, SupPid :: pid()}
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    tp_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc (暂时不关注)
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================


%%%-------------------------------------------------------------------
%%% @doc 启动和停止应用模块
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_pipe).
-export([start/0, stop/0]).

%%-------------------------------------------------------------------
%% @doc 启动应用(可用于erl启动参数-s)
%% @spec start() -> ok
%% @end
%%-------------------------------------------------------------------
start() ->
    application:start(tcp_pipe).

%%-------------------------------------------------------------------
%% @doc 停止应用
%% @spec stop() -> ok
%% @end
%%-------------------------------------------------------------------
stop() ->                                                                    % 停止1端应用
    application:stop(tcp_pipe).


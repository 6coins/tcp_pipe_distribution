%%%-------------------------------------------------------------------
%%% @doc 端监督进程
%%%         负责读取配置根据配置启动指定一端的监督进程
%%% @end
%%%-------------------------------------------------------------------

-module(tp_side_sup).

-behaviour(supervisor).

%% API
-export([start_link/4, 
         start_link/7, 
         start_child/2
        ]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% 接口
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 启动监督进程
%% @spec start_link(Side, SideSupName, OtherSideSupName, SocketDist) -> {ok, pid()} | {error, Reason}
%%          Side = SideSupName = OtherSideSupName = atom()
%%          Reason = term()
%% @end
%%--------------------------------------------------------------------
start_link(Side, SideSupName, OtherSideSupName, SocketDist) ->
    Type = pipe_util:get_side_type(Side),
    ChildModule = pipe_util:get_child_module(Side),
    Listen = pipe_util:get_side_listen(Side),
    start_link(Type, Side, SideSupName, OtherSideSupName, ChildModule, Listen, 
               SocketDist).

start_link(accept, Side, SideSupName, OtherSideSupName, ChildModule, Listen, 
           SocketDist) -> % 如果此端需要接受连接
    case supervisor:start_link(
            {local, SideSupName}, ?MODULE,
            [ChildModule, Side, SideSupName, OtherSideSupName, Listen, 
             SocketDist]) of      % 启动监督进程
        {ok, SupPid} ->                                                         % 成功后
            {ok, _} = tp_side_sup:start_child(SideSupName, undefined),          % 启动一个子进程等待接收连接
            {ok, SupPid};
        ErrorAndReason ->
           ErrorAndReason 
    end;
start_link(Type, Side, SideSupName, OtherSideSupName, ChildModule, Listen, 
           SocketDist) 
        when (Type =:= connect) or (Type =:= connect_x) ->                      % 否则
    supervisor:start_link(
        {local, SideSupName}, ?MODULE,
        [ChildModule, Side, SideSupName, OtherSideSupName, Listen, SocketDist]).% 启动监督进程

%%--------------------------------------------------------------------
%% @doc 启动子进程
%% @spec start_child(SideSupName:: atom(), OtherChild:: pid()) -> {ok, pid()}
%% @end
%%--------------------------------------------------------------------
start_child(SideSupName, OtherChild) ->
    supervisor:start_child(SideSupName, [OtherChild]).

%%%===================================================================
%%% Supervisor回调
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 初始化顶层监督
%% @end
%%--------------------------------------------------------------------
init([ChildModule | Args]) ->
    Child = {ChildModule, {ChildModule, start_link, Args},
             temporary, brutal_kill, worker, [ChildModule]},
    Children = [Child],
    Restart = {simple_one_for_one, 0, 1},
    {ok, {Restart, Children}}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

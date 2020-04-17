%%%-------------------------------------------------------------------
%%% @doc 顶层督程
%%% @end
%%%-------------------------------------------------------------------

-module(tp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%-compile(export_all).

%%%===================================================================
%%% 接口
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 启动顶层督程
%% @spec start_link() -> {ok, pid()} | {error, Reason)}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
 
%%%===================================================================
%%% Supervisor回调
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 初始化督程
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    Restart = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    OneSupName = pipe_util:get_side_sup_name(one),
    TwoSupName = pipe_util:get_side_sup_name(two),

    SocketDist = get_socket_dist_module(),
    Dist = {SocketDist, {SocketDist, start_link, []},
            permanent, 5000, worker, [SocketDist]},                             % 用于socket分布的进程

    OneSup = {OneSupName, {tp_side_sup, start_link, 
                           [one, OneSupName, TwoSupName, SocketDist]},
              permanent, infinity, supervisor, [tp_side_sup]},                  % 1端监督进程规格

    TwoSup = {TwoSupName, {tp_side_sup, start_link, 
                           [two, TwoSupName, OneSupName, SocketDist]},
              permanent, infinity, supervisor, [tp_side_sup]},                  % 2端监督进程规格

    Children = [Dist, OneSup, TwoSup],

    {ok, {Restart, Children}}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 得到socket分布的模块
%% @spec get_socket_dist_module() -> tp_socket_dist | tp_socket_dist_aa_cc
%% @end
%%--------------------------------------------------------------------
get_socket_dist_module() -> 
    case is_both_accept_or_connect() of
        true ->
            tp_socket_dist_aa_cc;                                               % 特殊socket分布模块
        false ->
            tp_socket_dist                                                      % 一般socket分布模块
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 是否是socket分布特殊情况(即非中间人端配置和分布配置中的type都是accept或connect)
%% @spec is_both_accept_or_connect() -> boolean()
%% @end
%%--------------------------------------------------------------------
is_both_accept_or_connect() ->
    case pipe_util:get_dist_config() of
        undefined ->                                                            % 没有分布配置
            false;
        [] ->                                                                   % 没有分布配置
            false;
        DistConfig ->                                                           % 有分布配置
            MmChildSide = proplists:get_value(mm_child_side, DistConfig),
            DistType = proplists:get_value(type, DistConfig),
            Side = pipe_util:get_the_other_side(MmChildSide),
            SideConfig = pipe_util:get_side_config(Side),
            SideType = proplists:get_value(type, SideConfig),
            is_same_type(DistType, SideType)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 即非中间人端配置和分布配置中的type都是accept或connect(connect_x)
%% @spec is_same_type(Type1, Type2) -> boolean()
%% @end
%%--------------------------------------------------------------------
is_same_type(Type1, Type2) when Type1 =:= Type2 -> 
    true;
is_same_type(Type1, Type2) when (Type1 =:= connect) and (Type2 =:= connect_x) ->
    true;
is_same_type(Type1, Type2) when (Type2 =:= connect) and (Type1 =:= connect_x) ->
    true;
is_same_type(_Type1, _Type2) ->
    false.


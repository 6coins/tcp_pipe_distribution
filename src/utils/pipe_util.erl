%%%-------------------------------------------------------------------
%%% @doc 子管子处理的公共模块
%%%         (测试函数见src/tests目录)
%%% @end
%%%-------------------------------------------------------------------

-module(pipe_util).

-export([get_side_sup_name/1,
         is_mm_child/2,
         get_side_type/1,
         get_side_config/1,
         get_side_listen/1,
         get_child_module/1,
         get_dist_type/0,
         get_dist_listen/1,
         get_dist_config/0,
         get_the_other_side/1,
         sub_pipes_handle/3,
         no_print/1,
         no_print/2
        ]).

%%%===================================================================
%%% 接口
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 得到端配置
%% @spec get_side_config(Side :: atom()) -> list() | undefined
%% @end
%%--------------------------------------------------------------------
get_side_config(Side) -> 
    case application:get_env(tcp_pipe, Side) of
        undefined ->
            undefined;
        {ok, Config} ->
            Config
    end.

%%--------------------------------------------------------------------
%% @doc 从配置中得到类型
%% @spec get_side_type(Side) -> atom()
%% @end
%%--------------------------------------------------------------------
get_side_type(Side) ->
    SideConfig = pipe_util:get_side_config(Side),
    _Type = proplists:get_value(type, SideConfig).

%%--------------------------------------------------------------------
%% @doc 得到端对应的监听
%% @spec get_side_listen(Side) -> listensocket() | undefined
%% @end
%%--------------------------------------------------------------------
get_side_listen(Side) ->
    SideConfig = get_side_config(Side),
    DistConfig = get_dist_config(),
    Type = proplists:get_value(type, SideConfig),                               % 端类型
    IsMMChild = is_mm_child(Side, DistConfig),                                  % 子进程是否是中间人
    get_side_listen_by_type(Type, IsMMChild, SideConfig).                       % 如果需要监听则监听

%%--------------------------------------------------------------------
%% @doc 得到子进程模块
%% @spec get_child_module(Side :: atom()) -> tp_side | tp_side_mm
%% @end
%%--------------------------------------------------------------------
get_child_module(Side) ->                                                       % 如果有socket分布配置
    DistConfig = get_dist_config(),
    IsSoketDist = proplists:get_value(is_socket_dist, DistConfig, false),       % 是否socket分布
    IsMMChild = is_mm_child(Side, DistConfig),                                  % 子进程是否是中间人
    case {IsSoketDist, IsMMChild} of 
        {true, true} -> 
            tp_side_mm;
        _ -> 
            tp_side
    end.

%%--------------------------------------------------------------------
%% @doc 得到socket分布配置
%% @spec get_socket_dist_config() -> list() | []
%% @end
%%--------------------------------------------------------------------
get_dist_config() ->
    case application:get_env(tcp_pipe, socket_dist) of
        undefined ->
            [];
        {ok, Config} ->
            Config
    end.

%%--------------------------------------------------------------------
%% @doc 从分布配置中得到类型
%% @spec get_dist_type() -> atom() | undefined
%% @end
%%--------------------------------------------------------------------
get_dist_type() ->
    DistConfig = get_dist_config(),
    _Type = proplists:get_value(type, DistConfig, undefined).

%%--------------------------------------------------------------------
%% @doc 得到分布对应的监听
%% @spec get_dist_listen(DefaultOptions) -> listensocket() | undefined
%% @end
%%--------------------------------------------------------------------
get_dist_listen(DefaultOptions) ->
    DistConfig = get_dist_config(),
    case proplists:get_value(type, DistConfig, undefined) of
        accept ->
            Port = proplists:get_value(port, DistConfig),
            Options = proplists:get_value(options, DistConfig, DefaultOptions),
            {ok, Listen} = gen_tcp:listen(Port, Options),
            Listen;
        _ ->
            undefined
    end.

%%-------------------------------------------------------------------
%% @doc 得到另一端
%% @spec get_the_other_side(Side :: atom()) -> TheOtherSide :: atom()
%% @end
%%-------------------------------------------------------------------
get_the_other_side(one) ->
    two;
get_the_other_side(two) ->
    one.

%%-------------------------------------------------------------------
%% @doc 得到端督程名字
%% @spec get_side_sup_name(Side :: atom()) -> SupName :: atom()
%% @end
%%-------------------------------------------------------------------
get_side_sup_name(Side) -> 
    _SupName = list_to_atom("tp_" ++ atom_to_list(Side) ++ "_sup").

%%-------------------------------------------------------------------
%% @doc 子管子依次处理, 并更新处理结果
%%          如果其中有一个子管子处理时报错, 会返回报错子管子名(SubPipe)和报错原因(Reason)
%% @spec sub_pipes_handle(Event, Subpipes, In) -> {ok, Out} | {error, SubPipe, Reason}
%%          Event = atom()
%%          Subpipes = list() | undefined, 
%%          In = Out = term()
%%          SubPipe = atom()
%%          Reason = term()
%% @end
%%-------------------------------------------------------------------
sub_pipes_handle(_Event, undefined, In) ->
    {ok, In}; 
sub_pipes_handle(Event, Subpipes, In) ->
    Fun = fun(Subpipe, {ok, In1} = _LastRet) -> Subpipe:handle(Event, In1);     % 如果上个子管子处理不报错,则继续子管子处理
             (_Subpipe, {error, _LastSubPipe, _Reaso} = LastRet) -> LastRet     % 如果上个子管子处理报错, 则返回报错信息
          end,
    lists:foldl(Fun, {ok, In}, Subpipes).

%%-------------------------------------------------------------------
%% @doc 不打印信息
%% @spec 
%% @end
%%-------------------------------------------------------------------
no_print(_) ->
    no_print.
no_print(_, _) ->
    no_print.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 得到端对应的监听
%% @end
%%--------------------------------------------------------------------
get_side_listen_by_type(accept, false, SideConfig) ->                           % 如果类型是接受监听
    Port = proplists:get_value(port, SideConfig),
    Options = proplists:get_value(options, SideConfig),
    {ok, Listen} = gen_tcp:listen(Port, Options),                               % 监听端口(承诺成功)
    Listen;
get_side_listen_by_type(_, _, _) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc 子进程是否需要是中间人进程(即指定端不是分布的当前端)
%% @spec is_mm_child(Side :: atom(), SocketDistConfig :: list() | undefined) -> boolean()
%% @end
%%--------------------------------------------------------------------
is_mm_child(Side, SocketDistConfig) ->                                          % 如果有socket分布配置
    case proplists:get_value(mm_child_side, SocketDistConfig, undefined) of     % 分布所在的当前端
        Side ->
            true;
        _ ->
            false
    end.



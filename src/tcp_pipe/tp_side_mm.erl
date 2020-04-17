%%--------------------------------------------------------------------
%% @doc 端中间人进程
%%          负责socket分布时代理端
%%          不分布是:
%%          1端 -call->  2端
%%          1端 <-reply- 2端
%%          分布是:
%%          1端 -call->  2端MM -<<call>>->  | -<<call>>->  1端MM -call->  2端
%%          1端 <-reply- 2端MM <-<<reply>>- | <-<<reply>>- 1端MM <-reply- 2端
%%
%% @end
%%--------------------------------------------------------------------
-module(tp_side_mm).

-behaviour(gen_server).

%% API
-export([start_link/6]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("pipe_recode.hrl").

%% 允许远程调用的函数
-define(ALLOW_CALLS, [{gen_server, cast}]).

%%%===================================================================
%%% 接口
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 启动进程
%% @spec start_link(Side, SideSupName, OtherSideSupName, Listen, SocketDist, OtherChild)
%%          -> {ok, pid()} {error, Reason)}).
%%          Side = SideSupName = OtherSideSupName = atom()
%%          Listen = listensocket() | undefined
%%          OtherChild = pid() | undefined
%%          Reason = term()
%% @end
%%--------------------------------------------------------------------
start_link(Side, SideSupName, OtherSideSupName, _Listen, SocketDist, OtherChild) ->
    Type = pipe_util:get_side_type(Side),
    gen_server:start_link(
        ?MODULE, 
        [Side, SideSupName, OtherSideSupName, OtherChild, Type, SocketDist],
        []).

%%%===================================================================
%%% gen_server 回调
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 初始化进程
%% @end
%%--------------------------------------------------------------------
init([Side, SideSupName, OtherSideSupName, OtherChild, Type, SocketDist]) ->
    process_flag(trap_exit, true),                                              % 监督进程可以停止她
    State = #state{module = ?MODULE,
                   side = Side,
                   type = Type,
                   socket_dist = SocketDist,
                   side_sup_name = SideSupName,
                   other_side_sup_name = OtherSideSupName,
                   other_child = OtherChild},                                   % 初始化状态
    self() ! accept_or_connect,                                                       % 通知自己,进行启动后需要执行的处理
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc (暂时不关注)
%% @end
%%--------------------------------------------------------------------
handle_call(_Data, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc 发送数据处理
%% @end
%%--------------------------------------------------------------------
handle_cast({send, _Data} = Msg, #state{side = _Side, 
                                        socket = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, false}]),                               % 用gen_tcp:recv/2收数据
    Request = {request_mm, {gen_server, cast, [Msg]}},                          % 请求内容是调用另一个中间人的gen_server:call(Pid, Data).
    BinRequest = term_to_binary(Request),                                       % 编码请求
    ok = gen_tcp:send(Socket, BinRequest),                                      % 请求给另一个中间人
    %{ok, BinResponse} = gen_tcp:recv(Socket, 0),                                % 得到响应
    %{response_mm, _Reply} = binary_to_term(BinResponse),                         % 解码响应
    ok = inet:setopts(Socket, [{active, once}]),                                % 用handle_info/2收数据
    {noreply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc 和另一端子进程一起停止处理
%% @end
%%--------------------------------------------------------------------
handle_cast({together_stop}, State) ->
    %{stop, together_stop, State}.
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc 进程启动后的处理
%% @end
%%--------------------------------------------------------------------
handle_info(accept_or_connect, #state{type = accept, 
                                      socket_dist = SocketDist} = State) ->     % 如果此端需要接收连接
    Ret = SocketDist:wait_socket(self()),                                       % 阻塞，等待得到socket
    wait_socket_ret_handle(Ret, State);                                         % 接受连接后的处理

handle_info(accept_or_connect, #state{type = Type, 
                                      socket_dist = SocketDist} = State) 
        when (Type =:= connect) or (Type =:= connect_x) ->                      % 如果此端需要连接
    Ret = SocketDist:get_socket(self()),                                        % 阻塞，等待得到socket
    get_socket_ret_handle(Ret, State);                                          % 连接后的处理

%%--------------------------------------------------------------------
%% @private
%% @doc tcp连接收到数据的处理
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, BinRequest}, 
            #state{socket = Socket, other_child = OtherChild} = State) ->
    {request_mm, {M, F, A}} = binary_to_term(BinRequest),                       % 解码数据
    _Reply = dist_apply(M, F, [OtherChild | A]),                                % 根据请求调用函数得到回复
    %BinResponse = term_to_binary({response_mm, Reply}),                         % 编码响应
    %ok = gen_tcp:send(Socket, BinResponse),                                     % 响应给另一个中间人
    ok = inet:setopts(Socket, [{active, once}]),                                % 继续接收数据
    {noreply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc tcp连接关闭的处理
%% @end
%%--------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State) ->
    %{stop, tcp_closed, State};
    {stop, normal, State};

%%--------------------------------------------------------------------
%% @private
%% @doc tcp连接异常的处理
%% @end
%%--------------------------------------------------------------------
handle_info({tcp_error, Socket, _Reason} = Msg, #state{side = _Side, 
                                                       socket = Socket} = State) ->
    {stop, Msg, State};

%%--------------------------------------------------------------------
%% @private
%% @doc 未知消息的处理
%% @end
%%--------------------------------------------------------------------
handle_info(UnknownMsg, State) ->
    {stop, {unknown_msg, UnknownMsg}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 终止进程时的清理
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket = Socket, other_child = OtherChild}) ->
    ok = gen_server:cast(OtherChild, {together_stop}),                          % 通知另一端子进程一起停止
    case is_port(Socket) of                                                     % 关闭socket
        true -> gen_tcp:close(Socket);
        false -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc (暂时不关注)
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 接受到连接后的处理
%% @spec wait_socket_ret_handle(Ret, State) -> {noreply, NewState} | {stop, Reason, State};
%% @end
%%--------------------------------------------------------------------
wait_socket_ret_handle({ok, Socket}, 
                       #state{side_sup_name = SideSupName,
                              other_side_sup_name = OtherSideSupName} = State) ->% 成功接受连接
    {ok, _} = tp_side_sup:start_child(SideSupName, undefined),                  % 让此端sup创建子进程等待接受下一个连接
    {ok, OtherChild} = tp_side_sup:start_child(OtherSideSupName, self()),       % 让另一端sup创建子进程
    State1 = State#state{socket = Socket, other_child = OtherChild},            % 更新状态
    {noreply, State1}.
%wait_socket_ret_handle({error, Reason}, State) ->                               % 某端监听socket报错
%    Reason1 = lists:concat([?MODULE, " ", State#state.side, " ", 
%                            State#state.type, " socket ", Reason]),                   
%    {stop, {error, Reason1}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc 连接后的处理
%% @spec get_socket_ret_handle(Ret, State)-> {noreply, NewState} | {stop, Reason, State};
%% @end
%%--------------------------------------------------------------------
get_socket_ret_handle({ok, Socket}, State) ->                                   % 成功连接
    State1 = State#state{socket = Socket},                                      % 更新状态
    {noreply, State1}.
%get_socket_ret_handle({error, Reason}, State) ->                                % 某端监听socket报错
%    Reason1 = lists:concat([?MODULE, " ", State#state.side, " ", 
%                            State#state.type, " socket ", Reason]),                   
%    {stop, {error, Reason1}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc 先检查是否允许远程调用, 如果允许才调用
%% @spec dist_apply(M, F, A) -> Ret | {error, Reason}
%% @end
%%--------------------------------------------------------------------
dist_apply(M, F, A) ->
    case lists:member({M, F}, ?ALLOW_CALLS) of
        true ->                                                                 % 如果调用的函数在允许的范围内
            _Ret = apply(M, F, A);                                              % 则调用
        _ ->
            {error, "not allow call"} 
    end.


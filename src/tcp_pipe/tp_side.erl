%%--------------------------------------------------------------------
%% @doc 端进程
%% @end
%%--------------------------------------------------------------------

-module(tp_side).

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
start_link(Side, SideSupName, OtherSideSupName, Listen, SocketDist, OtherChild) ->
    Config = pipe_util:get_side_config(Side),
    Type = proplists:get_value(type, Config),
    gen_server:start_link(
        ?MODULE, 
        [Side, SideSupName, OtherSideSupName, Listen, SocketDist, OtherChild, 
         Type, Config],
        []).

%%%===================================================================
%%% gen_server 回调
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 初始化进程
%% @end
%%--------------------------------------------------------------------
init([Side, SideSupName, OtherSideSupName, Listen, _SocketDist, OtherChild, 
      Type, Config]) ->
    process_flag(trap_exit, true),                                              % 监督进程可以停止她
    State = #state{module = ?MODULE,
                   side = Side,
                   type = Type,
                   listen = Listen, 
                   side_sup_name = SideSupName,
                   other_side_sup_name = OtherSideSupName,
                   config = Config,
                   other_child = OtherChild},                                   % 初始化状态
    self() ! accept_or_connect,                                                       % 通知自己,进行启动后需要执行的处理
    {ok, State}.

handle_call(_Data, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc 发送数据处理
%% @end
%%--------------------------------------------------------------------
handle_cast({send, Data}, #state{config = Config} = State) ->
    SubPipes = proplists:get_value(sub_pipes, Config),
    {ok, {Data1, State1}} 
    = pipe_util:sub_pipes_handle(send, SubPipes, {Data, State}),          % 让子管子们进行发送数据的处理
    Ret = gen_tcp:send(State1#state.socket, Data1),
    case Ret of
        ok ->   % 发送成功
            {noreply, State1};
        _ ->    % 发送异常
            {stop, Ret, State1}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc 和另一端子进程一起停止处理(如果socket分布, 另一个应用的两个子进程也会一起停止)
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
                                      listen = Listen} = State) ->              % 如果此端需要接收连接
    Ret = gen_tcp:accept(Listen),                                               % 等待接受连接
    wait_socket_ret_handle(Ret, State);                                         % 接受连接后的处理

handle_info(accept_or_connect, #state{type = connect, 
                                      config = Config} = State) ->          % 如果此端需要连接
    Address = proplists:get_value(address, Config),
    Port = proplists:get_value(port, Config),
    Options = proplists:get_value(options, Config),
    Timeout = proplists:get_value(timeout, Config),
    Ret = gen_tcp:connect(Address, Port, Options, Timeout),                     % 连接
    get_socket_ret_handle(Ret, State);                                          % 连接后的处理

handle_info(accept_or_connect, #state{type = connect_x} = State) ->             % 如果此端连接不确定(即需要在子管子after_accept事件时指定连接)
    {noreply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc tcp连接收到数据的处理
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, #state{socket = Socket, 
                                        other_child = OtherChild,
                                        config = Config} = State) ->       % 如果此端收到数据
    SubPipes = proplists:get_value(sub_pipes, Config),
    {ok, {Data1, State1}} 
    = pipe_util:sub_pipes_handle(recive, SubPipes, {Data, State}),        % 让子管子们进行接收数据的处理
    ok = gen_server:cast(OtherChild, {send, Data1}),                            % 发给另一端子tp_side或tp_side_mm进程, 让其处理
    ok = inet:setopts(Socket, [{active, once}]),                                % 激活此端继续接收数据
    {noreply, State1};

%%--------------------------------------------------------------------
%% @private
%% @doc tcp连接关闭的处理
%% @end
%%--------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

%%--------------------------------------------------------------------
%% @private
%% @doc tcp连接异常的处理
%% @end
%%--------------------------------------------------------------------
handle_info({tcp_error, Socket, _Reason} = Msg, 
            #state{side = _Side, socket = Socket} = State) ->
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
wait_socket_ret_handle(
    {ok, Socket}, 
    #state{side_sup_name = SideSupName,
           other_side_sup_name = OtherSideSupName} = State) ->                  % 成功接受连接
    {ok, _} = tp_side_sup:start_child(SideSupName, _OtherChild = undefined),    % 让此端sup创建子进程等待接受下一个连接
    {ok, OtherChild} = tp_side_sup:start_child(OtherSideSupName, self()),       % 让另一端sup创建子进程
    State1 = State#state{socket = Socket, other_child = OtherChild},            % 更新状态
    {noreply, State1}.
%wait_socket_ret_handle({error, closed}, State) ->                               % 某端监听socket关闭认为没错
%    {stop, normal, State};
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


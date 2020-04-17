%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @doc 此模块用于特殊socket分布处理(即应用是分布，并且两端都是接收连接或连接)
%%% @end
%%%-------------------------------------------------------------------

-module(tp_socket_dist_aa_cc).

-behaviour(gen_server).

%% API
-export([start_link/0,
         wait_socket/1,
         get_socket/1,
         cast_if_no_usable_socket_then_create/0,
         get_state/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {module = ?MODULE,
                type,
                listen,
                socket,
                config,
                usable_socket_list = []     % 此项用于表示可用soket的编号和和socket, 如结构[{id, socket}]
               }).

%% 默认tcp选项
-define(DEFAULT_OPTIONS, [binary, 
                           {reuseaddr, true},
                           {active, false},             % 方便同步协商数据
                           {packet, 4}                  % 防止把大的数据包拆成小数据包发送
                          ]).

-define(USABLE_SOCKET_NUM, 4).             % 可用soket的数量

%% TODO 调试使用, 记得注释掉
%-compile(export_all).

%%%===================================================================
%%% 接口
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 启动进程
%% @spec start_link() ->  {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Type = pipe_util:get_dist_type(),
    Listen = pipe_util:get_dist_listen(?DEFAULT_OPTIONS),
    Config = pipe_util:get_dist_config(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Type, Listen, Config], []).

%%--------------------------------------------------------------------
%% @doc 等待得到socket(会阻塞)
%% @spec wait_socket(Pid) -> {ok, Socket} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
wait_socket(Pid) ->
    gen_server:call(?MODULE, {wait_socket, Pid}, infinity).                     % 这可能会接受连接阻塞, 所以要设置成不超时(即infinity)

%%--------------------------------------------------------------------
%% @doc 得到socket
%% @spec get_socket(Pid) -> {ok, Socket} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_socket(Pid) ->
    gen_server:call(?MODULE, {get_socket, Pid}).

%%--------------------------------------------------------------------
%% @doc 得到进程状态
%% @spec gen_server:cast(?MODULE, {if_no_usable_socket_then_create}) -> ok
%% @end
%%--------------------------------------------------------------------
cast_if_no_usable_socket_then_create() ->
    gen_server:cast(?MODULE, {if_no_usable_socket_then_create}).

%%--------------------------------------------------------------------
%% @doc 得到进程状态
%% @spec get_state() -> State
%% @end
%%--------------------------------------------------------------------
get_state() ->
    gen_server:call(?MODULE, {get_state}).

%%%===================================================================
%%% gen_server 回调
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 初始化
%% @end
%%--------------------------------------------------------------------
init([Type, Listen, Config]) ->
    process_flag(trap_exit, true),                                              % 监督进程可以停止她
    State = #state{type = Type, listen = Listen, config = Config},
    self() ! accept_or_connect,
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc 根据自己端的类型进行接收连接或连接
%% @end
%%--------------------------------------------------------------------
handle_info(accept_or_connect, #state{type = accept, 
                                      listen = Listen} = State) ->              % 特殊socket分布且需要接受连接
    Ret = gen_tcp:accept(Listen),                                               % 接受连接
    accept_or_connect_handle(Ret, State);                                       % 接受连接后的处理
handle_info(accept_or_connect, #state{type = Type, config = Config} = State) 
        when (Type =:= connect) or (Type =:= connect_x) ->                      % 特殊socket分布且需要连接
    Address = proplists:get_value(address, Config),
    Port = proplists:get_value(port, Config),
    Options = proplists:get_value(options, Config, ?DEFAULT_OPTIONS),
    Ret = gen_tcp:connect(Address, Port, Options),                              % 连接
    accept_or_connect_handle(Ret, State).                                       % 接受连接后的处理

%%--------------------------------------------------------------------
%% @private
%% @doc 得到socket处理
%% @end
%%--------------------------------------------------------------------
handle_call({get_state}, _From, State) ->
    {reply, State, State};

%%--------------------------------------------------------------------
%% @private
%% @doc 等待socket处理
%% @end
%%--------------------------------------------------------------------
handle_call({wait_socket, Pid}, _From, #state{socket = CommonSocket} = State) ->
    case gen_tcp:recv(CommonSocket, 0) of                                       % 等待另一端通知使用指定socket
        {ok, BinMsg} ->                                                         % 如果接受通知成功
            {use_socket, Id} = binary_to_term(BinMsg),
            {Socket, State1} = use_usable_socket(Pid, Id, State),               % 使用指定socket
            {reply, {ok, Socket}, State1};                                      % 回复socket
        ErrorReason ->                                                          % 报错
            {reply, ErrorReason, State}                                         % 回复报错
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc 得到socket处理
%% @end
%%--------------------------------------------------------------------
handle_call({get_socket, Pid}, _From, 
            #state{socket = CommonSocket, usable_socket_list = List} = State) ->
    {Id, _} = hd(List),                                                         % 取一个可用socket
    BinMsg = term_to_binary({use_socket, Id}),
    case gen_tcp:send(CommonSocket, BinMsg) of                                  % 把可用socket编号通知分布的另一端
        ok ->                                                                   % 如果通知成功
            {Socket, State1} = use_usable_socket(Pid, Id, State),               % 使用指定socket
            {reply, {ok, Socket}, State1};                                      % 回复socket
        ErrorReason ->                                                          % 报错
            self() ! accept_or_connect,                                         % 重新接受新连接(断线重连处理)
            {reply, ErrorReason, State}                                         % 回复报错
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc socket分布特殊情况下, 如果没有可用的socket, 创建可用socket处理
%% @end
%%--------------------------------------------------------------------
handle_cast({if_no_usable_socket_then_create}, 
            #state{usable_socket_list = List} = State) ->
    case length(List) of
        0 ->                                                                    % 如果没有可用的socket
            State1 = create_usable_socket_list(State),                          % 创建可用socket
            {noreply, State1};
        _ ->     
            {noreply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 终止进程时的清理
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket = Socket}) ->
    case is_port(Socket) of
        true -> gen_tcp:close(Socket);
        false -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc socket分布特殊情况下, 接受连接后或连接后的处理
%% @spec accept_or_connect_handle(Ret, State) -> {noreply, NewState} | {stop, Reason, State};
%% @end
%%--------------------------------------------------------------------
accept_or_connect_handle({ok, Socket}, State) ->                                % 成功接受连接
    State1 = State#state{socket = Socket},                                      % 记录socket
    State2 = create_usable_socket_list(State1),                                 % 创建可用socket
    {noreply, State2}.

%%--------------------------------------------------------------------
%% @private
%% @doc 创建可用socket
%% @spec create_usable_socket_list(State) -> NewState
%% @end
%%--------------------------------------------------------------------
create_usable_socket_list(State) ->
    List = [consult_create_usable_socket(State) || 
            _Index <- lists:seq(1, ?USABLE_SOCKET_NUM)],                        % 得到可用socket列表    
    _State1 = State#state{usable_socket_list = List}.                           % 更新状态

%%--------------------------------------------------------------------
%% @private
%% @doc 协商创建可用socket
%% @spec consult_create_usable_socket(State) -> {Id, Socket}
%% @end
%%--------------------------------------------------------------------
consult_create_usable_socket(#state{type = accept, socket = CommonSocket, 
                                    listen = Listen}) -> 
    {ok, BinMsg1} = gen_tcp:recv(CommonSocket, 0),  
    {ready_accept, Id} = binary_to_term(BinMsg1),

    BinMsg2 = term_to_binary({ready_connect, Id}),
    ok = gen_tcp:send(CommonSocket, BinMsg2), 

    {ok, Socket} = gen_tcp:accept(Listen),                                      % 接受连接
    {Id, Socket};

consult_create_usable_socket(#state{type = connect, socket = CommonSocket, 
                                    config = Config}) -> 
    Id = now(),                                                                 % 时间戳不会重复作为编号即可
    BinMsg1 = term_to_binary({ready_accept, Id}),
    ok = gen_tcp:send(CommonSocket, BinMsg1), 

    {ok, BinMsg2} = gen_tcp:recv(CommonSocket, 0),  
    {ready_connect, Id} = binary_to_term(BinMsg2),

    Address = proplists:get_value(address, Config),
    Port = proplists:get_value(port, Config),
    Options = proplists:get_value(options, Config, ?DEFAULT_OPTIONS),
    {ok, Socket} = gen_tcp:connect(Address, Port, Options),                     % 连接
    {Id, Socket}.

%%--------------------------------------------------------------------
%% @private
%% @doc 使用指定socket
%% @spec use_usable_socket(Pid, Id, State) -> {Socket, NewState}.
%% @end
%%--------------------------------------------------------------------
use_usable_socket(Pid, Id, #state{usable_socket_list = List} = State) -> 
    {[[{Id, Socket}]], List1} = proplists:split(List, [Id]),                    % 根据id从列表移除
    %ok = gen_tcp:controlling_process(Socket, Pid),                              % 安排指定进程为控制进程
    gen_tcp:controlling_process(Socket, Pid),                                   % 安排指定进程为控制进程(由于断网有时返回{error, badarg})
    ok = inet:setopts(Socket, [{active, once}]),                                % 用handle_info/2收数据
    State1 = State#state{usable_socket_list = List1},                           % 更新状态
    cast_if_no_usable_socket_then_create(),                                     % 通知自己如没有可用socket则创建一批新的socket
    {Socket, State1}.



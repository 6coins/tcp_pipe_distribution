%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @doc 此模块用于特殊socket分布处理(即应用是分布，并且两端都是接收连接或连接)
%%% @end
%%%-------------------------------------------------------------------

-module(tp_socket_dist).

-behaviour(gen_server).

%% API
-export([start_link/0,
         wait_socket/1,
         get_socket/1,
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
                config
               }).

%% 默认tcp选项
-define(DEFAULT_OPTIONS, [binary, 
                           {reuseaddr, true},
                           {active, once},
                           {packet, 4}                  % 防止把大的数据包拆成小数据包发送
                          ]).

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
    {ok, State}.

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
handle_call({wait_socket, Pid}, _From, #state{listen = Listen} = State) ->
    Ret = gen_tcp:accept(Listen),                                               % 等待接受连接
    case Ret of
        {ok, Socket} ->                                                         % 如果成功接受连接
            ok = gen_tcp:controlling_process(Socket, Pid);                      % 将控制进程安排给指定进程
        _ ->
            nothing
    end,
    {reply, Ret, State};

%%--------------------------------------------------------------------
%% @private
%% @doc 得到socket处理
%% @end
%%--------------------------------------------------------------------
handle_call({get_socket, Pid}, _From, #state{config = Config} = State) ->
    Address = proplists:get_value(address, Config),
    Port = proplists:get_value(port, Config),
    Options = proplists:get_value(options, Config, ?DEFAULT_OPTIONS),
    Ret = gen_tcp:connect(Address, Port, Options),                              % 连接
    case Ret of
        {ok, Socket} ->                                                         % 如果成功连接
            ok = gen_tcp:controlling_process(Socket, Pid);                      % 将控制进程安排给指定进程
        _ ->
            nothing
    end,
    {reply, Ret, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc (暂时不关注)
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc (暂时不关注)
%% @end
%%--------------------------------------------------------------------
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc 终止进程时的处理
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
%%% 内部函数
%%%===================================================================


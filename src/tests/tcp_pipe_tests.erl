%%%-------------------------------------------------------------------
%%% @doc 测试1端接受连接2端连接的模块(make a_c_pipe_tests即可运行)
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_pipe_tests).

%-include_lib("eunit/include/eunit.hrl").

-export([server_two/0, server_one/0]).
%-compile(export_all).

-define(COMMON_OPTIONS, [{reuseaddr, true}]).    % 另一端连接的端口

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% 接收应用2的2端的连接, 收a发b(用于分布测试)
server_two() ->
    TwoPort = get_config_file_port(two, "config/dist_tcp_pipe_tests2.config"),
    simulate_accpet(TwoPort, fun receive_a_send_b/1).

%% 连接应用1的1端, 发a收b(用于分布测试)
server_one() ->
    OnePort = get_config_file_port(one, "config/dist_tcp_pipe_tests1.config"),
    %{ok, _} = simulate_connect(OnePort, fun wait_receive/1).
    simulate_connect(OnePort, fun send_a_receive_b/1).

%%%===================================================================
%%% 内部函数
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc 模拟等待接受pipe的另一端连接
%% @end
%%--------------------------------------------------------------------
simulate_accpet(ListenPort, ReceiveSendFun) ->
    Options = ?COMMON_OPTIONS,
    {ok, ListenSocket} = gen_tcp:listen(ListenPort, Options),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    ReceiveSendFun(Socket).

%%--------------------------------------------------------------------
%% @doc 模拟连接pipe的一端
%% @end
%%--------------------------------------------------------------------
simulate_connect(Port, ReceiveSendFun) ->
    {ok, Socket} = simulate_connect(Port),
    ReceiveSendFun(Socket).

simulate_connect(Port) ->
    Address = {127, 0, 0, 1},
    Options = ?COMMON_OPTIONS,
    {ok, _Socket} = gen_tcp:connect(Address, Port, Options).

%% 收"a"发"b"
receive_a_send_b(Socket) ->
    receive
        {tcp, Socket, "a"} -> 
            io:format("receive a ~n"),
            Ret = gen_tcp:send(Socket, "b"),
            io:format("send b ~p~n", [Ret]),
            receive_a_send_b(Socket);
        {tcp_closed, _} -> 
            ok;
        Date ->
            error(Date) 
    end.

%% 发"a"收"b"
send_a_receive_b(Socket) ->
    Ret = gen_tcp:send(Socket, "a"), 
    io:format("send a ~p~n", [Ret]),
    receive
        {tcp, Socket, "b"} -> 
            io:format("receive b ~n"),
            wait_receive(Socket);
        Date ->
            error(Date) 
    end.

%% 等待接收数据
wait_receive(Socket) ->
    receive
         Msg-> 
            io:format("receive ~p ~n", [Msg]),
            wait_receive(Socket)
    end.

%% 得到配置文件中的端口
get_config_file_port(Side, File) ->
    {ok, [[{_App, Config}]]} = file:consult(File),
    SideConfig = proplists:get_value(Side, Config),
    proplists:get_value(port, SideConfig).


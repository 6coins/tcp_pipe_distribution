%%%-------------------------------------------------------------------
%%% @doc http代理的子管子
%%% @end
%%%-------------------------------------------------------------------
-module(http_proxy).

-export([handle/2]).

-include("pipe_recode.hrl").

-define(HTTP_METHOD_HEADER, [
            <<"GE">>,   % GET
            <<"PO">>,   % POST
            <<"HE">>,   % HEAD
            <<"PU">>,   % PUT
            <<"DE">>,   % DELETE
            <<"TR">>,   % TRACE
            <<"CO">>,   % CONNECT
            <<"OP">>    % OPTIONS
        ]).

%%%===================================================================
%%% 接口
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc 子管子处理
%% @spec handle(Event, In) -> {ok, Out} |{error, http_proxy, Reason}
%%          Event = atom()
%%          In = Out = term()
%%          Reason = term()
%% @end
%%-------------------------------------------------------------------
handle(recive, In) ->                                                    % 让子管子们进行接收数据的处理
    {ok, In};

handle(send, {<<Header:2/binary, _Rest/binary>> = Request, State}) ->    % 让子管子们进行发送数据的处理
    IsHttpNew = lists:member(Header, ?HTTP_METHOD_HEADER),                      % 是不是新的http
    request_handle(IsHttpNew, Request, State);
   
handle(_Event, In) ->                                                    % 其它事件忽略
    {ok, In}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc 请求处理
%% @spec request_handle(IsHttpNew, InRequest, InState) 
%%          -> {ok, {OutRequest, OutState}} | {error, Module, Reason}
%%          IsHttpNew = boolean()
%%          InRequest = OutRequest = binary()
%%          InState = OutState = term()
%% @end
%%-------------------------------------------------------------------
request_handle(true, InRequest, InState) ->                                     % 新请求处理
    try
        {ok, {Address, Port}, OutRequest} = parse_new_request(InRequest),       % 解析新请求
        {ok, OutState} = connect_and_update_state(Address, Port, InState),      % 连接得到socket并更新到状态中
        {ok, {OutRequest, OutState}}
    catch _Error : Reason -> 
        {error, ?MODULE, Reason}
    end;
request_handle(false, InRequest, InState) ->                                    % 非新请求处理
    {ok, {InRequest, InState}}.

%%-------------------------------------------------------------------
%% @private
%% @doc 解析新请求
%% @spec parse_new_request(NewRequest) 
%%          -> {ok, {Address, Port}, OutRequest} | {error, Reason}
%% @end
%%-------------------------------------------------------------------
parse_new_request(NewRequest) ->
    {ok, {{Method, Uri, Version}, RequestHeader}} 
    = split_new_request(NewRequest),                                        % 把新请求分解成有用的字段
    {ok, {Address, Port}} = get_address_port(Uri),                          % 从请求地址中得到地址和端口
    {ok, NormalizedRequest} 
    = normalize_reqeust(Method, Uri, Version, RequestHeader),               % 把请求头转化成平常的请求
    {ok, {Address, Port}, NormalizedRequest}.

%%-------------------------------------------------------------------
%% @private
%% @doc 把新请求分解成有用的字段
%% @spec split_new_request(NewRequest) 
%%          -> {ok, {{Method, Uri, Version}, RequestHeader}} | {error, Reason}
%%          Method = Uri = Version = RequestHeader = binary()
%%          Reason = term()
%% @end
%%-------------------------------------------------------------------
split_new_request(NewRequest) ->
    [MethodUriVersion, RequestHeader] = binary:split(NewRequest, <<"\r\n">>),
    [Method, Uri, Version] = binary:split(MethodUriVersion, <<" ">>, [global]),
    {ok, {{Method, Uri, Version}, RequestHeader}}.

%%-------------------------------------------------------------------
%% @private
%% @doc 根据请求地址得到地址和端口
%% @spec get_address_port(Uri) -> {ok, {Address, Port}} | {error, Reason}
%%          Uri = binary()
%%          Address = string()
%%          Port = integer()
%%          Reason = term()
%% @end
%%-------------------------------------------------------------------
get_address_port(Uri) -> 
    case http_uri:parse(binary_to_list(Uri)) of
        {_Sheme, _UserInfo, Host, Port, _Path, _Query} ->                   % R14B的返回结果
            {ok, {Host, Port}};
        {_Sheme, _UserInfo, Host, Port, _Path, _Query, _Fragment} ->        % R14B的返回结果
            {ok, {Host, Port}};
        {ok, {_Sheme, _UserInfo, Host, Port, _Path, _Query}} ->             % R15B的返回结果
            {ok, {Host, Port}};
        {ok, {_Sheme, _UserInfo, Host, Port, _Path, _Query, _Fragment}} ->  % R15B的返回结果
            {ok, {Host, Port}};
        {error, Reason} ->
            {error, Reason}
    end.

%%-------------------------------------------------------------------
%% @private
%% @doc 连接得到socket并更新到状态中
%% @spec connect_and_update_state(Address, Port, InState) -> {ok, OutState} | {error, Reason}
%%          Address = string()
%%          Port = integer()
%%          InState = OutState = term()
%%          Reason = term()
%% @end
%%-------------------------------------------------------------------
connect_and_update_state(Address, Port, #state{config = Config} = InState) -> 
    Options = proplists:get_value(options, Config),
    Timeout = proplists:get_value(timeout, Config),
    case gen_tcp:connect(Address, Port, Options, Timeout) of   % 连接
        {ok, Socket} ->
            OutState = InState#state{socket = Socket},          % 更新状态
            {ok, OutState};
        ErrorReason ->
            ErrorReason 
    end.

%%-------------------------------------------------------------------
%% @private
%% @doc 把请求头转化成平常的请求
%% @spec normalize_reqeust(Method, Uri, Version, RequestHeader) 
%%          -> {ok, NormalizedRequest} | {error, {split_failure, UriSplited}}
%%          Method = Uri = Version = RequestHeader = NormalizedRequest = binary()
%% @end
%%-------------------------------------------------------------------
normalize_reqeust(Method, Uri, Version, RequestHeader) ->
    UriSplited = binary:split(Uri, <<"/">>, [global]),
    case UriSplited of
        [_, _, _] ->
            Normalized = <<Method/binary, <<" ">>/binary, <<"/">>/binary, 
                           <<" ">>/binary,  Version/binary, <<"\r\n">>/binary, 
                           RequestHeader/binary>>,
            {ok, Normalized};
        [_, _, _ | Paths] ->
            Path =  lists:foldr(
                    fun(Item, Acc) -> <<Item/binary, Acc/binary>> end,
                    <<>>,
                    lists:map(
                        fun(Item) -> << <<"/">>/binary, Item/binary >> end, 
                        Paths)),
            Normalized = <<Method/binary, <<" ">>/binary, Path/binary, 
                           <<" ">>/binary,  Version/binary, <<"\r\n">>/binary,
                           RequestHeader/binary >>,
            {ok, Normalized};
        _ ->
            {error, {split_failure, UriSplited}}
    end.


%%%-------------------------------------------------------------------
%%% @doc 测试模块
%%% @end
%%%-------------------------------------------------------------------
-module(pipe_util_tests).

-include_lib("eunit/include/eunit.hrl").

-include("pipe_recode.hrl").

%-compile(export_all).

%%%===================================================================
%%% 测试函数
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 测试
%% @end
%%--------------------------------------------------------------------

get_the_other_side_test() -> 
    two = pipe_util:get_the_other_side(one),
    one = pipe_util:get_the_other_side(two).

get_side_sup_name_test() -> 
    tp_one_sup = pipe_util:get_side_sup_name(one),
    tp_two_sup = pipe_util:get_side_sup_name(two).

sub_pipes_handle_test() ->
    In = [],                                                                    % 处理前列表
    Event = add_self,                                                           % 处理的事件
    % 子管子处理成功
    SubPipes1 = [sub_pipe1, sub_pipe2],                                         % 处理的子管子列表(这两个管子会把自己名填在列表中)
    {ok, Out} = pipe_util:sub_pipes_handle(Event, SubPipes1, In),               % 断言子管子处理成功
    [sub_pipe1, sub_pipe2] = Out,                                               % 断言处理结果
    % 子管子处理报错
    SubPipes2 = [sub_pipe1, error_sub_pipe, sub_pipe2],                         % 处理的子管子列表(中间有一个报错的管子)
    {error, SubPipe, Reason} = pipe_util:sub_pipes_handle(Event, SubPipes2, In),% 断言子管子处理报错
    SubPipe = error_sub_pipe,                                                   % 断言报错的子管子名
    Reason = {reason},                                                          % 断言报错的原因       
    % 子管子未知事件处理
    {ok, []} = pipe_util:sub_pipes_handle(unkwon_event, SubPipes1, In).

    
%%%===================================================================
%%% 内部函数
%%%===================================================================


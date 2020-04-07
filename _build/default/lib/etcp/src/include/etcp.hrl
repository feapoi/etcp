%%%-------------------------------------------------------------------
%%% @author zyh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 四月 2020 11:20
%%%-------------------------------------------------------------------
-author("zyh").
-define(LAGER_LOG(__Level,__Msg),LAGER_LOG(__Level,__Msg,[])).

-define(LAGER_LOG(__Level,__Msg,__Args),lager:__Level("~w "++__Msg,[etcp:process_name(self())|__Args])).

-define(DEBUG(__Msg),?DEBUG(__Msg,[])).
-define(DEBUG(__Msg,__Args),?LAGER_LOG(debug,__Msg,__Args)).
%% 同DEBUG普通信息输出
-define(INFO(__Msg),?INFO(__Msg,[])).
-define(INFO(__Msg,__Args),?LAGER_LOG(info,__Msg,__Args)).
%% 非法操作
-define(WARNING(__Msg),?WARNING(__Msg,[])).
-define(WARNING(__Msg,__Args),?LAGER_LOG(warning,__Msg,__Args)).
%% 报错 未处理消息
-define(ERROR(__Msg),?ERROR(__Msg,[])).
-define(ERROR(__Msg,__Args),?LAGER_LOG(error,__Msg,__Args)).
%% 核心进程不响应 数据库延迟
-define(CRITICAL(__Msg),?CRITICAL(__Msg,[])).
-define(CRITICAL(__Msg,__Args),?LAGER_LOG(critical,__Msg,__Args)).


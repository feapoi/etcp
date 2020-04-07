%%%-------------------------------------------------------------------
%%% @author zyh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 四月 2020 11:01
%%%-------------------------------------------------------------------
-module(etcp_app).
-author("zyh").

-behaviour(application).

%% Application callbacks
-export([start/2,
    stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    case etcp_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

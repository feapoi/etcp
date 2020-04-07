%%%-------------------------------------------------------------------
%%% @author zyh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 四月 2020 11:37
%%%-------------------------------------------------------------------
-module(etcp).
-author("zyh").

-include("./include/etcp.hrl").

%% API
-export([start/0, open/5, process_name/1, format/1]).
start() ->
    {ok, _} = application:ensure_all_started(etcp), ok.

open(Proto, Port, Opts, SupName, ChildName) ->
    supervisor:start_child(etcp_sup, child_spec(Proto, Port, Opts, SupName, ChildName)).

child_spec(Proto, Port, SockOpts, SupName, ChildName) when is_atom(Proto) ->

    TuneFun = buffer_tune_fun(SockOpts),
    #{id => child_id(Proto, Port),
        start => {etcp_listener, start_link, [{Port, SockOpts, SupName, ChildName, TuneFun}]},
        restart => transient,
        shutdown => infinity,
        type => worker,
        modules => [etcp_listener]}.

child_id(Proto, ListenOn) ->
    {listener_sup, {Proto, ListenOn}}.

process_name(Pid)->
    case process_info(Pid,registered_name) of
        {registered_name,Name}->
            Name;
        _->
            Pid
    end.

format({Addr, Port}) ->
    inet:ntoa(Addr) ++ ":" ++ integer_to_list(Port).

buffer_tune_fun(Opts) ->
    buffer_tune_fun(proplists:get_value(buffer, Opts),
        proplists:get_bool(tune_buffer, Opts)).

%% when 'buffer' is undefined, and 'tune_buffer' is enabled...
buffer_tune_fun(undefined, true) ->
    fun(Sock) ->
        case inet:getopts(Sock, [sndbuf, recbuf, buffer]) of
            {ok, BufSizes} ->
                BufSz = lists:max([Sz || {_Opt, Sz} <- BufSizes]),
                inet:setopts(Sock, [{buffer, BufSz}]),
                {ok, Sock};
            Error -> Error
        end
    end;
buffer_tune_fun(_, _) ->
    fun(Sock) -> {ok, Sock} end.


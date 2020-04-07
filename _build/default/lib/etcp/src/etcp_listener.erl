%%%-------------------------------------------------------------------
%%% @author zyh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 四月 2020 15:15
%%%-------------------------------------------------------------------
-module(etcp_listener).
-author("zyh").

-behaviour(gen_statem).

-include("./include/etcp.hrl").

%% API
-export([start_link/1]).

%% state callbacks
-export([ accepting/3
    , suspending/3
]).

%% gen_statem callbacks
-export([
    init/1,
    terminate/3,
    code_change/4,
    callback_mode/0
]).

-define(SERVER, ?MODULE).

-record(state, {
    lsock
    ,sup_name     :: module()
    ,child_name   :: module()
    ,sockmod      :: module()
    ,sockname     :: {inet:ip_address(), inet:port_number()}
    ,tune_fun     :: esockd:sock_fun()
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link({Port, SockOpts, SupName, ChildName, TuneFun}) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, {Port, SockOpts, SupName, ChildName, TuneFun}, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
init({Port, SockOpts, SupName, ChildName, TuneFun}) ->
    case gen_tcp:listen(Port, [{active, false} | proplists:delete(active, SockOpts)]) of
        {ok, LSock} ->
            {ok, Sockname} = inet:sockname(LSock),
            {ok, SockMod} = inet_db:lookup_socket(LSock),
            {ok, accepting, #state{
                lsock = LSock
                ,sup_name = SupName
                ,tune_fun = TuneFun
                ,sockname = Sockname
                ,sockmod = SockMod
                ,child_name = ChildName
            }, {next_event, internal, accept}};
        {error, Reason} ->
            {stop, Reason, #state{}}
    end.

callback_mode() ->
    state_functions.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% state function
%%%===================================================================

accepting(internal, accept, State = #state{lsock = LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, _Ref} ->
            {keep_state, State};
        {error, Reason} when Reason =:= emfile;
            Reason =:= enfile ->
            {next_state, suspending, State, 1000};
        {error, closed} ->
            {stop, normal, State};
        {error, Reason} ->
            {stop, Reason, State}
    end;

accepting(info, {inet_async, LSock, _Ref, {ok, Sock}},
    State = #state{lsock        = LSock,
        tune_fun     = TuneFun,
        sup_name =  SupName,
        sockmod = SockMod,
        sockname = SockName,
        child_name = ChildName
        }) ->
    %% make it look like gen_tcp:accept
    inet_db:register_socket(Sock, SockMod),
    case TuneFun(Sock) of
        {ok, Sock} ->
            case gen_server:call(SupName, {start_child, ChildName, Sock}) of
                {ok, Pid} ->
                    case gen_tcp:controlling_process(Sock, Pid) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            ?ERROR("Failed to controlling process on ~s: ~p",
                                [etcp:format(SockName), Reason]),
                            close(Sock)
                    end;
                {error, Reason} ->
                    ?ERROR("Failed to start connection on ~s: ~p",
                        [etcp:format(SockName), Reason]),
                    close(Sock)
            end;
        {error, enotconn} ->
            close(Sock);
        {error, einval} ->
            close(Sock);
        {error, closed} ->
            close(Sock);
        {error, Reason} ->
            ?ERROR("Tune buffer failed on ~s: ~s",
                [etcp:format(SockName), Reason]),
            close(Sock)
    end,
    {keep_state, State, {next_event, internal, accept}};

accepting(info, {inet_async, LSock, _Ref, {error, closed}},
    State = #state{lsock = LSock}) ->
    {stop, normal, State};

%% {error, econnaborted} -> accept
%% {error, esslaccept}   -> accept
accepting(info, {inet_async, LSock, _Ref, {error, Reason}},
    #state{lsock = LSock})
    when Reason =:= econnaborted; Reason =:= esslaccept ->
    {keep_state_and_data, {next_event, internal, accept}};

%% emfile: The per-process limit of open file descriptors has been reached.
%% enfile: The system limit on the total number of open files has been reached.
accepting(info, {inet_async, LSock, _Ref, {error, Reason}},
    State = #state{lsock = LSock, sockname = SockName})
    when Reason =:= emfile; Reason =:= enfile ->
    ?ERROR("Accept error on ~s: ~s",
        [etcp:format(SockName), Reason]),
    {next_state, suspending, State, 1000};

accepting(info, {inet_async, LSock, _Ref, {error, Reason}},
    State = #state{lsock = LSock}) ->
    {stop, Reason, State}.

suspending(timeout, _Timeout, State) ->
    {next_state, accepting, State, {next_event, internal, accept}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================

close(Sock) -> catch port_close(Sock).


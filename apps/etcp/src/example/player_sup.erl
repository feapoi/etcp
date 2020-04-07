%%%-------------------------------------------------------------------
%%% @author zyh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 四月 2020 15:36
%%%-------------------------------------------------------------------
-module(player_sup).
-author("zyh").
-include("../include/etcp.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    Opts = [binary, {packet, raw}],
    etcp:open(player, 8111, Opts, player_sup, player),
    {ok, #state{}}.

handle_call(Call,From,State)->
    try
        do_handle_call(Call,From,State)
    catch
        E:R:S ->
            ?ERROR("handle call ~p ~p call ~p ~p",[E,R,Call,S]),
            {reply,error,State}
    end.
handle_cast(Msg,State)->
    do_handle(cast,Msg,State).
handle_info(Msg,State)->
    do_handle(info,Msg,State).
do_handle(Tag,Msg,State)->
    try
        do_handle_msg(Msg,State)
    catch
        E:R:S ->
            ?ERROR("handle ~w ~p ~p msg ~p ~p",[Tag,E,R,Msg,S]),
            {noreply,State}
    end.

do_handle_call({start_child, ChildMod, Socket}, _From, State) ->
    Reply =
        case ChildMod:start_link(Socket) of
            {ok, Pid}-> {ok, Pid};
            {error, {already_started, Pid}} -> {error, already_started};
            {error, Reason} -> {error, Reason}
        end,
    {reply, Reply, State};
do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.


do_handle_msg({'EXIT', Pid, Why}, State) ->
    case Why of
        stop -> ok;
        _ ->
            ?ERROR("process ~p exit reason ~p ~n", [Pid, Why])
    end,
    {noreply, State};
do_handle_msg(stop, State) ->
    {stop,normal,State};
do_handle_msg(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

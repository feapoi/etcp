%%%-------------------------------------------------------------------
%%% @author zyh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 四月 2020 15:36
%%%-------------------------------------------------------------------
-module(player).
-author("zyh").

-include("../include/etcp.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Socket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Socket]) ->
    ?INFO("init player", []),
    inet:setopts(Socket, [{active, once}]),
    {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Data}, State = #state{socket = Sock}) ->
    {ok, Peername} = inet:peername(Sock),
    ?INFO("Data from ~p: ~p~n", [etcp:format(Peername), Data]),
    gen_tcp:send(Sock, Data),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State};

handle_info({tcp_error, Sock, Reason}, State = #state{socket = Sock}) ->
    ?INFO("Error from: ~p~n", [Sock]),
    ?INFO("tcp_error: ~p~n", [Reason]),
    {stop, {shutdown, Reason}, State};

handle_info({tcp_closed, Sock}, State = #state{socket = Sock}) ->
    ?INFO("tcp_closed~n"),
    {stop, normal, State};

handle_info(Info, State) ->
    ?INFO("other info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

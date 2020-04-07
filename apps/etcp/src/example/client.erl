%%%-------------------------------------------------------------------
%%% @author 10621
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 四月 2020 14:32
%%%-------------------------------------------------------------------
-module(client).
-author("10621").

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

-export([connect/0]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(connect, State) ->
    {ok,Socket} = gen_tcp:connect("127.0.0.1",8111,[binary, {packet, raw}, {active, true}]),
    {noreply, State#state{socket = Socket}};
handle_info(send, #state{socket = Socket} = State) ->
    gen_tcp:send(Socket, "22222"),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
connect() ->
    ?MODULE ! connect.


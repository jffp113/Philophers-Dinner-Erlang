%%%-------------------------------------------------------------------
%%% @author jorgepereira
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. dez 2020 10:32
%%%-------------------------------------------------------------------
-module(leader).

-behaviour(gen_server).
-define(N,1000000).
%% API
-export([start_protocol/0,init/1,handle_call/3,sendOne/4,sendTwo/2]).

start_protocol() ->
  Left = random:uniform(?N),
  First = random:uniform(?N),
  start(First,Left),
  L = start_protocol(First,Left,4,[First]),
  [sendStartup(X) || X <- L ].

start_protocol(First,Left,1,P) ->
  start(Left,First),
  [Left | P];
start_protocol(First,Left,N,P) ->
  NewLeft = random:uniform(?N),
  start(Left,NewLeft),
  start_protocol(First,NewLeft,N-1,[Left | P]).

start(MyId,LeftId) ->
  gen_server:start_link({local, atom(MyId)} , ?MODULE, [MyId,LeftId],[]).

init([MyId,LeftId]) ->
  io:format("Started process ~p with Left ~p ~n",[MyId,LeftId]),
  State = {LeftId,active,0,MyId,MyId},
  {ok, State}.

sendOne(Name,I,Phase,Counter) -> gen_server:call(atom(Name), {one, {I,Phase,Counter}}).

sendTwo(Name,J) -> gen_server:call(atom(Name), {two, {J}}).

sendStartup(Name) -> gen_server:call(atom(Name), {startup}).




handle_call({startup}, _From, State) ->
  {PId,_,PhaseS,Id,Max} = State,
  sendOne(PId,Max,PhaseS,PhaseS bsl 1),
  {reply,ok,State};
handle_call({one, {I,Phase,Counter}}, _From, {PId,active,PhaseS,Id,Max}) when I > Max->
  {reply, {},{PId,waiting,PhaseS,Id,I}};
handle_call({one, {I,Phase,Counter}}, _From, {PId,active,PhaseS,Id,Max})->
  sendTwo(PId,Max),
  {reply, {},{PId,passive,PhaseS,Id,Max}};
handle_call({two, {J}}, _From, {PId,waiting,Phase,Id,Max}) when J == Max ->
  {State, Reply} = {{PId,active,Phase + 1,Id,Max}, {}},
  sendOne(PId,Max,Phase,Phase bsl 1),
  {reply, Reply,State};
handle_call({one, {I,Phase,Counter}}, _From, {PId,waiting,PhaseS,Id,Max})->
  {reply, {},{PId,active,PhaseS + 1,Id,Max}}.

atom(Name) -> list_to_atom(lists:flatten(io_lib:format("~p", [Name]))).
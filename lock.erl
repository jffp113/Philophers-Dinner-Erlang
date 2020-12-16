%%%-------------------------------------------------------------------
%%% @author jorgepereira, joanaparreira
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. dez 2020 11:13
%%%-------------------------------------------------------------------
-module(lock).
-author("jorgepereira").
-define(NUM_PHILOSOPHERS,5).

%% API
-export([start/0,init/1,handle_call/3,can_eat_msg/1,release_msg/1]).

start() ->
  gen_server:start_link({local, ?MODULE} , ?MODULE, [],[]).

init(_Args) ->
  State = {[{0,false,0},{1,false,0},{2,false,0},{3,false,0},{4,false,0}],1},
  {ok, State}.

can_eat_msg(Pos) -> gen_server:call(?MODULE, {eat,Pos}).

release_msg(Pos) -> gen_server:call(?MODULE, {release,Pos}).

handle_call({eat,Pos}, _From, State) ->
  {Result,NewState} = can_eat(Pos,State),
  {reply,Result,NewState};
handle_call({release,Pos},  _From, State) ->
  {Result,NewState} = check_release(Pos,State),
  {reply,Result,NewState}.


can_eat(Pos,{EatList,GRound}) ->
  {_,LeftState,_} = lists:keyfind(calculate_left_pos(Pos), 1, EatList),
  {_,_,Round} = lists:keyfind(Pos, 1, EatList),
  {_,RightState,_} = lists:keyfind(calculate_right_pos(Pos), 1,EatList),
  GlobalRound = current_round(GRound),
  if
    not LeftState and not RightState
      and (Round < GlobalRound) ->
      NewList = lists:keyreplace(Pos, 1, EatList, {Pos,true,Round + 1}),
      {ok,{NewList,GRound + 1}};
    true -> {error,{EatList,GRound}}
  end.


calculate_left_pos(Pos) ->
  Res =  (Pos - 1) rem ?NUM_PHILOSOPHERS,
  if
    Res < 0 -> ?NUM_PHILOSOPHERS - 1;
    true -> Res
  end.

calculate_right_pos(Pos) -> (Pos + 1) rem ?NUM_PHILOSOPHERS.


current_round(GRound) -> GRound/?NUM_PHILOSOPHERS.

check_release(Pos,{EatList,GRound}) ->
  {_,_,Round} = lists:keyfind(Pos, 1, EatList),
  NewList = lists:keyreplace(Pos, 1, EatList, {Pos,false,Round}),
  {ok,{NewList,GRound}}.







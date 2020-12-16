%%%-------------------------------------------------------------------
%%% @author jorgepereira, joanaparreira
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Implementation of a global lock. This is s smart global lock, which
%%% prevents Philosophers from entering the critical region (Eating) if
%%% they don't have available both chop sticks.
%%% Philosophers can ask for the right to eat (get both chop sticks) by
%%% sending a message to the server (can_eat_msg). When they finish eating
%%% they must send a msg to the server to communicate the intent of releasing
%%% both sticks.
%%% @end
%%% Created : 16. dez 2020 11:13
%%%-------------------------------------------------------------------
-module(lock).
-author("jorgepereira").
-define(NUM_PHILOSOPHERS,5).

%% API
-export([start/0,init/1,handle_call/3,can_eat_msg/1,release_msg/1]).

%start/0 starts the lock server responsible to give authorization
% to eat
start() ->
  gen_server:start_link({local, ?MODULE} , ?MODULE, [],[]).

%init/0 initiates the OTP (gen_server with the initial state)
% Every Philosopher initially is not eating.
% Philosopher State = {ID,Is_eating,Round}
init(_Args) ->
  State = {[{0,false,0},{1,false,0},{2,false,0},{3,false,0},{4,false,0}],1},
  {ok, State}.

% can_eat_msg/1 sends a message to the lock asking for
% permission to eat
% responses : ok -> if the philosopher can start eating
%                   Philosopher state will change from
%                   {_,false,_} -> {_,true,_}
%             error -> if the philosopher does not
%                      have the rights to eat
can_eat_msg(Pos) -> gen_server:call(?MODULE, {eat,Pos}).

%release_msg/1 sends a message to the Lock (otp server)
% to release the rights of eating
release_msg(Pos) -> gen_server:call(?MODULE, {release,Pos}).

%handle_call/1 is called when a msg for the server arrives
% if msg contains atom eat, it will be processed as a ask for
% eating
% if the msg contains release, it will be processed as a ask for
% eating privileges revoke
handle_call({eat,Pos}, _From, State) ->
  {Result,NewState} = can_eat(Pos,State),
  {reply,Result,NewState};
handle_call({release,Pos},  _From, State) ->
  {Result,NewState} = check_release(Pos,State),
  {reply,Result,NewState}.

%can_eat/2 is responsible for checking if a philosopher with ID 'Pos'
% can eat. This is done in an atomic fashion without the need for
% chop sticks notion.
% can_eat/2 verifies if both the Philosopher (Pos) neighbours are eating.
% If know of them is eating, the Philosopher (Pos) can start eating if he
% is on his round, receiving a msg with ok.
% Otherwise receives error.
% To prevent a philosopher from starving others there is a notion of rounds
% if a philosopher is on is round he can eat, otherwise receives a error msg.
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

%calculate_left_pos/1 returns the Id of the left
%philosopher
calculate_left_pos(Pos) ->
  Res =  (Pos - 1) rem ?NUM_PHILOSOPHERS,
  if
    Res < 0 -> ?NUM_PHILOSOPHERS - 1;
    true -> Res
  end.

%calculate_right_pos/1 returns the Id of the right
%philosopher
calculate_right_pos(Pos) -> (Pos + 1) rem ?NUM_PHILOSOPHERS.

% current_round/1 computes the current round
current_round(GRound) -> GRound/?NUM_PHILOSOPHERS.

% check_release/1 releases the lock os a philosopher
% returning the new state.
% {_,true,_} -> {_,false,_}
check_release(Pos,{EatList,GRound}) ->
  {_,_,Round} = lists:keyfind(Pos, 1, EatList),
  NewList = lists:keyreplace(Pos, 1, EatList, {Pos,false,Round}),
  {ok,{NewList,GRound}}.





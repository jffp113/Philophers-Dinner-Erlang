%%%-------------------------------------------------------------------
%%% @author jorgepereira, joanaparreira
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% This file contains the implementation of a philosopher
%%% Philosophers have two states. They can be eating or sleeping
%%% for a random time.
%%% Philosophers before changing from a thinking state to
%%% a eating state they must contact a smart lock that knows
%%% if a philosopher can eating.
%%% @end
%%% Created : 16. dez 2020 12:24
%%%-------------------------------------------------------------------
-module(philosopher).
-author("jorgepereira").

%% API
-export([start/0, run_philosopher/1, supervise/1]).
-define(NUM_PHILOSOPHERS,5).
-define(MAX_EAT, 3000).
-define(MAX_THINK, 1000).

%start/0 is responsible to initiate the smart lock
%server and to initiate the philosophers with a
%superviser
start() ->
  lock:start(),
  process_flag(trap_exit, true),
  List = spawn_philosophers(?NUM_PHILOSOPHERS,[]),
%  spawn(?MODULE, sleep_kill, [List]),
  supervise(List).

%sleep_kill(List) ->
%  timer:sleep(5000),
%  [{Pid,Id}|_] = List,
%  exit(Pid,badarg),
%  io:format("parou ~p~n", [Id]).

%spawn_philosophers is responsible for spawning
% the philosophers and returning a list mapping the
% id of a philosopher to the corresponding Pid[{Pid,Id}|T]
spawn_philosophers(0,List) -> List;
spawn_philosophers(Num,List) ->
  LocalId = Num - 1,
  Pid = spawn_link(?MODULE, run_philosopher, [LocalId]),
  spawn_philosophers(Num-1,[{Pid,LocalId} | List]).

simulate_thinking() ->
  Time = rand:uniform(?MAX_THINK),
  timer:sleep(Time).

simulate_eating(Id) ->
  io:format("Started eating ~p~n",[Id]),
  Time = rand:uniform(?MAX_EAT),
  timer:sleep(Time),
  io:format("Stop Eating ~p~n",[Id]).

run_philosopher(Id) ->
  simulate_thinking(),
  try_until_can_eat(Id),
  run_philosopher(Id).

try_until_can_eat(Id) ->
  Result = lock:can_eat_msg(Id),
  case Result of
    ok ->
      simulate_eating(Id),
      lock:release_msg(Id);
    error -> try_until_can_eat(Id)
  end.

% supervise/1 watches over the process that runs
% the bounded buffer and, if the process crashes
% the supervise will restart a new bounded buffer
% supervise/1 receives the Size of the buffer to
% be restarted
supervise(PidList) ->
  receive
    {'EXIT', Pid, Reason} ->
      io:format("Died ~p~n", [Reason]),
      {_,ID} = lists:keyfind(Pid, 1, PidList),
      cleanup(ID),
      NewList = respawn_philosopher(Pid,PidList, ID),
      supervise(NewList)
  end.

cleanup(Id) -> lock:release_msg(Id).

respawn_philosopher(Pid,PidList, ID) ->
  NewPID = spawn_link(?MODULE, run_philosopher, [ID]),
  NewList = lists:keyreplace(Pid, 1, PidList, {NewPID,ID}),
  NewList.
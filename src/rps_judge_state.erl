-module(rps_judge_state).





-export([new/1, get_user/2, get_user_pid/2, get_pid/1, get_user_nick/2, get_user_answer/2, get_answer/1, is_complete/1, set_user_answer/3, winner/1]).





% <-> SPEC RPS_JUDGE_STATE -> PROPLIST -> [{Pid, {Nick , Answer}}]





new(UserList) -> 
	lists:map(
		fun(X) ->
			{Pid, Nick} = X,
			user(Pid, Nick, undefined)
		end,
		UserList
	).



user(Pid, Nick, Answer) -> {Pid, {Nick, Answer}}.



get_user(State, Pid) when is_pid(Pid) == true ->
	case State of
		[A = {Pid, _}, _] -> A;
		[_, A = {Pid, _}] -> A;
		_ -> 'ERROR'
	end;

get_user(State, Nick) when is_binary(Nick) == true ->
	case State of
		[A = {_, {Nick, _}}, _] -> A;
		[_, A = {_, {Nick, _}}] -> A;
		_ -> 'ERROR'
	end.

get_user_pid(State, Nick) -> get_user_parameter(get_user(State, Nick), pid).

get_pid(User) -> get_user_parameter(User, pid).

get_user_nick(State, Pid) -> get_user_parameter(get_user(State, Pid), nick).

get_user_answer(State, Param) -> get_user_parameter(get_user(State, Param), answer).

get_answer(User) -> get_user_parameter(User, answer).

get_user_parameter(User, Parameter) ->
 {Pid, {Nick, Answer}} = User,
 case Parameter of
 	nick -> Nick;
 	pid -> Pid;
 	answer -> Answer
 end.



is_complete(State) ->
	[{_,{_, A1}},{_, {_, A2}}] = State,
	if 
		(A1 == undefined) or (A2 == undefined) -> false;
		true -> true
	end.		

winner(State) ->
	[User1, User2] = State,
	User1Answer = get_answer(User1),
	User2Answer = get_answer(User2),
	case who_win(User1Answer, User2Answer) of
		first -> [{winner, User1}, {loser, User2}];
		second -> [{loser, User1}, {winner, User2}];
		stalemate -> [{draw, User1}, {draw, User2}]
	end.

who_win(rock, rock) -> stalemate;
who_win(rock, paper) -> second;
who_win(rock, scissors) -> first;

who_win(paper, paper) -> stalemate;
who_win(paper, rock) -> first;
who_win(paper, scissors) -> second;

who_win(scissors, scissors) -> stalemate;
who_win(scissors, paper) -> first;
who_win(scissors, rock) -> second.


set_answer(User, Answer) ->
	{Pid, {Nick, _}} = User,
	user(Pid, Nick, Answer).

set_user_answer(State, UserParam, Answer) ->
	[U1, U2] = State,
	case get_user(State, UserParam) of
		U1 ->
			NewU1 = set_answer(U1, Answer),
			[NewU1, U2];
		U2 ->
			NewU2 = set_answer(U2, Answer),
			[U1, NewU2]
	end.

-module(rps_queue_state).





-include("rps_globals.hrl").





% MEMBER DATA CREATE WRAPS
-export([users_idle/1, users_playing/1, user/2]).

% OBJECT CREATION AND MANIPULATION
-export([new/2, default/0, find_user/2, register_user/2, unregister_user/2, move_user/3, client_exists/2, client_idle_exists/2, client_playing_exists/2]).

% MEMBER DATA GETTERS
-export([get_users_idle/1, get_users_playing/1, get_all_users/1]).





% <-> SPEC RPS_QUEUE_STATE -> PROPLIST -> [users_idle, users_playing] users_idle {ui, [{Pid, Nick}]}, users_plaing {up, [{Pid, Nick}]}




% -> API

% -> MEMBER DATA CREATE WRAPS

users_idle(Users) when is_list(Users) -> {ui, Users}.

users_playing(Users) when is_list(Users) -> {up, Users}.	

user(Pid, Nick) -> {Pid, Nick}.

% <- MEMBER DATA CREATE WRAPS





% -> OBJECT CREATION AND MANIPULATION

new(NewState, []) -> NewState;

new(State, [Parameter | Rest]) -> new(replace(State, Parameter), Rest);

new(State, Parameter) -> replace(State, Parameter).

default() -> create([], []).

find_user(UserName, State) ->
	Users = get_all_users(State),
	lists:keyfind(UserName, 2, Users).

register_user(User, State) -> add_user(User, ui, State).

move_user(User, _To = ui, State) ->
	NS1 = remove_user(User, up, State),
	add_user(User, ui, NS1);

move_user(User, _To = up, State) ->
	NS1 = remove_user(User, ui, State),
	add_user(User, up, NS1).

unregister_user(User, State) -> 
	NS1 = remove_user(User, ui, State),
	remove_user(User, up, NS1).

client_exists(Name, State) ->
	Users = get_all_users(State),
	lists:any(
		fun(Elem) ->
			case Elem of
				{_, Name} -> true;
				_ -> false
			end
		end,
		Users
	).

client_idle_exists(Name, State) ->
	lists:any(
		fun(Elem) ->
			case Elem of
				{_, Name} -> true;
				_ -> false
			end
		end,
		get_users_idle(State)
	).

client_playing_exists(Name, State) ->
	lists:any(
		fun(Elem) ->
			case Elem of
				{_, Name} -> true;
				_ -> false
			end
		end,
		get_users_playing(State)
	).

% <- OBJECT CREATION AND MANIPULATION





% -> MEMBER DATA GETTERS

get_users_idle(State) -> get_parameter(ui, State).

get_users_playing(State) -> get_parameter(up, State).

get_all_users(State) -> lists:append(get_users_playing(State), get_users_idle(State)).

% <- MEMBER DATA GETTERS

% <- API





% -> PRIVATE

add_user(User, ui, State) ->
	UIL = get_users_idle(State),
	case lists:member(User, UIL) of
		true -> State;
		false -> 
			NewUIL = [User| UIL],
			new(State, users_idle(NewUIL))
	end;

add_user(User, up, State) ->
	UPL = get_users_playing(State),
	case lists:member(User, UPL) of
		true -> State;
		false -> 
			NewUPL = [User| UPL],
			new(State, users_playing(NewUPL))
	end.



remove_user(User, ui, State) ->
	UIL = get_users_idle(State),
	NewUIL = lists:delete(User, UIL),
	new(State, users_idle(NewUIL));

remove_user(User, up, State) ->
	UPL = get_users_playing(State),
	NewUPL = lists:delete(User, UPL),
	new(State, users_playing(NewUPL)).



get_parameter(Parameter, State) ->
	{Parameter, Value} = proplists:lookup(Parameter, State),
	Value.



param_name({X, _}) -> X.



param_val({_, X}) -> X.



create(UIL, UPL) -> [users_idle(UIL), users_playing(UPL)].



replace(_State = [UIL, UPL], Parameter) ->
	case param_name(Parameter) of
		ui -> create(param_val(Parameter), param_val(UPL));
		up -> create(param_val(UIL), param_val(Parameter));
		_ -> 'ERROR'
	end.

% <- PRIVATE
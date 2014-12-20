-module(rps_connect_state).





% MEMBER DATA CREATE WRAPS
-export([pipe/1, data/1, status/1, user/1, judge/1]).

% OBJECT CREATION AND MANIPULATION
-export([new/5, new/2, default/1]).

% MEMBER DATA GETTERS
-export([get_pipe/1, get_transport/1, get_socket/1, get_data/1, get_status/1, get_user/1, get_judge/1]).







% <-> SPEC  RPS_CONNECT_STATE -> PROPLIST -> [pipe, data, status, user, judge]





% -> PUBLIC

% -> MEMBER DATA CREATE WRAPS

pipe(Pipe = {_Transport, _Socket}) -> {pipe, Pipe}.

data(PartialData) when is_binary(PartialData) -> {data, PartialData}.

status(StatusName) when is_atom(StatusName) -> {state, StatusName}.

user(undefined) -> {user, undefined};

user(UserName) when is_list(UserName) -> {user, UserName};

user(UserName) when is_binary(UserName) -> {user, UserName}.

judge(JudgePid) -> {judge, JudgePid}.

% <- MEMBER DATA CREATE WRAPS





% -> OBJECT CREATION AND MANIPULATION

new(TSTuple, PartialData, StatusName, UserName, JudgePid) ->
	[
		pipe(TSTuple),
		data(PartialData),
		status(StatusName),
		user(UserName),
		judge(JudgePid)
	].

new(NewState, []) -> NewState;

new(State, [Parameter | Rest]) -> new(replace(State, Parameter), Rest);
 
new(State, Parameter) -> replace(State, Parameter).


default(TSTuple) ->
  	new(TSTuple, <<"">>, init, undefined, undefined).

% <- OBJECT CREATION AND MANIPULATION





% -> MEMBER DATA GETTERS

get_pipe(State) -> get_parameter(pipe, State).


get_transport(State) ->
	{Transport, _Socket} = get_parameter(pipe, State),
	Transport.


get_socket(State) ->
	{_Transport, Socket} = get_parameter(pipe, State),
	Socket.


get_data(State) -> get_parameter(data, State).

get_status(State) -> get_parameter(state, State).

get_user(State) -> get_parameter(user, State).

get_judge(State) -> get_parameter(judge, State).

% <- MEMBER DATA GETTERS

% <- PUBLIC





% -> PRIVATE

param_name({X, _}) -> X.

param_val({_, X}) -> X.


replace(_State = [_Pipe = {pipe, TSTuple}, _Data = {data, PartialData}, _Status = {state, StatusName}, _User = {user, UserName}, _Judge = {judge, JudgePid}], Parameter) ->
	case param_name(Parameter) of
		pipe -> new(param_val(Parameter), PartialData, StatusName, UserName, JudgePid);
		data -> new(TSTuple, param_val(Parameter), StatusName, UserName, JudgePid);
		state -> new(TSTuple, PartialData, param_val(Parameter), UserName, JudgePid);
		user -> new(TSTuple, PartialData, StatusName, param_val(Parameter), JudgePid);
		judge -> new(TSTuple, PartialData, StatusName, UserName, param_val(Parameter));
		_ -> 'ERROR'
	end.


get_parameter(Parameter, State) ->
	{Parameter, Value} = proplists:lookup(Parameter, State),
	Value.

% <- PRIVATE
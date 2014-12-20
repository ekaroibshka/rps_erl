-module(rps_queue).





-include("rps_globals.hrl").





-behaviour(gen_server).





% API (OUTER CALLS)
-export([start_link/0, register/1, unregister/2, users/0, user_says/2, play/2]).

% GEN_SERVER CALLBACKS
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).





% -> API (OUTER CALLS)

start_link() -> gen_server:start_link({local, ?MODULE}, rps_queue, [], []).

register(UserName) -> gen_server:call(?MODULE, {register_me_as, UserName}).

unregister(Pid, UserName) -> gen_server:cast(?MODULE, {unregister_me, Pid, UserName}).

users() -> gen_server:call(?MODULE, users).

user_says(UserName, What) -> gen_server:cast(?MODULE, {says, UserName, What}).

play(Pid, UserName) -> gen_server:cast(?MODULE, {active, Pid, UserName}).

% <- API (OUTER CALLS)





% -> GEN_SERVER CALLBACKS

init(_Args) ->
  io:format("Global QUEUE(~p) started!~n", [?MODULE]),
  {ok, ?QS:default()}.



handle_call(users, _From, State) -> {reply, ?QS:get_all_users(State), State};

handle_call({register_me_as, Name}, _From = {Pid, _Ref}, State) ->
  case ?QS:client_exists(Name, State) of
    false ->
      NewClient = {Pid, Name},
      {reply, ok, ?QS:register_user(NewClient, State)};
    true ->
      {reply, {error, already_exists}, State}
  end;

handle_call(dbg, _From, State) -> {reply, State, State};

handle_call(Request, From, State) ->
  io:format("Global QUEUE (~p) received call message: ~p, from ~p~n", [self(), Request, From]),
  {reply, ok, State}.



handle_cast({active, Pid, UserName}, State) ->
  User = ?QS:user(Pid, UserName),
  NewState = ?QS:move_user(User, up, State),
  case ?QS:get_users_playing(NewState) of
    [_OnlyUser] -> {noreply, NewState};
    [User1 = {Pid1, _}, User2 = {Pid2, _} | _] ->
      {ok, NewJudge} = ?J:start([User1, User2]),
      case ?C:game_starts(Pid1, {NewJudge, User2}) of
        ok ->
          case ?C:game_starts(Pid2, {NewJudge, User1}) of
            ok ->
              NewState2 = ?QS:move_user(User1, ui, NewState),
              NewState3 = ?QS:move_user(User2, ui, NewState2),
              {noreply, NewState3};
            _ ->
              {Pid, _} = User1,
              ?C:sorry(Pid, User2),
              ?J:close(NewJudge),
              {noreply, NewState}
          end;
        _ ->
          {Pid, _} = User2,
          ?C:sorry(Pid, User1),
          ?J:close(NewJudge),
          {noreply, NewState}
      end
  end;

handle_cast({unregister_me, Pid, Nick}, State) ->
  io:format("Global QUEUE (~p) received exit message from {~p, ~p}~n", [self(), Pid, Nick]),
  User = {Pid, Nick},
  {noreply, ?QS:unregister_user(User, State)};

handle_cast({says ,{_, Nick}, What}, State) ->
  lists:foreach(
    fun(X) -> 
      case X of
        {_, Nick} -> continue;
        {Pid, _} -> ?C:chat_event(Pid, Nick, What)
      end
    end,
    ?QS:get_all_users(State)),
  {noreply, State};

handle_cast(Request, State) ->
  io:format("Global QUEUE (~p) received cast message: ~p~n", [self(), Request]),
  {noreply, State}.



handle_info(Info, State) ->
  io:format("Global QUEUE (~p) received unexpected message: ~p~n", [self(), Info]),
  {noreply, State}.



terminate(Reason, State) -> io:format("Global QUEUE (~p) terminated: ~p, ~p~n",[?MODULE, Reason, State]).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% <- GEN_SERVER CALLBACKS

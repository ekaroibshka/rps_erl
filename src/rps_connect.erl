-module(rps_connect).





-behaviour(gen_server).

-behaviour(ranch_protocol).





-include("rps_globals.hrl").





% API (OUTER CALLS)
-export([start_link/4, game_starts/2, game_ended/2, chat_event/3, sorry/2]).

% GEN_SERVER CALLBACKS
-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).





% -> PUBLIC

% -> API (OUTER CALLS)

start_link(Ref, Socket, Transport, Opts) -> proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

game_starts(Client, {JudgePid, Opponent}) -> gen_server:call(Client, {game_starts, {JudgePid, Opponent}}).

game_ended(Client, GameResult) -> gen_server:cast(Client, {game_ended, GameResult}).

chat_event(Client, Who, What) -> gen_server:cast(Client, {says, {Who, What}}).

sorry(Client, DisconnectedClient) -> gen_server:cast(Client, {disconnected, DisconnectedClient}).

% <- API (OUTER CALLS)





% -> GEN_SERVER CALLBACKS

% CONSTRUCTOR
init(_Stub) -> {ok, []}.

init(Ref, Socket, Transport, _Opts = []) ->
  io:format("Client(~p) connected!~n", [self()]),
  ok = proc_lib:init_ack({ok, self()}),
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, true}]),
  Transport:send(Socket, [greet(), <<"Enter nickname: "/utf8>>]),
  gen_server:enter_loop(?MODULE, [], ?CS:default({Transport,Socket})).


handle_call(Request, From, State) -> handle_state_call(Request, From, ?CS:get_status(State), State).


handle_cast(Request, State) -> handle_state_cast(Request, ?CS:get_status(State), State).

% TCP HANG CHECK
handle_info({tcp_closed, _}, State) ->
  {stop, normal, State};

% MAIN TCP LOOP
handle_info({tcp, _Socket, Message}, State) ->
  PartialData = ?CS:get_data(State),
  case is_complete(Message) of
    true -> 
      handle_socket_info(strip_msg(<<PartialData/binary, Message/binary>>), ?CS:get_status(State), ?CS:new(State, ?CS:data(<<"">>)));
    false ->
      {noreply, ?CS:new(State, ?CS:data(<<PartialData/binary, Message/binary>>))}
  end;

% STRANGE MESSAGE HANDLER
handle_info(Info, State) ->
  io:format("Client(~p) received unexpected message: ~p~n", [self(), Info]),
  {noreply, State}.

% DESTRUCTOR
terminate(_Reason, State) ->
  ?Q:unregister(self(), ?CS:get_user(State)),
  case ?CS:get_judge(State) of
    undefined -> io:format("Client (~p) terminated: ~p~n",[self(), ?CS:get_status(State)]);
    Judge -> ?J:quit_me(Judge, ?CS:get_user(State)),
    io:format("Client (~p) terminated: ~p~n",[self(), ?CS:get_status(State)])
  end.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% <- GEN_SERVER CALLBACKS

% <- PUBLIC





% -> PRIVATE

% -> CALL EVENT HANDLERS

handle_state_call({game_starts, {JudgePid, _Opponent = {_Pid, Nick}}}, _From, game_awaiting, State) ->
  send(State, [<<"Opponent found - \""/utf8>>, Nick, <<"\"!\r\n"/utf8>> | battle_start_msg()]),
  {reply, ok, ?CS:new(State, [?CS:status(game), ?CS:judge(JudgePid)])};

handle_state_call({game_starts, _JudgePid, _Opponent}, _From, _, State) -> {reply, no, State};

handle_state_call(Request, From, _StateName, State) ->
  io:format("Client(~p) received unexpected call message: ~p, from ~p~n", [self(), Request, From]),
  {reply, ok, State}.

% <- CALL EVENT HANDLERS





% -> CAST EVENT HANDLERS

handle_state_cast({disconnected, _Who}, _StateName, State) ->
  send(State, ["Sorry, but it seems like your opponent disconnected... Returning to lobby\r\n"]),
  {noreply, ?CS:new(State, [?CS:status(idle), ?CS:judge(undefined)])};

handle_state_cast({says, {Who, What}}, _StateName, State) ->
  send(State, ["\"", Who, "\"", <<" says: "/utf8>>, What, "\r\n"]),
  {noreply, State};





handle_state_cast({game_ended, disconnected}, _, State) ->
  send(State, ["Sorry, but it seems like your opponent disconnected... Returning to lobby\r\n"]),
  {noreply, ?CS:new(State, [?CS:status(idle), ?CS:judge(undefined)])};

handle_state_cast({game_ended, draw}, _, State) ->
  send(State, ["Draw! No one is winner...\r\n"]),
  {noreply, ?CS:new(State, [?CS:status(idle), ?CS:judge(undefined)])};

handle_state_cast({game_ended, winner}, _, State) ->
  send(State, ["Congratulations! You are the WINNER!!!\r\n"]),
  {noreply, ?CS:new(State, [?CS:status(idle), ?CS:judge(undefined)])};

handle_state_cast({game_ended, loser}, _, State) ->
  send(State, ["You lose. I know that feel bro :[\r\n"]),
  {noreply, ?CS:new(State, [?CS:status(idle), ?CS:judge(undefined)])};





handle_state_cast(Request, _StateName, State) ->
  io:format("Client(~p) received unexpected cast: ~p~n", [self(), Request]),
  {noreply, State}.

% <- CAST EVENT HANDLERS





% -> SOCKET MESSAGE HANDLERS

handle_socket_info(Name, init, State) ->
  case ?Q:register(Name) of
    ok ->
      send(State, [<<"Hello, \""/utf8>>, Name, <<"\"!\r\nType /help for command list\r\n"/utf8>>]),
      {noreply, ?CS:new(State, [?CS:status(idle), ?CS:user(Name)])};
    {error, already_exists} ->
      send(State, [<<"Sorry, but nickname \""/utf8>>, Name, <<"\" already exists :(\r\nPlease, pick another one: ">>]),
      {noreply, State}
  end;

% ALL STATES
%% USERS QUERY
handle_socket_info(<<"/users"/utf8>>, _StateName, State) ->
  Users = ?Q:users(),
  send(State, [<<"Current users:\r\n"/utf8>>, lists:map(fun(X) -> {_,B} = X, ["\"",B,"\"\r\n"] end, Users)]),
  {noreply, State};


handle_socket_info(<<"/play"/utf8>>, idle, State) ->
  ?Q:play(self(), ?CS:get_user(State)),
  send(State, <<"We\'re now searching opponent for you\r\nWait please...\r\n"/utf8>>),
  {noreply, ?CS:new(State, ?CS:status(game_awaiting))};



handle_socket_info(<<"/r"/utf8>>, game, State) ->
  ?J:answer(?CS:get_judge(State), ?CS:get_user(State), rock),
  send(State, <<"Roger that! Let\'s wait for the result!...\r\n"/utf8>>),
  {noreply, ?CS:new(State, ?CS:status(game_ending))};

handle_socket_info(<<"/p"/utf8>>, game, State) ->
  ?J:answer(?CS:get_judge(State), ?CS:get_user(State), paper),
  send(State, <<"Roger that! Let\'s wait for the result...\r\n"/utf8>>),
  {noreply, ?CS:new(State, ?CS:status(game_ending))};

handle_socket_info(<<"/s"/utf8>>, game, State) ->
  ?J:answer(?CS:get_judge(State), ?CS:get_user(State), scissors),
  send(State, <<"Roger that! Let\'s wait for the result...\r\n"/utf8>>),
  {noreply, ?CS:new(State, ?CS:status(game_ending))};



handle_socket_info(<<"/exit"/utf8>>, game, State) ->
  send(State, bye()),
  {stop, normal, State};



handle_socket_info(_, game, State) ->
  send(State, <<"Please, choose from /r, /p or /s: "/utf8>>),
  {noreply, State};

handle_socket_info(<<"/say "/utf8, Message/binary>>, _StateName, State) ->
  ?Q:user_says({self(), ?CS:get_user(State)}, Message),
  {noreply, State};

handle_socket_info(<<"/help"/utf8>>, _StateName, State) ->
  send(State, help_msg()),
  {noreply, State};

% ALL STATES
%% EXIT QUERY
handle_socket_info(<<"/exit"/utf8>>, _StateName, State) ->
  send(State, bye()),
  {stop, normal, State};

%% _DGB
handle_socket_info(_Message, _StateName, State) ->
  {noreply, State}.

% <- SOCKET MESSAGE HANDLERS





% -> MISC ROUTINES

help_msg() ->
  [
    <<"====AVAILABLE COMMANDS====\r\n"/utf8>>,
    <<"|                        |\r\n"/utf8>>,
    <<"| /help - this text      |\r\n"/utf8>>,
    <<"| /users - present users |\r\n"/utf8>>,
    <<"| /say %smt% - say %smt% |\r\n"/utf8>>,
    <<"| /play - queue reg      |\r\n"/utf8>>,
    <<"| /exit - exit           |\r\n"/utf8>>,
    <<"|________________________|\r\n"/utf8>>
  ].



battle_start_msg() ->
  [
    <<" ______________________\r\n"/utf8>>,
    <<"|                      |\r\n"/utf8>>,
    <<"|    BATTLE BEGINS!    |\r\n"/utf8>>,
    <<"|______________________|\r\n"/utf8>>,
    <<"|                      |\r\n"/utf8>>,
    <<"|  1. Type your choice |\r\n"/utf8>>,
    <<"|  2. Press \"Enter\"    |\r\n"/utf8>>,
    <<"|                      |\r\n"/utf8>>,
    <<"|  /r - rock           |\r\n"/utf8>>,
    <<"|  /p - paper          |\r\n"/utf8>>,
    <<"|  /s - scissors       |\r\n"/utf8>>,
    <<"|______________________|\r\n"/utf8>>,
    <<"Your choice: "/utf8>>
  ].



send(State, Msg) ->
  {Transport, Socket} = ?CS:get_pipe(State),
  Transport:send(Socket, Msg).


strip_msg(BinMsg) -> 
  ListMsg = ?UTC(BinMsg, utf8),
  RevListMsg = lists:reverse(ListMsg),
  case RevListMsg of
    [10, 13 | Rest] -> ?UTB(lists:reverse(Rest), utf8);
    [10 | Rest] -> ?UTB(lists:reverse(Rest), utf8);
    _ -> 'ERROR'
  end.
  


is_complete(BinMsg) ->
  ListMsg = binary_to_list(BinMsg),
  RevListMsg = lists:reverse(ListMsg),
  case RevListMsg of
    [10| _] -> true;
    _ -> false
  end.



greet() ->
  [
    <<"    _______         "/utf8>>, <<" ___________ "/utf8>> ,<<"    _______    \r\n"/utf8>>,
    <<"---'   ____)____    "/utf8>>, <<"|           |"/utf8>> ,<<"   (____   '---\r\n"/utf8>>,
    <<"          ______)   "/utf8>>, <<"|  WELCOME! |"/utf8>> ,<<"  (_____)      \r\n"/utf8>>,
    <<"       __________)  "/utf8>>, <<"|___________|"/utf8>> ,<<"  (_____)      \r\n"/utf8>>,
    <<"      (____)        "/utf8>>, <<"             "/utf8>> ,<<"   (____)      \r\n"/utf8>>,
    <<"---.__(___)         "/utf8>>, <<"             "/utf8>> ,<<"    (___)__.---\r\n"/utf8>>
  ].



bye() ->
  [
    <<"    _______    "/utf8>>, <<" __________ "/utf8>> ,<<"         _______    \r\n"/utf8>>,
    <<"---'   ____)   "/utf8>>, <<"|          |"/utf8>> ,<<"    ____(____   '---\r\n"/utf8>>,
    <<"      (_____)  "/utf8>>, <<"|   BYE!   |"/utf8>> ,<<"   (______          \r\n"/utf8>>,
    <<"      (_____)  "/utf8>>, <<"|__________|"/utf8>> ,<<"  (_______          \r\n"/utf8>>,
    <<"      (____)   "/utf8>>, <<"            "/utf8>> ,<<"   (_______         \r\n"/utf8>>,
    <<"---.__(___)    "/utf8>>, <<"            "/utf8>> ,<<"     (__________.---\r\n"/utf8>>
  ].

% <- MISC ROUTINES

% <- PRIVATE
-module(rps_judge).



-include("rps_globals.hrl").



-behaviour(gen_server).



-export([start/1, start_link/1, answer/3, quit_me/2, close/1]).

-export([init/1]).

-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%% API

start(JS) -> supervisor:start_child(?JSUP, [JS]).



start_link(UL) -> gen_server:start_link(?MODULE, ?JS:new(UL), []).



answer(Pid, Nick, Answer) -> gen_server:cast(Pid, {answer, Nick, Answer}).



quit_me(Pid, Nick) -> gen_server:cast(Pid, {quit_me, Nick}).



close(Pid) -> supervisor:terminate_child(?JSUP, Pid).



init(State) ->
  io:format("Game judge(~p) started!~n", [?MODULE]),
  {ok, State}.



handle_call(Request, From, State) ->
  io:format("Game judge(~p) received call message: ~p, from ~p, when state was ~p~n", [self(), Request, From, State]),
  {reply, ok, State}.



handle_cast({quit_me, Nick}, State) ->
	[User1, User2] = State,
	case ?JS:get_user(State, Nick) of
		User1 ->
			?C:game_ended(?JS:get_pid(User2), disconnected),
			{stop, normal, State};
		User2 ->
			?C:game_ended(?JS:get_pid(User2), disconnected),
			{stop, normal, State};
		_User3 -> 'ERROR'
  	end;

handle_cast({answer, Nick, Answer}, State) ->
	NewState = ?JS:set_user_answer(State, Nick, Answer),
	case ?JS:is_complete(NewState) of
		true ->	
			[{User1Res, User1}, {User2Res, User2}] = ?JS:winner(NewState),
			?C:game_ended(?JS:get_pid(User1), User1Res),
			?C:game_ended(?JS:get_pid(User2), User2Res),
			{stop, normal, NewState};
		false -> {noreply, NewState}
	end;

handle_cast(Request, State) ->
  io:format("Game judge(~p) received cast message: ~p~n", [self(), Request]),
  {noreply, State}.



handle_info(Info, State) ->
  io:format("Game judge(~p) received unexpectesdfgsdfgsdfgd message: ~p~n", [self(), Info]),
  {noreply, State}.



terminate(Reason, State) -> io:format("Game judge(~p) terminated: ~p, ~p~n",[?MODULE, Reason, State]).



code_change(_OldVsn, State, _Extra) -> {ok, State}.






% update_state(State, {{Pid, Nick}, Answer}).

% is_complete(State).



% r(ltr, br) ->
%   [<<"    _______\r\n"/utf8>>,
%   <<"---'   ____)\r\n"/utf8>>,
%   <<"      (_____)\r\n"/utf8>>,
%   <<"      (_____)\r\n"/utf8>>,
%   <<"      (____)\r\n"/utf8>>,
%   <<"---.__(___)\r\n"/utf8>>];

% r(rtl, br) ->
%   [<<"    _______    \r\n"/utf8>>,
%   <<"   (____   '---\r\n"/utf8>>,
%   <<"  (_____)      \r\n"/utf8>>,
%   <<"  (_____)      \r\n"/utf8>>,
%   <<"   (____)      \r\n"/utf8>>,
%   <<"    (___)__.---\r\n"/utf8>>].

% r(ltr) ->
%   [<<"    _______    "/utf8>>,
%   <<"---'   ____)   "/utf8>>,
%   <<"      (_____)  "/utf8>>,
%   <<"      (_____)  "/utf8>>,
%   <<"      (____)   "/utf8>>,
%   <<"---.__(___)    "/utf8>>];

% r(rtl) ->
%   [<<"    _______    "/utf8>>,
%   <<"   (____   '---"/utf8>>,
%   <<"  (_____)      "/utf8>>,
%   <<"  (_____)      "/utf8>>,
%   <<"   (____)      "/utf8>>,
%   <<"    (___)__.---"/utf8>>].



% p(ltr, br) ->
%   [<<"    _______\r\n"/utf8>>,
%   <<"---'   ____)____\r\n"/utf8>>,
%   <<"          ______)\r\n"/utf8>>,
%   <<"          _______)\r\n"/utf8>>,
%   <<"         _______)\r\n"/utf8>>,
%   <<"---.__________)\r\n"/utf8>>];

% p(rtl, br) ->
%   [<<"       _______    \r\n"/utf8>>,
%   <<"  ____(____   '---\r\n"/utf8>>,
%   <<" (______          \r\n"/utf8>>,
%   <<"(_______          \r\n"/utf8>>,
%   <<" (_______         \r\n"/utf8>>,
%   <<"   (__________.---\r\n"/utf8>>].

% p(ltr) ->
%   [<<"    _______         "/utf8>>,
%   <<"---'   ____)____    "/utf8>>,
%   <<"          ______)   "/utf8>>,
%   <<"          _______)  "/utf8>>,
%   <<"         _______)   "/utf8>>,
%   <<"---.__________)     "/utf8>>];

% p(rtl) ->
%   [<<"         _______    "/utf8>>,
%   <<"    ____(____   '---"/utf8>>,
%   <<"   (______          "/utf8>>,
%   <<"  (_______          "/utf8>>,
%   <<"   (_______         "/utf8>>,
%   <<"     (__________.---"/utf8>>].



% s(ltr,br) ->
%   [<<"    _______\r\n"/utf8>>,
%   <<"---'   ____)____\r\n"/utf8>>,
%   <<"          ______)\r\n"/utf8>>,
%   <<"       __________)\r\n"/utf8>>,
%   <<"      (____)\r\n"/utf8>>,
%   <<"---.__(___)\r\n"/utf8>>];  

% s(rtl,br) ->
%   [<<"         _______    \r\n"/utf8>>,
%   <<"    ____(____   '---\r\n"/utf8>>,
%   <<"   (______          \r\n"/utf8>>,
%   <<"  (__________       \r\n"/utf8>>,
%   <<"        (____)      \r\n"/utf8>>,
%   <<"         (___)__.---\r\n"/utf8>>].  

% s(ltr) ->
%   [<<"    _______         "/utf8>>,
%   <<"---'   ____)____    "/utf8>>,
%   <<"          ______)   "/utf8>>,
%   <<"       __________)  "/utf8>>,
%   <<"      (____)        "/utf8>>,
%   <<"---.__(___)         "/utf8>>];

% s(rtl) ->
%   [<<"         _______    "/utf8>>,
%   <<"    ____(____   '---"/utf8>>,
%   <<"   (______          "/utf8>>,
%   <<"  (__________       "/utf8>>,
%   <<"        (____)      "/utf8>>,
%   <<"         (___)__.---"/utf8>>].
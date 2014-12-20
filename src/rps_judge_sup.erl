-module(rps_judge_sup).

-include("rps_globals.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1]).



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {
    	ok,
    	{
    		{ simple_one_for_one, 0, 1},
          	[
          		{
          			?J,
          			{?J, start_link, []},
            		temporary,
            		brutal_kill,
            		worker,
            		[?J]
            	}
            ]
        }
    }.

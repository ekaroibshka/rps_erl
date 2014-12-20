-module(rps_sup).

-include("rps_globals.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1]).



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {
    	ok, {
    			{one_for_one, 10, 60},
    			[
    				{
    					?Q,
    					{?Q, start_link, []},
 						permanent,
 						brutal_kill,
 						worker,
 						[?Q]
 					},
					{
    					?JSUP,
    					{?JSUP, start_link, []},
 						permanent,
 						brutal_kill,
 						supervisor,
 						[?JSUP]
 					}

 				]
 			}
 	}.

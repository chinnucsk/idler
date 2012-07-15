%%%-------------------------------------------------------------------
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%%
%%% @end
%%% Created : 22 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(idler_connection_sup).

-behaviour(supervisor).
-include("../include/idler_irc.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    
    Cfgs = idler_config:load_config(),
    
    Children = [ {list_to_atom(Cfg#serverconfig.name), {idler_connection, start_link, [Cfg]}, Restart, Shutdown, Type, [idler_connection]} || Cfg <- Cfgs ],
    io:format("~p~n",[Children]),
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



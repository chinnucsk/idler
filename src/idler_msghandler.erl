%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Behaviour definition for msghandlers
%%% @end
%%% Created : 29 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_msghandler).
-include("../include/idler_irc.hrl").

%% @doc
%% The callback function that needs to be implemented.
%% Always returns ok, sends back with idler_connection:send_msg(self(), Prefix, Command, Args, Tail).
%% If you spawn a new process to handle the message, remember to pass the self() to that function as a Pid argument, or your message will get sent to the newly spawned process.
%% @end
-callback handle_msg(Prefix :: binary(), Command :: binary(), Arguments :: [binary()], Tail :: binary()) -> ok.




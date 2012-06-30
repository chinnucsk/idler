%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Behaviour definition for msghandlers
%%% @end
%%% Created : 29 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(msghandler).
-include("../include/irc.hrl").

%% @doc
%% The callback function that needs to be implemented.
%% Always returns ok, sends back with gen_server:cast(self(), {send_msg, #ircmsg{}}).
%% @end
-callback handle_msg(Message :: #ircmsg{}) -> ok.



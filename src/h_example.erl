%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Example module implementation
%%% @end
%%% Created :  2 Jul 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(h_example).
-behaviour(msghandler).
-include("../include/irc.hrl").
-export([handle_msg/4]).

%% @doc
%% Prefix is the part that contains the nickname and host on most messages
%% Especially PRIVMSG etc.
%% 
%% Command is the type of message it is. CTCP messages get converted from PRIVMSG to
%% CTCP and CTCP_REPLY for easy matching
%% 
%% Args is the list of arguments that is supplied. For example the channel to which the message
%% has been sent.
%% 
%% Tail is the actual message. The things people type in IRC are here.
%% @end
-spec handle_msg(binary(), binary(), [binary()], binary()) -> ok.
handle_msg(_Prefix, _Command, _Args, _Tail) ->
    ok.




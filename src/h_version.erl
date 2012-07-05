-module(h_version).
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
handle_msg(Prefix, <<"CTCP">>, _Args, <<"VERSION">>) ->
    Nick = ircmsg:nick_from_prefix(Prefix),
    connection:send_msg(self(), <<>>, <<"CTCP_REPLY">>, [Nick], <<"DingBot Erlang version http://bitbucket.org/gertm/idler/ ">> ),
    ok;
handle_msg(_,Cmd,_,_) ->
    io:format("--> ~p~n",[Cmd]),
    ok.


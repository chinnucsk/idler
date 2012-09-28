%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% 
%%% @end
%%% Created : 26 Sep 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_le_handler).
-behaviour(idler_msghandler).
-include("../include/idler_irc.hrl").
-export([handle_msg/4]).
%% -compile(export_all).

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
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<"Le doc for ", Doc/binary>>) ->   
    idler_connection:reply(self(), Args, erlang_doc_url(Doc));
handle_msg(_Prefix, <<"CTCP">>, Args, <<"ACTION searches for ", _ToSearch/binary>>) ->
    io:format("In le search handler!"),
    idler_connection:reply(self(), Args, <<"Dunno that yet!">>);
handle_msg(_Prefix, _Command, _Args, _Tail) ->
    ok.

-spec erlang_doc_url(binary()) -> binary().
erlang_doc_url(Doc) ->
    ColonReplaced = binary:replace(Doc,<<":">>,<<".html#">>),
    UrlPart = binary:replace(ColonReplaced, <<"/">>, <<"-">>),
    list_to_binary("http://www.erlang.org/erldoc?q="++edoc_lib:escape_uri(binary_to_list(UrlPart))).
    

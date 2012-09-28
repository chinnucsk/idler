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
-compile(export_all).

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
    case erlang_doc_url(Doc) of
        none -> ok;
        Url -> idler_connection:reply(self(), Args, Url)
    end;
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<"le doc for ", Doc/binary>>) ->   
    case erlang_doc_url(Doc) of
        none -> ok;
        Url -> idler_connection:reply(self(), Args, Url)
    end;
    
handle_msg(_Prefix, <<"CTCP">>, Args, <<"ACTION searches for ", _ToSearch/binary>>) ->
    io:format("In le search handler!"), 
    idler_connection:reply(self(), Args, <<"Dunno that yet!">>);
handle_msg(_Prefix, _Command, _Args, _Tail) ->
    ok.


-spec erlang_doc_url(binary()) -> binary().
erlang_doc_url(Doc) ->
    case binary:split(Doc, <<":">>) of
        [H|_T] -> 
            case url_exists("http://www.erlang.org/doc/man/"++binary_to_list(H)++".html") of
                false -> none;
                true ->
                    reformat_url(Doc)
            end;
        _ -> none
    end.
    
            
url_exists(Url) ->
    case httpc:request(Url) of
        {error, _} -> false;
        {ok, {{_, 404, _}, _, _}} -> false;
        _ -> true
    end.

reformat_url(Doc) ->
    case binary:split(Doc, <<":">>) of
        [H|T] ->
            case binary:split(hd(T), <<"/">>) of 
                [H2|T2] -> iolist_to_binary([<<"http://www.erlang.org/doc/man/">>, H, <<".html#">>, H2, <<"-">>, binary:first(hd(T2))]);
                _ -> none
            end;
        _ -> none
    end.

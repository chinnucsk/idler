%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Handler module for the 'le ..' commands of the bot.
%%% @end
%%% Created : 26 Sep 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_le_handler).
-behaviour(idler_msghandler).
-include("../include/idler_irc.hrl").
-export([handle_msg/4]).
-compile(export_all).

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
                    case reformat(Doc) of
                        none -> iolist_to_binary("http://www.erlang.org/doc/man/"++binary_to_list(H)++".html");
                        Url -> Url
                    end
            end;
        _ -> none
    end.
    
-spec url_exists(string()) -> boolean().
url_exists(Url) ->
    case httpc:request(Url) of
        {error, _} -> false;
        {ok, {{_, 404, _}, _, _}} -> false;
        _ -> true
    end.

reformat(Doc) ->
    Pattern="^(\\w+):(\\w+)/(\\d+)$",
    {ok,Regex} = re:compile(Pattern, []),
    case re:run(Doc, Regex, [{capture, all_but_first, binary}]) of
        {match, [Module, Fn, Arity]} ->
            iolist_to_binary([<<"http://www.erlang.org/doc/man/">>, Module, <<".html#">>, Fn, <<"-">>, Arity]);
        _ -> none
    end.

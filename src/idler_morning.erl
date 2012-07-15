%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Handler for the 'morning' message.
%%% @end
%%% Created : 29 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_morning).
-behaviour(idler_msghandler).
-include("../include/idler_irc.hrl").
-export([handle_msg/4]).

-spec handle_msg(binary(), binary(), [binary()], binary()) -> ok.
handle_msg(Prefix, <<"PRIVMSG">>, Args, <<"not">>) ->
    %% these are used in case I ever switch back to an opaque type.
    Nick = ircmsg:nick_from_prefix(Prefix), 
    case Nick of
        <<"Erlang">> -> 
            connection:send_msg(self(), <<>>, <<"PRIVMSG">>, Args, <<"But it is!">>);
        _ -> connection:send_msg(self(), <<>>, <<"PRIVMSG">>, Args, <<"You sure?">>)
    end;
handle_msg(_, _, _, _) -> ok.


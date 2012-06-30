%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Handler for the 'morning' message.
%%% @end
%%% Created : 29 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(h_morning).
-behaviour(msghandler).
-include("../include/irc.hrl").
-export([handle_msg/1]).

handle_msg(#ircmsg{tail = <<"not">> }=Msg) ->
    %% these are used in case I ever switch back to an opaque type.
    Nick = ircmsg:nick(Msg),
    Args = ircmsg:arguments(Msg),
    case Nick of
        <<"Erlang">> -> 
            connection:send_msg(self(), 
                                ircmsg:create(<<>>,<<"PRIVMSG">>,Args,<<"But it is!">>));
        _ -> connection:send_msg(self(),
                                 ircmsg:create(<<>>,<<"PRIVMSG">>,Args,<<"You sure?">>))
    end;
handle_msg(_) -> ok.



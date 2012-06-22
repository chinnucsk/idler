%%% @author Gert <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert
%%% @doc
%%% Helper module to check for connection timeouts.
%%% @end
%%% Created : 29 Feb 2012 by Gert <@G3rtm on Twitter>

-module(connectionhelper).

-export([ping_server/2, start/2]).


start(Sock, Pid) ->
    receive
    after 15000 ->
            ping_server(Sock, Pid)
    end.

ping_server(Sock, Pid) ->
    gen_server:cast(Pid, ping_server),
    receive
        pong ->
            receive
            after 56000 ->
                    ping_server(Sock, Pid)
            end
        after 57000 ->
                gen_server:cast(Pid, no_pong)
        end.
            

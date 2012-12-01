%% Copyright 2012 Gert Meulyzer

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%% @author Gert Meulyzer <@G3rtm on Twitter>
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
    Nick = idler_ircmsg:nick_from_prefix(Prefix), 
    case Nick of
        <<"Erlang">> -> 
            idler_connection:send_msg(self(), <<>>, <<"PRIVMSG">>, Args, <<"But it is!">>);
        _ -> idler_connection:send_msg(self(), <<>>, <<"PRIVMSG">>, Args, <<"You sure?">>)
    end;
handle_msg(_, _, _, _) -> ok.


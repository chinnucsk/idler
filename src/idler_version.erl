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
-module(idler_version).
-behaviour(idler_msghandler).
-include("../include/idler_irc.hrl").
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
    Nick = idler_ircmsg:nick_from_prefix(Prefix),
    idler_connection:send_msg(self(), <<>>, <<"CTCP_REPLY">>, [Nick], <<"Idler IRC bot http://bitbucket.org/gertm/idler/ ">> ),
    ok;
handle_msg(_,_,_,_) ->
    ok.


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
%%% Behaviour definition for msghandlers
%%% @end
%%% Created : 29 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_msghandler).
-include("../include/idler_irc.hrl").

%% @doc
%% The callback function that needs to be implemented.
%% Always returns ok, sends back with idler_connection:send_msg(self(), Prefix, Command, Args, Tail).
%% If you spawn a new process to handle the message, remember to pass the self() to that function as a Pid argument, or your message will get sent to the newly spawned process.
%% @end
-callback handle_msg(Prefix :: binary(), Command :: binary(), Arguments :: [binary()], Tail :: binary()) -> ok.




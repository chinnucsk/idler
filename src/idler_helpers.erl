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
%%% General helpers.
%%% @end
%%% Created :  2 Dec 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_helpers).
-export([get_urls/1]).

-spec get_urls(binary()) -> [binary()].
get_urls(Bin) ->
    Parts = binary:split(Bin, <<" ">>, [global, trim]),
    lists:flatten([ case Part of
                       <<"http://", _/binary>> -> Part;
                       <<"https://", _/binary>> -> Part;
                       _ -> []
                   end || Part <- Parts ]).


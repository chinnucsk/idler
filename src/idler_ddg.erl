%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Module for interfacing with DuckDuckGo
%%% @end
%%% Created : 16 Nov 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_ddg).
-include("../include/idler_irc.hrl").

%% debug exports
-export([definition/1]).
-define(U(X), unicode:characters_to_binary(X)).

%% various stuff I'll need later:
%% edoc_lib:escape_uri/1
%% http://api.duckduckgo.com/?q=<STRING>&format=json
%% mochijson:decode(STRING)
%% 

-spec definition(string() | binary()) -> binary().
definition(SearchString) when is_binary(SearchString) ->
    definition(binary_to_list(SearchString));
definition(SearchString) ->
    case httpc:request("http://api.duckduckgo.com/?format=json&q="++edoc_lib:escape_uri(SearchString)) of
        {error, _} -> none;
        {ok, {_,_, Result}} -> 
            {struct, PL} = mochijson2:decode(Result),
            Def = ?U(proplists:get_value(<<"Definition">>, PL)),
            Source = ?U(proplists:get_value(<<"DefinitionSource">>, PL)),
            iolist_to_binary([Def, <<" (from ">>, Source, <<")">>])
    end.

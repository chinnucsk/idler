%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Module for interfacing with DuckDuckGo
%%% @end
%%% Created : 16 Nov 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_ddg).
-include("../include/idler_irc.hrl").

%% debug exports
-export([definition/1, related_topics/2]).
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
            {struct, PL} = mochijson:decode(Result),
            Def = proplists:get_value("Definition", PL),
            Source = proplists:get_value("DefinitionSource", PL),
            case {Def, Source} of
                {"", ""} -> iolist_to_binary(["Cannot find definition for ", SearchString]);
                {_, ""} -> iolist_to_binary(["{", SearchString, ", \"", Def, "\"}"]);
                _ -> iolist_to_binary(["{", SearchString, ", \"", Def, " (from ", Source, ")", "\"}"])
            end
    end.

related_topics(Str, Count) when is_binary(Str) ->
    related_topics(binary_to_list(Str), Count);
related_topics(Str, Count) ->
    case httpc:request("http://api.duckduckgo.com/?format=json&q="++edoc_lib:escape_uri(Str)) of
        {error, _} -> none;
        {ok, {_,_, Result}} ->
            {struct, PL} = mochijson:decode(Result),
            {array, Lst} = proplists:get_value("RelatedTopics", PL),
            [ iolist_to_binary(["{", Str, ", \"", ?U(Res),"\"}"]) || Res <- 
            lists:sublist(
              lists:takewhile(fun(X) -> X =/= undefined end,
                                          [ proplists:get_value("Text", X) || {struct,X} <- Lst]), 
              Count) ]
    end.

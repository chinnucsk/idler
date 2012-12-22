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
%%% The rss feed generator for idler.
%%% @end
%%% Created : 22 Dec 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_rss).

-export([item_node/3, item_node/4, store/3, create_xml_file/0,
        store_and_publish/3, utcnow/0]).

%% need to create a file with all the rss feed entries as erlang terms.
%% or separate files for each entry?

item_node(Title, Desc, URL) ->
    item_node(Title, Desc, URL, utcnow()).


item_node(Title, Desc, URL, Date) ->
    {item, [], [{title, [], [Title]},
                {pubDate, [], [Date]},
                {description, [], [Desc]},
                {link, [], [URL]}]}.

utcnow() ->
    httpd_util:rfc1123_date(erlang:localtime_to_universaltime(erlang:localtime())).

rss_term(ItemList) ->
    {rss, [{version, "2.0"}], [{pubDate, [], [utcnow()]}, {channel, [], ItemList}]}.


termstorage_file() ->
    code:priv_dir(idler)++"/yflrss.terms".

rss_filename() ->
    code:priv_dir(idler)++"/yflrss.xml".

%% load previous list of items from the local dump
create_xml_file() ->
    {ok, Terms} = file:consult(termstorage_file()),
    TotalRss = rss_term(Terms),
    file:write_file(rss_filename(), xmerl:export_simple([TotalRss], xmerl_xml)).

store_item(Item) ->
    {ok, Iodev} = file:open(termstorage_file(), [append]),
    io:format(Iodev, "~p.~n",[Item]),
    file:close(Iodev).
    
%% unconsult(File, L) ->
%%     {ok, S} = file:open(File, write),
%%     lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L), 
%%     file:close(S).

store(Title, Desc, URL) ->
    Item = item_node(Title, Desc, URL),
    store_item(Item).

store_and_publish(Title, Desc, URL) ->
    store(Title, Desc, URL),
    create_xml_file().

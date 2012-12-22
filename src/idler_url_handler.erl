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

%%% @doc
%%% Example module implementation
%%% @end
%%% Created :  2 Jul 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_url_handler).
-behaviour(idler_msghandler).
-include("../include/idler_irc.hrl").
-export([handle_msg/4]).

%% debug exports for testing
-export([type_and_size/1, get_page_title/1]).

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
handle_msg(Prefix, <<"PRIVMSG">>, [<<"#yfl">>], Tail) ->
    handle_urls(Prefix, [<<"#yfl">>], Tail);
handle_msg(Prefix, <<"PRIVMSG">>, [<<"#testerlounge">>], Tail) ->
    handle_urls(Prefix, [<<"#testerlounge">>], Tail);
handle_msg(Prefix, <<"CTCP">>, [<<"#yfl">>], Tail) ->
    handle_urls(Prefix, [<<"#yfl">>], Tail);
handle_msg(Prefix, <<"NOTICE">>, [<<"#yfl">>], Tail) ->
    handle_urls(Prefix, [<<"#yfl">>], Tail);
handle_msg(_Prefix, _Command, _Args, _Tail) ->
    ok.

-spec handle_urls(binary(), [binary()], binary()) -> ok.
handle_urls(Prefix, Args, Tail) ->
    [ process_url(Prefix, Args, URL) || URL <- idler_helpers:get_urls(Tail) ].

-spec process_url(binary(), [binary()], binary()) -> ok.
process_url(Prefix, Args, URL) ->
    P = self(),
    case byte_size(URL) > 100 of
        true -> idler_connection:reply(self(), Args, tinyurl(URL));
        _ -> ok
    end,
    spawn(fun() -> export_xml_for_url(URL, idler_ircmsg:nick_from_prefix(Prefix)) end),
    spawn(fun() -> case idler_command_handler:reply_if_single_tweet(URL, Args, P) of
                       ok -> ok;
                       false ->
                           case get_page_title(URL) of
                               none -> ok;
                               Title -> idler_connection:reply(P, Args, Title)
                           end
                   end  
          end).
    
%% fill up the ets table msgHandlers with for example:
%% ets:insert(myTable, {<<"blabla">>, {module, func}}).
%% what way we can make automatic handlers.
%% load and compile modules on the fly?

tinyurl(Url) ->
    %% http://tinyurl.com/api-create.php?url=http://scripting.com/
    case httpc:request("http://tinyurl.com/api-create.php?url="++edoc_lib:escape_uri(binary_to_list(Url))) of
        {error, _} -> ok;
        {ok, {_, _, TinyUrl}} -> list_to_binary(TinyUrl)
    end.

export_xml_for_url(URL, NickName) ->
    Nick = binary_to_list(NickName),
    Title = get_page_title(URL),
    idler_rss:store_and_publish(case Title of
                                        none -> URL;
                                        T -> binary_to_list(T)
                                end,
                                "Posted by "++Nick++" in #YFL on "++idler_rss:utcnow(),
                                URL),
    ok.

type_and_size(Url) ->
    Resp = httpc:request(head, {Url, []}, [{autoredirect, true}], []),
    case Resp of
        {ok, {_, Headers, _}} -> {proplists:get_value("content-type", Headers),
                                  proplists:get_value("content-length", Headers)};
        _ -> {undefined, undefined}
    end.

-spec get_page_title(string() | binary()) -> binary() | none.
get_page_title(<<"http://192.168.", _/binary>>) ->
    none;
get_page_title(<<"https://192.168.", _/binary>>) ->
    none;
get_page_title(<<"http://localhost", _/binary>>) ->
    none;
get_page_title(<<"https://localhost", _/binary>>) ->
    none;
get_page_title(<<"http://10.", _/binary>>) ->
    none;
get_page_title(<<"https://10.", _/binary>>) ->
    none;
%% I know, lazy. I'll make a better 'is_private_ip' function later.
get_page_title(<<"http://172.", _/binary>>) ->
    none;
get_page_title(<<"https://172.", _/binary>>) ->
    none;
get_page_title(Url) when is_binary(Url) ->
    get_page_title(binary_to_list(Url));
get_page_title(Url) ->
    {Type, _} = type_and_size(Url),
    case lists:prefix("text/html", Type) of
        true ->
            Resp = httpc:request(get, {Url, []}, [{autoredirect, true}], []),
            case Resp of
                {ok, {_, _, Contents}} ->
                    {<<"html">>,_,Tags} = mochiweb_html:parse(Contents),
                    [{<<"head">>,_,HeadTags}] = lists:filter(
                                                  fun(X) -> case X of
                                                                {<<"head">>,_,_} -> true;
                                                                _ -> false
                                                            end end, Tags),
                    [{<<"title">>,_,TitleList}] = lists:filter(
                                                    fun(X) -> case X of
                                                                  {<<"title">>,_,_} -> true;
                                                                  _ -> false
                                                              end end, HeadTags),
                    io:format("~p~n",[TitleList]),
                    hd(TitleList);
                _ -> none
            end;
        _ -> io:format("URL not text/html~n"),
            none
    end.

%% for just getting the headers so we can check for content-type/size:
%% httpc:request(head, {"http://www.youtube.com", []}, [{autoredirect, true}], []).

%% example RSS feed I found somewhere:

%% <?xml version="1.0"?>
%% <rss version="2.0">
%% <channel>

%% <title>The Channel Title Goes Here</title>
%% <description>The explanation of how the items are related goes here</description>
%% <link>http://www.directoryoflinksgohere</link>

%% <item>
%% <title>The Title Goes Here</title>
%% <description>The description goes here</description>
%% <link>http://www.linkgoeshere.com</link>
%% </item>

%% <item>
%% <title>Another Title Goes Here</title>
%% <description>Another description goes here</description>
%% <link>http://www.anotherlinkgoeshere.com</link>
%% </item>

%% </channel>
%% </rss>

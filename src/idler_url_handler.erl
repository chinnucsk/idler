%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Example module implementation
%%% @end
%%% Created :  2 Jul 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_url_handler).
-behaviour(idler_msghandler).
-include("../include/idler_irc.hrl").
-export([handle_msg/4]).
-compile(export_all).
-define(Pattern, "(http|ftp|https):\\/\\/[\\w\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\., @?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?").

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
handle_msg(_Prefix, <<"PRIVMSG">>, Args, Tail) when byte_size(Tail) > 135 ->
    case check_for_url(Tail) of
        [] -> ok;
        URL -> idler_connection:reply(self(), Args, tinyurl(URL))
    end;
handle_msg(Prefix, <<"PRIVMSG">>, _Args, Tail) ->
    case check_for_url(Tail) of
        [] -> ok;
        URL -> spawn(fun() -> export_xml_for_url(URL, idler_ircmsg:nick_from_prefix(Prefix)) end),
               ok
    end;
handle_msg(_Prefix, _Command, _Args, _Tail) ->
    ok.


check_for_url(Line) ->
    {ok, Regex} = re:compile(?Pattern, [caseless]), 
    case re:run(Line, Regex, [{capture, first, binary}]) of
        {match, [H]} -> H;
        _ -> []
    end.

%% fill up the ets table msgHandlers with for example:
%% ets:insert(myTable, {<<"blabla">>, {module, func}}).
%% what way we can make automatic handlers.
%% load and compile modules on the fly?

tinyurl(Url) ->
    %% http://tinyurl.com/api-create.php?url=http://scripting.com/
    case httpc:request("http://tinyurl.com/api-create.php?url="++edoc_lib:escape_uri(binary_to_list(Url))) of
        {error, _} -> ok;
        {ok, {_,_,TinyUrl}} -> list_to_binary(TinyUrl)
    end.

export_xml_for_url(URL, NickName) ->
    Nick = binary_to_list(NickName),
    file:write_file(code:priv_dir(idler)++"/"++get_timestamp_string()++".xml",
                    create_rss_item("Posted by "++Nick,edoc_lib:escape_uri(get_pretty_datetime()),URL)),
    ok.

get_timestamp_string() ->
    {A,B,C} = os:timestamp(),
    [D,E,F] = io_lib:format("~p~p~p",[A,B,C]),
    D++E++F.

-spec create_rss_item(Title :: string(), Desc :: string(), URL :: string()) -> string().
create_rss_item(Title, Desc, URL) ->
    "<item><title>"++
        xmerl_lib:export_text(Title)++"</title>"++
        "<description>"++xmerl_lib:export_text(Desc)++"</description>"++
        "<link>"++xmerl_lib:export_text(URL)++"</link></item>".
    
get_pretty_datetime() ->
    {Year, Month, Day} = date(),
    {Hour, Minutes, _} = time(),
    binary_to_list(iolist_to_binary(io_lib:format("~w/~w/~w ~w:~w GMT+1",[Year, Month, Day, Hour, Minutes]))).
            
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


%% {ok,{{"HTTP/1.1",200,"OK"},
%%      [{"connection","close"},
%%       {"date","Mon, 24 Sep 2012 18:42:04 GMT"},
%%       {"server","TinyURL/1.6"},
%%       {"content-length","26"},
%%       {"content-type","text/plain"}],
%%      "http://tinyurl.com/8zrxr9x"}},

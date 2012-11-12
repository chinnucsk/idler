%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Example module implementation
%%% @end
%%% Created :  2 Jul 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_url_handler).
-behaviour(idler_msghandler).
-include("../include/idler_irc.hrl").
-export([handle_msg/4]).

%% debug exports for testing
-export([check_for_url/1, type_and_size/1, get_page_title/1]).

-define(Pattern, "(http|ftp|https):\\/\\/[\\w\\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\., @?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?").
%-define(Pattern, "(http|ftp|https):\\/\\/[\\w\\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\., @?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?").

%%-define(Pattern, "/^(https?:\\/\\/)?([\\da-z\\.-]+)\\.([a-z\\.]{2, 6})([\\/\\w \\.-]*)*\\/?$/").

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

handle_urls(Prefix, Args, Tail) ->
    case check_for_url(Tail) of
        [] -> ok;
        [_|_]=L -> [ spawn(fun() -> export_xml_for_url(URL, idler_ircmsg:nick_from_prefix(Prefix)) end) ||
                       URL <- L ],
                   ok;
        URL -> io:format("Found URL: ~p~n",[URL]),
               spawn(fun() -> export_xml_for_url(URL, idler_ircmsg:nick_from_prefix(Prefix)) end),
               P = self(),
               spawn(fun() -> case get_page_title(URL) of
                                  none -> ok;
                                  Title -> idler_connection:reply(P, Args, Title)
                              end
                     end),
               %% really need a Twitter library instead of this.
               spawn(fun() -> idler_command_handler:reply_if_single_tweet(URL, Args, P) end),
               ok
    end.

check_for_url(Line) ->
    {ok, Regex} = re:compile(?Pattern, [caseless]),
    case re:run(Line, Regex, [{capture, first, binary}]) of
        {match, [H]} -> before_the_spaces(H);
        {match, [_|_]=L} -> L;
        _ -> []
    end.

before_the_spaces(Url) ->
    hd(binary:split(Url,<<" ">>)).

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
    file:write_file(code:priv_dir(idler)++"/"++get_timestamp_string()++".xml",
                    create_rss_item(case Title of
                                        none -> URL;
                                        T -> binary_to_list(T)
                                    end, "Posted by "++Nick++" in YFL on "++get_pretty_datetime(), URL)),
    ok.

get_timestamp_string() ->
    {A, B, C} = os:timestamp(),
    [D, E, F] = io_lib:format("~p~p~p", [A, B, C]),
    D++E++F.

-spec create_rss_item(Title :: string(), Desc :: string(), URL :: string()) -> string().
create_rss_item(Title, Desc, URL) ->
    "<item><title>"++
        xmerl_lib:export_text(Title)++"</title>"++
        "<description>"++xmerl_lib:export_text(Desc)++"</description>"++
        "<pubDate>"++format_utc_timestamp()++"</pubDate>"++
        "<link>"++xmerl_lib:export_text(URL)++"</link></item>".

-spec get_pretty_datetime() -> string().
get_pretty_datetime() ->
    {Year, Month, Day} = date(),
    {Hour, Minutes, _} = time(),
    %%    binary_to_list(iolist_to_binary(
    io_lib:format("~w/~w/~w ~w:~w GMT+1", [Year, Month, Day, Hour, Minutes]).
%%        )).

-spec format_utc_timestamp() -> string().
format_utc_timestamp() ->
    TS = {_, _, _} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_universal_time(TS),
    Mstr = element(Month, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                           "Aug", "Sep", "Oct", "Nov", "Dec"}),
    io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w",
                  [Day, Mstr, Year, Hour, Minute, Second]).

type_and_size(Url) ->
    Resp = httpc:request(head, {Url, []}, [{autoredirect, true}], []),
    case Resp of
        {ok, {_, Headers, _}} -> {proplists:get_value("content-type", Headers),
                                  proplists:get_value("content-length", Headers)};
        _ -> {undefined, undefined}
    end.

-spec get_page_title(string() | binary()) -> binary() | none.
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

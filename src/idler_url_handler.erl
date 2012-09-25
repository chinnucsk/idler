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
handle_msg(_Prefix, <<"PRIVMSG">>, Args, Tail) ->
    if byte_size(Tail) > 135 ->
            case check_for_url(Tail) of
                [] -> ok;
                URL -> idler_connection:send_msg(self(), <<>>, <<"PRIVMSG">>, Args, tinyurl(URL))
            end;
       true -> ok
    end;
handle_msg(_Prefix, _Command, _Args, _Tail) ->
    ok.


check_for_url(Line) ->
    Pattern="(http|ftp|https):\\/\\/[\\w\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\., @?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?", 
    {ok, Regex} = re:compile(Pattern, [caseless]), 
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


%% {ok,{{"HTTP/1.1",200,"OK"},
%%      [{"connection","close"},
%%       {"date","Mon, 24 Sep 2012 18:42:04 GMT"},
%%       {"server","TinyURL/1.6"},
%%       {"content-length","26"},
%%       {"content-type","text/plain"}],
%%      "http://tinyurl.com/8zrxr9x"}},

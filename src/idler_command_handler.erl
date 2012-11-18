%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Command interface to the bot.
%%% @end
%%% Created : 11 Nov 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_command_handler).
-behaviour(idler_msghandler).
-include("../include/idler_irc.hrl").
-export([handle_msg/4]).
-compile(export_all).
-export([reply_if_single_tweet/3]).

-define(SingleTweetPattern, "https://twitter.com/(\\w*)/status/(\\d*)").

-spec handle_msg(binary(), binary(), [binary()], binary()) -> ok.
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<"\\twit ", SearchString/binary>>) ->
    handle_twitter_search(Args, SearchString);
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<"\\@", Username/binary>>) ->
    handle_twitter_usertimeline(Args, Username);
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<"\\def ", SearchString/binary>>) ->
    handle_def_command(Args, SearchString);
handle_msg(_Prefix, _Command, _Args, _Tail) ->
    ok.

handle_twitter_search(Args, SearchString) ->
    URL = "http://search.twitter.com/search.json?rpp=2&q=" ++
        edoc_lib:escape_uri(binary_to_list(SearchString)),
    case httpc:request(URL) of
        {error, _} -> ok;
        {ok, {_, _, JSON}} -> 
            TwtLst = get_tweets(JSON),
            Pid = self(),
            spawn(fun() -> 
                          [ reply_with_tweet(Tweet, Pid, Args) || Tweet <- TwtLst ] 
                  end)
    end.

handle_def_command(Args, SearchString) ->
    P = self(),
    spawn(fun() ->
                  case idler_ddg:related_topics(SearchString, 2) of
                      [] -> idler_connection:reply(P, Args, idler_ddg:definition(SearchString));
                      Lst -> [ idler_connection:reply(P, Args, X) || X <- Lst ]
                  end
          end).

reply_with_tweet(Tweet, Pid, Args) ->
    spawn(fun() ->
                  idler_connection:reply(Pid, Args, unicode:characters_to_binary(Tweet))
          end).

get_tweets(JSON) ->
    P = mochijson:decode(JSON),
    {struct, PropList} = P,
    {array, TwtLst} = proplists:get_value("results", PropList),
    [ tweet_to_line(T) || T <- TwtLst ].

tweet_to_line({struct, P}) ->
    Nick = proplists:get_value("from_user", P),
    Name = proplists:get_value("from_user_name", P),
    Text = proplists:get_value("text", P),
    Name ++ " ("++Nick++"): "++Text.

handle_twitter_usertimeline(Args, Username) ->
    Pid = self(),
    spawn(fun() ->
                  URL = "https://api.twitter.com/1/statuses/user_timeline.json?count=2&screen_name="++
                      %%    URL = "https://api.twitter.com/1.1/statuses/user_timeline.json?count=2&screen_name="++
                      edoc_lib:escape_uri(binary_to_list(Username)),
                  case httpc:request(URL) of
                      {error, _} -> ok;
                      {ok, {_, _, JSON}} -> 
                          {array, TwtList} = mochijson:decode(JSON),
                          %% [ io:format("~p~n",[Tweet])
                          [ spawn(fun() -> reply_with_tweet(Tweet, Pid, Args) end) 
                            || Tweet <- [ get_usertimeline_tweet(Twt) || Twt <- TwtList ]]
                  end
          end).
    
get_usertimeline_tweet({struct, Twt}) ->
    {struct, User} = proplists:get_value("user", Twt),
    Nick = proplists:get_value("screen_name", User),
    Name = proplists:get_value("name", User),
    Text = proplists:get_value("text", Twt),
    Name ++ " ("++Nick++"): "++Text.
    
get_tweet_by_id(Args, ID, Pid) ->
    URL = "https://api.twitter.com/1/statuses/show.json?id="++binary_to_list(ID),
    spawn(fun() ->
                  case httpc:request(URL) of
                      {error, _} -> ok;
                      {ok, {_, _, JSON}} ->
                          TweetStruct = mochijson:decode(JSON),
                          reply_with_tweet(get_usertimeline_tweet(TweetStruct), Pid, Args)
                  end
          end).

reply_if_single_tweet(URL, Args, Pid) ->
    {ok, Regex} = re:compile(?SingleTweetPattern, [caseless]),
    case re:run(URL, Regex, [{capture, all_but_first, binary}]) of
        {match, [_, TweetID]} -> 
            get_tweet_by_id(Args, TweetID, Pid);
        _ -> false
    end.

                       

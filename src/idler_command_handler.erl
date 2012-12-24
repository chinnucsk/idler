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
%%% Command interface to the bot.
%%% @end
%%% Created : 11 Nov 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_command_handler).
-behaviour(idler_msghandler).
-include("../include/idler_irc.hrl").
-export([handle_msg/4]).

-export([reply_if_single_tweet/3]).

-define(SingleTweetPattern, "https://twitter.com/(\\w*)/status/(\\d*)").

-spec handle_msg(binary(), binary(), [binary()], binary()) -> ok.
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<"\\twit ", SearchString/binary>>) ->
    handle_twitter_search(Args, SearchString);
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<"\\@", Username/binary>>) ->
    handle_twitter_usertimeline(Args, Username);
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<"\\def ", SearchString/binary>>) ->
    handle_def_command(Args, SearchString);
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<",bb 15">>) ->
    idler_connection:reply(self(), Args, <<"This command has been replaced by \\bb.">>);
handle_msg(_Prefix, <<"PRIVMSG">>, Args, <<"\\bb ", BB/binary>>) ->
    handle_bb(Args, BB);
handle_msg(_Prefix, _Command, _Args, _Tail) ->
    ok.

-spec handle_twitter_search([binary()], binary()) -> ok.
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
                  end),
            ok
    end.

-spec handle_def_command([binary()], binary()) -> ok.
handle_def_command(Args, SearchString) ->
    P = self(),
    spawn(fun() ->
                  case idler_ddg:related_topics(SearchString, 2) of
                      [] -> idler_connection:reply(P, Args, idler_ddg:definition(SearchString));
                      Lst -> [ idler_connection:reply(P, Args, X) || X <- Lst ]
                  end
          end),
    ok.

-spec reply_with_tweet(string(), pid(), [binary()]) -> ok.
reply_with_tweet(Tweet, Pid, Args) ->
    spawn(fun() ->
                  idler_connection:reply(Pid, Args, unicode:characters_to_binary(Tweet))
          end),
    ok.

-spec get_tweets(string()) -> [string()].
get_tweets(JSON) ->
    P = mochijson:decode(JSON),
    {struct, PropList} = P,
    {array, TwtLst} = proplists:get_value("results", PropList),
    [ tweet_to_line(T) || T <- TwtLst ].

-spec tweet_to_line({'struct', [any()]}) -> string().
tweet_to_line({struct, P}) ->
    Nick = proplists:get_value("from_user", P),
    Name = proplists:get_value("from_user_name", P),
    Text = proplists:get_value("text", P),
    Name ++ " ("++Nick++"): "++Text.

-spec handle_twitter_usertimeline([binary()], binary()) -> ok.
handle_twitter_usertimeline(Args, Username) ->
    Pid = self(),
    spawn(fun() ->
                  URL = "https://api.twitter.com/1/statuses/user_timeline.json?count=2&screen_name="++
                      edoc_lib:escape_uri(binary_to_list(Username)),
                  case httpc:request(URL) of
                      {error, _} -> ok;
                      {ok, {_, _, JSON}} -> 
                          {array, TwtList} = mochijson:decode(JSON),
                          [ spawn(fun() -> reply_with_tweet(Tweet, Pid, Args) end) 
                            || Tweet <- [ get_usertimeline_tweet(Twt) || Twt <- TwtList ]]
                  end
          end),
    ok.
    
-spec get_usertimeline_tweet({'struct', [any()]}) -> string().
get_usertimeline_tweet({struct, Twt}) ->
    {struct, User} = proplists:get_value("user", Twt),
    Nick = proplists:get_value("screen_name", User),
    Name = proplists:get_value("name", User),
    Text = proplists:get_value("text", Twt),
    Name ++ " ("++Nick++"): "++Text.
    
-spec get_tweet_by_id([binary()], binary(), pid()) -> ok.
get_tweet_by_id(Args, ID, Pid) ->
    URL = "https://api.twitter.com/1/statuses/show.json?id="++binary_to_list(ID),
    spawn(fun() ->
                  case httpc:request(URL) of
                      {error, _} -> ok;
                      {ok, {_, _, JSON}} ->
                          TweetStruct = mochijson:decode(JSON),
                          reply_with_tweet(get_usertimeline_tweet(TweetStruct), Pid, Args)
                  end
          end),
    ok.

-spec reply_if_single_tweet(binary(), [binary()], pid()) -> ok | false.
reply_if_single_tweet(URL, Args, Pid) ->
    {ok, Regex} = re:compile(?SingleTweetPattern, [caseless]),
    case re:run(URL, Regex, [{capture, all_but_first, binary}]) of
        {match, [_, TweetID]} -> 
            get_tweet_by_id(Args, TweetID, Pid);
        _ -> false
    end.

                       
handle_bb(Args, BB) ->
    P = self(),
    spawn(fun() ->
                  Nr = list_to_integer(binary_to_list(BB)),
                  case idler_bb:get_post_by_number(Nr) of
                      none -> ok;
                      R -> idler_connection:reply(P, Args, R)
                  end
          end).

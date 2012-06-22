%%%-------------------------------------------------------------------
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2011, Gert Meulyzer
%%% @doc
%%% attempt at IRC client in a gen_server
%%% @end
%%% Created : 28 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(connection).

-behaviour(gen_server).
-include("../include/irc.hrl").

-define(COLON, 58).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-export([send_msg/2, send_raw/2, handle/1, handle_numeric_reply/2]).

-define(SERVER, ?MODULE).

-record(state, {socket, 
                serverconfig=#serverconfig{},
                connectionhelper=undefined
               }).

%%%===================================================================
%%% API
%%%===================================================================

%% -spec send_msg(Pid :: pid()) -> Msg :: ircmsg:#ircmsg{}.
send_msg(Pid, Msg) ->
    gen_server:cast(Pid, {send_msg, Msg}).
send_raw(Pid, Line) ->
    gen_server:cast(Pid, {send_raw, Line}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(#serverconfig{}=Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([#serverconfig{}=Cfg]) ->
    {ok, #state{serverconfig=Cfg}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok, 
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send_raw, Line}, #state{socket=Sock}=State) ->
    send_rawmsg(Sock, Line), 
    {noreply, State};
handle_cast({send_msg, Msg}, #state{socket=Sock}=State) ->
    send_ircmsg(Sock, Msg), 
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(ping_server, State) ->
    gen_server:cast(self(), {send_raw, <<"PING :DingBotConnCheck">>}),
    {noreply, State};
handle_cast(got_pong, #state{connectionhelper=C}=State) ->
    C ! pong,
    {noreply, State};
handle_cast(no_pong, State) ->
    io:format("Disconnected apparently... letting the bot crash.."),
    {stop, disconnected, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{serverconfig=#serverconfig{hostname=Host, port=Port, nick=UserName}=ServerCfg}) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, once}]), 
    %% Do the IRC login
    gen_tcp:send(Sock, "NICK "++UserName), 
    gen_tcp:send(Sock, "\r\n"), 
    gen_tcp:send(Sock, "USER "++UserName++" "++UserName++" "++UserName++" "++UserName), 
    gen_tcp:send(Sock, "\r\n"), 

    %% spawn the helper process to keep pinging the server.
    %% needs to be its own module probably.
    ConHelpPid = spawn_link(connectionhelper, start, [Sock, self()]),
    inet:setopts(Sock, [{active, once}]),
    {noreply, #state{socket=Sock, serverconfig=ServerCfg, connectionhelper=ConHelpPid}};
handle_info({tcp, _S, Data}, #state{socket=Sock}=State) ->
    Msg = ircmsg:parse_line(Data), 
    Response = ?MODULE:handle(Msg), 
    case Response of
        ok -> ok;
        _ -> send_ircmsg(Sock, Response)
    end,
    inet:setopts(Sock, [{active, once}]),    
    {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
    io:format("DISCONNECTED!!!!"), 
    {stop, disconnected, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    io:format("TCP ERROR! ~p~n", [Reason]), 
    {stop, error, State};
handle_info(Info, State) ->
    io:format("UNKNOWN: ~p~n", [Info]), 
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_ircmsg(_Sock, ok) ->
    ok;
send_ircmsg(Sock, Msg) ->
    gen_tcp:send(Sock, iolist_to_binary([ircmsg:to_line(Msg), <<"\r\n">>])).

send_rawmsg(Sock, Line) ->
    gen_tcp:send(Sock, [Line, "\r\n"]).


%%%===================================================================
%%% Main dispatcher
%%% This needs to be reworked to do the actual handling.
%%%===================================================================

-spec handle(#ircmsg{}) -> ok.
handle(#ircmsg{command= <<"PING">>, tail=T}=_Msg) ->
    ircmsg:create(<<>>, <<"PONG">>, [], T);
handle(#ircmsg{prefix=_P, command= <<"PONG">>, arguments=_A, tail=_T}=_Msg) ->
    gen_server:cast(self(), got_pong);
handle(#ircmsg{prefix=_P, command=_C, arguments=_A, tail=_T}=Msg) ->
    case ircmsg:is_numeric(Msg) of
        {true, Nr} -> ?MODULE:handle_numeric_reply(Nr, Msg);
        {false, _} -> io:format("~p~n",[Msg]),
                      ok
    end.
    
%%%===================================================================
%%% Numeric handlers. See: http://www.irchelp.org/irchelp/rfc/rfc2812.txt
%%%===================================================================
%%% Numerics in the range from 001 to 099 are used for client-server
%%% connections only and should never travel between servers.  Replies
%%% generated in the response to commands are found in the range from 200
%%% to 399.
%%%===================================================================

-spec handle_numeric_reply(Nr :: integer(), Msg :: #ircmsg{}) -> #ircmsg{} | ok.

%%% The server sends Replies 001 to 004 to a user upon
%%% successful registration.
%% RPL_WELCOME
handle_numeric_reply(001, _Msg) -> 
    io:format("Reply for RPL_WELCOME not implemented.~n~p~n", [_Msg]),
    ok;
%% RPL_
handle_numeric_reply(002, _Msg) ->
    io:format("Reply for RPL_ not implemented.~n~p~n", [_Msg]),
    ok;
%% RPL_CREATED
handle_numeric_reply(003, _Msg) ->
    io:format("Reply for RPL_CREATED no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_MYINFO
handle_numeric_reply(004, _Msg) ->
    io:format("Reply for RPL_MYINFO no implemented yet.~n~p~n", [_Msg]),
    ok;

%% Sent by the server to a user to suggest an alternative
%% server.  This is often used when the connection is
%% refused because the server is already full.

%% RPL_BOUNCE
handle_numeric_reply(005, _Msg) ->
    io:format("Reply for RPL_BOUNCE not implemented.~n~p~n", [_Msg]),
    ok;

%% - Reply format used by USERHOST to list replies to
%%   the query list.  The reply string is composed as
%%   follows:
 
%%   reply = nickname [ "*" ] "=" ( "+" / "-" ) hostname

%%   The '*' indicates whether the client has registered
%%   as an Operator.  The '-' or '+' characters represent
%%   whether the client has set an AWAY message or not
%%   respectively.

%% RPL_USERHOST
handle_numeric_reply(302, _Msg) ->
    io:format("Reply for RPL_USERHOST not implemented.~n~p~n", [_Msg]),
    ok;

%% - Reply format used by ISON to list replies to the
%%   query list.

%% RPL_ISON
handle_numeric_reply(303, _Msg) ->
    io:format("Reply for RPL_ISON not implemented.~n~p~n", [_Msg]),
    ok;

%% - These replies are used with the AWAY command (if
%% allowed).  RPL_AWAY is sent to any client sending a
%% PRIVMSG to a client which is away.  RPL_AWAY is only
%% sent by the server to which the client is connected.
%% Replies RPL_UNAWAY and RPL_NOWAWAY are sent when the
%% client removes and sets an AWAY message.

%% RPL_AWAY
handle_numeric_reply(301, _Msg) ->
    io:format("Reply for RPL_AWAY not implemented.~n~p~n", [_Msg]),
    ok;

%% RPL_UNAWAY
handle_numeric_reply(305, _Msg) ->
    io:format("Reply for RPL_UNAWAY no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_NOWAWAY
handle_numeric_reply(306, _Msg) ->
    io:format("Reply for RPL_NOWAWAY no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - Replies 311 - 313, 317 - 319 are all replies
%% generated in response to a WHOIS message.  Given that
%% there are enough parameters present, the answering
%% server MUST either formulate a reply out of the above
%% numerics (if the query nick is found) or return an
%% error reply.  The '*' in RPL_WHOISUSER is there as
%% the literal character and not as a wild card.  For
%% each reply set, only RPL_WHOISCHANNELS may appear
%% more than once (for long lists of channel names).
%% The '@' and '+' characters next to the channel name
%% indicate whether a client is a channel operator or
%% has been granted permission to speak on a moderated
%% channel.  The RPL_ENDOFWHOIS reply is used to mark
%% the end of processing a WHOIS message.

%% RPL_WHOISUSER
handle_numeric_reply(311, _Msg) ->
    io:format("Reply for RPL_WHOISUSER no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_WHOISSERVER
handle_numeric_reply(312, _Msg) ->
    io:format("Reply for RPL_WHOISSERVER no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_WHOISOPERATOR
handle_numeric_reply(313, _Msg) ->
    io:format("Reply for RPL_WHOISOPERATOR no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_WHOISIDLE
handle_numeric_reply(317, _Msg) ->
    io:format("Reply for RPL_WHOISIDLE no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFWHOIS
handle_numeric_reply(318, _Msg) ->
    io:format("Reply for RPL_ENDOFWHOIS no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_WHOISCHANNELS
handle_numeric_reply(319, _Msg) ->
    io:format("Reply for RPL_WHOISCHANNELS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - When replying to a WHOWAS message, a server MUST use
%% the replies RPL_WHOWASUSER, RPL_WHOISSERVER or
%% ERR_WASNOSUCHNICK for each nickname in the presented
%% list.  At the end of all reply batches, there MUST
%% be RPL_ENDOFWHOWAS (even if there was only one reply
%% and it was an error).

%% RPL_WHOWASUSER
handle_numeric_reply(314, _Msg) ->
    io:format("Reply for RPL_WHOWASUSER no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFWHOWAS
handle_numeric_reply(369, _Msg) ->
    io:format("Reply for RPL_ENDOFWHOWAS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - Replies RPL_LIST, RPL_LISTEND mark the actual replies
%% with data and end of the server's response to a LIST
%% command.  If there are no channels available to return,
%% only the end reply MUST be sent.

%% RPL_LIST
handle_numeric_reply(322, _Msg) ->
    io:format("Reply for RPL_LIST no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_LISTEND
handle_numeric_reply(323, _Msg) ->
    io:format("Reply for RPL_LISTEND no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_UNIQOPIS
handle_numeric_reply(325, _Msg) ->
    io:format("Reply for RPL_UNIQOPIS no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_CHANNELMODEIS
handle_numeric_reply(324, _Msg) ->
    io:format("Reply for RPL_CHANNELMODEIS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - When sending a TOPIC message to determine the
%% channel topic, one of two replies is sent.  If
%% the topic is set, RPL_TOPIC is sent back else
%% RPL_NOTOPIC.

%% RPL_NOTOPIC
handle_numeric_reply(331, _Msg) ->
    io:format("Reply for RPL_NOTOPIC no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_TOPIC
handle_numeric_reply(332, _Msg) ->
    io:format("Reply for RPL_TOPIC no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - Returned by the server to indicate that the
%% attempted INVITE message was successful and is
%% being passed onto the end client.

%% RPL_INVITING
handle_numeric_reply(341, _Msg) ->
    io:format("Reply for RPL_INVITING no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - Returned by a server answering a SUMMON message to
%% indicate that it is summoning that user.

%% RPL_SUMMONING
handle_numeric_reply(342, _Msg) ->
    io:format("Reply for RPL_SUMMONING no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - When listing the 'invitations masks' for a given channel,
%% a server is required to send the list back using the
%% RPL_INVITELIST and RPL_ENDOFINVITELIST messages.  A
%% separate RPL_INVITELIST is sent for each active mask.
%% After the masks have been listed (or if none present) a
%% RPL_ENDOFINVITELIST MUST be sent.

%% RPL_INVITELIST
handle_numeric_reply(346, _Msg) ->
    io:format("Reply for RPL_INVITELIST no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFINVITELIST
handle_numeric_reply(347, _Msg) ->
    io:format("Reply for RPL_ENDOFINVITELIST no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - When listing the 'exception masks' for a given channel,
%% a server is required to send the list back using the
%% RPL_EXCEPTLIST and RPL_ENDOFEXCEPTLIST messages.  A
%% separate RPL_EXCEPTLIST is sent for each active mask.
%% After the masks have been listed (or if none present)
%% a RPL_ENDOFEXCEPTLIST MUST be sent.

%% RPL_EXCEPTLIST
handle_numeric_reply(348, _Msg) ->
    io:format("Reply for RPL_EXCEPTLIST no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFEXCEPTLIST
handle_numeric_reply(349, _Msg) ->
    io:format("Reply for RPL_ENDOFEXCEPTLIST no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - Reply by the server showing its version details.
%% The <version> is the version of the software being
%% used (including any patchlevel revisions) and the
%% <debuglevel> is used to indicate if the server is
%% running in "debug mode".

%% The "comments" field may contain any comments about
%% the version or further version details.

%% RPL_VERSION
handle_numeric_reply(351, _Msg) ->
    io:format("Reply for RPL_VERSION no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - The RPL_WHOREPLY and RPL_ENDOFWHO pair are used
%% to answer a WHO message.  The RPL_WHOREPLY is only
%% sent if there is an appropriate match to the WHO
%% query.  If there is a list of parameters supplied
%% with a WHO message, a RPL_ENDOFWHO MUST be sent
%% after processing each list item with <name> being
%% the item.

%% RPL_WHOREPLY
handle_numeric_reply(352, _Msg) ->
    io:format("Reply for RPL_WHOREPLY no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFWHO
handle_numeric_reply(315, _Msg) ->
    io:format("Reply for RPL_ENDOFWHO no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - To reply to a NAMES message, a reply pair consisting
%% of RPL_NAMREPLY and RPL_ENDOFNAMES is sent by the
%% server back to the client.  If there is no channel
%% found as in the query, then only RPL_ENDOFNAMES is
%% returned.  The exception to this is when a NAMES
%% message is sent with no parameters and all visible
%% channels and contents are sent back in a series of
%% RPL_NAMEREPLY messages with a RPL_ENDOFNAMES to mark
%% the end.        

%% RPL_NAMREPLY
handle_numeric_reply(353, _Msg) ->
    io:format("Reply for RPL_NAMREPLY no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFNAMES
handle_numeric_reply(366, _Msg) ->
    io:format("Reply for RPL_ENDOFNAMES no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - In replying to the LINKS message, a server MUST send
%% replies back using the RPL_LINKS numeric and mark the
%% end of the list using an RPL_ENDOFLINKS reply.

%% RPL_LINKS
handle_numeric_reply(364, _Msg) ->
    io:format("Reply for RPL_LINKS no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFLINKS
handle_numeric_reply(365, _Msg) ->
    io:format("Reply for RPL_ENDOFLINKS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - When listing the active 'bans' for a given channel,
%% a server is required to send the list back using the
%% RPL_BANLIST and RPL_ENDOFBANLIST messages.  A separate
%% RPL_BANLIST is sent for each active banmask.  After the
%% banmasks have been listed (or if none present) a
%% RPL_ENDOFBANLIST MUST be sent.

%% RPL_BANLIST
handle_numeric_reply(367, _Msg) ->
    io:format("Reply for RPL_BANLIST no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFBANLIST
handle_numeric_reply(368, _Msg) ->
    io:format("Reply for RPL_ENDOFBANLIST no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - A server responding to an INFO message is required to
%% send all its 'info' in a series of RPL_INFO messages
%% with a RPL_ENDOFINFO reply to indicate the end of the
%% replies.

%% RPL_INFO
handle_numeric_reply(371, _Msg) ->
    io:format("Reply for RPL_INFO no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFINFO
handle_numeric_reply(374, _Msg) ->
    io:format("Reply for RPL_ENDOFINFO no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - When responding to the MOTD message and the MOTD file
%% is found, the file is displayed line by line, with
%% each line no longer than 80 characters, using
%% RPL_MOTD format replies.  These MUST be surrounded
%% by a RPL_MOTDSTART (before the RPL_MOTDs) and an
%% RPL_ENDOFMOTD (after).

%% RPL_MOTDSTART
handle_numeric_reply(375, _Msg) ->
    io:format("Reply for RPL_MOTDSTART no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_MOTD
handle_numeric_reply(372, _Msg) ->
    io:format("Reply for RPL_MOTD no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFMOTD
%% we're using this to start the joining of channels.
%% Should get passed around as state, so we know what to join here.
%% 
handle_numeric_reply(376, _Msg) ->
    gen_server:cast(self(), {send_raw, <<"JOIN #erlounge">>}),
    ok;

%% - RPL_YOUREOPER is sent back to a client which has
%% just successfully issued an OPER message and gained
%% operator status.

%% RPL_YOUREOPER
handle_numeric_reply(381, _Msg) ->
    io:format("Reply for RPL_YOUREOPER no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - If the REHASH option is used and an operator sends
%% a REHASH message, an RPL_REHASHING is sent back to
%% the operator.

%% RPL_REHASHING
handle_numeric_reply(382, _Msg) ->
    io:format("Reply for RPL_REHASHING no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - Sent by the server to a service upon successful
%% registration.

%% RPL_YOURESERVICE
handle_numeric_reply(383, _Msg) ->
    io:format("Reply for RPL_YOURESERVICE no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - When replying to the TIME message, a server MUST send
%% the reply using the RPL_TIME format below.  The string
%% showing the time need only contain the correct day and
%% time there.  There is no further requirement for the
%% time string.

%% RPL_TIME
handle_numeric_reply(391, _Msg) ->
    io:format("Reply for RPL_TIME no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - If the USERS message is handled by a server, the
%% replies RPL_USERSTART, RPL_USERS, RPL_ENDOFUSERS and
%% RPL_NOUSERS are used.  RPL_USERSSTART MUST be sent
%% first, following by either a sequence of RPL_USERS
%% or a single RPL_NOUSER.  Following this is
%% RPL_ENDOFUSERS.

%% RPL_USERSSTART
handle_numeric_reply(392, _Msg) ->
    io:format("Reply for RPL_USERSSTART no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_USERS
handle_numeric_reply(393, _Msg) ->
    io:format("Reply for RPL_USERS no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ENDOFUSERS
handle_numeric_reply(394, _Msg) ->
    io:format("Reply for RPL_ENDOFUSERS no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_NOUSERS
handle_numeric_reply(395, _Msg) ->
    io:format("Reply for RPL_NOUSERS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - The RPL_TRACE* are all returned by the server in
%% response to the TRACE message.  How many are
%% returned is dependent on the TRACE message and
%% whether it was sent by an operator or not.  There
%% is no predefined order for which occurs first.
%% Replies RPL_TRACEUNKNOWN, RPL_TRACECONNECTING and
%% RPL_TRACEHANDSHAKE are all used for connections
%% which have not been fully established and are either
%% unknown, still attempting to connect or in the
%% process of completing the 'server handshake'.
%% RPL_TRACELINK is sent by any server which handles
%% a TRACE message and has to pass it on to another
%% server.  The list of RPL_TRACELINKs sent in
%% response to a TRACE command traversing the IRC
%% network should reflect the actual connectivity of
%% the servers themselves along that path.
%% RPL_TRACENEWTYPE is to be used for any connection
%% which does not fit in the other categories but is
%% being displayed anyway.
%% RPL_TRACEEND is sent to indicate the end of the list.

%% RPL_TRACELINK
handle_numeric_reply(200, _Msg) -> 
    io:format("Reply for RPL_TRACELINK no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_TRACECONNECTING
handle_numeric_reply(201, _Msg) -> 
    io:format("Reply for RPL_TRACECONNECTING no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACEHANDSHAKE
handle_numeric_reply(202, _Msg) -> 
    io:format("Reply for RPL_TRACEHANDSHAKE no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACEUNKNOWN
handle_numeric_reply(203, _Msg) -> 
    io:format("Reply for RPL_TRACEUNKNOWN no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACEOPERATOR
handle_numeric_reply(204, _Msg) -> 
    io:format("Reply for RPL_TRACEOPERATOR no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACEUSER
handle_numeric_reply(205, _Msg) -> 
    io:format("Reply for RPL_TRACEUSER no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACESERVER
handle_numeric_reply(206, _Msg) -> 
    io:format("Reply for RPL_TRACESERVER no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACESERVICE
handle_numeric_reply(207, _Msg) -> 
    io:format("Reply for RPL_TRACESERVICE no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACENEWTYPE
handle_numeric_reply(208, _Msg) -> 
    io:format("Reply for RPL_TRACENEWTYPE no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACECLASS
handle_numeric_reply(209, _Msg) -> 
    io:format("Reply for RPL_TRACECLASS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACERECONNECT
handle_numeric_reply(210, _Msg) -> 
    io:format("Reply for RPL_TRACERECONNECT no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACELOG
handle_numeric_reply(261, _Msg) -> 
    io:format("Reply for RPL_TRACELOG no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TRACEEND
handle_numeric_reply(262, _Msg) -> 
    io:format("Reply for RPL_TRACEEND no implemented yet.~n~p~n", [_Msg]),
    ok;

%% - reports statistics on a connection.  <linkname> identifies the particular connection,
%% <sendq> is the amount of data that is queued and waiting to be
%% sent <sent messages> the number of messages sent, and <sent Kbytes> the amount of data sent,
%% in Kbytes. <received messages> and <received Kbytes> are the equivalent of 
%% <sent messages> and <sent Kbytes> for received data,
%% respectively.  <time open> indicates how long ago the connection was opened, in seconds.

%% RPL_STATSLINKINFO
%% "<linkname> <sendq> <sent messages> <sent Kbytes> <received messages> <received Kbytes> <time open>"
handle_numeric_reply(211, _Msg) ->
    io:format("Reply for RPL_STATSLINKINFO no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_STATSCOMMANDS
%% "<command> <count> <byte count> <remote count>"
%% reports statistics on commands usage.
handle_numeric_reply(212, _Msg) ->
    io:format("Reply for RPL_STATSCOMMANDS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_ENDOFSTATS
%% "<stats letter> :End of STATS report"
handle_numeric_reply(219, _Msg) ->
    io:format("Reply for RPL_ENDOFSTATS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_STATSUPTIME
%% ":Server Up %d days %d:%02d:%02d"
%% Reports the server uptime.
handle_numeric_reply(242, _Msg) ->
    io:format("Reply for RPL_STATSUPTIME no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_STATSOLINE
%% "O <hostmask> * <name>"
%% reports the allowed hosts from where user may become IRC operators.
handle_numeric_reply(243, _Msg) ->
    io:format("Reply for RPL_STATSOLINE no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_UMODEIS
%% "<user mode string>"
%% To answer a query about a client's own mode, RPL_UMODEIS is sent back.
handle_numeric_reply(221, _Msg) ->
    io:format("Reply for RPL_UMODEIS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_SERVLIST
%% "<name> <server> <mask> <type> <hopcount> <info>"
handle_numeric_reply(234, _Msg) ->
    io:format("Reply for RPL_SERVLIST no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_SERVLISTEND
%% "<mask> <type> :End of service listing"
%% When listing services in reply to a SERVLIST message,
%% a server is required to send the list back using the
%% RPL_SERVLIST and RPL_SERVLISTEND messages.  A separate
%% RPL_SERVLIST is sent for each service.  After the
%% services have been listed (or if none present) a
%% RPL_SERVLISTEND MUST be sent.
handle_numeric_reply(235, _Msg) ->
    io:format("Reply for RPL_SERVLISTEND no implemented yet.~n~p~n", [_Msg]),
    ok;

%% In processing an LUSERS message, the server
%% sends a set of replies from RPL_LUSERCLIENT,
%% RPL_LUSEROP, RPL_USERUNKNOWN,
%% RPL_LUSERCHANNELS and RPL_LUSERME.  When
%% replying, a server MUST send back
%% RPL_LUSERCLIENT and RPL_LUSERME.  The other
%% replies are only sent back if a non-zero count
%% is found for them.

%% RPL_LUSERCLIENT
%% ":There are <integer> users and <integer> services on <integer> servers"
handle_numeric_reply(251, _Msg) ->
    io:format("Reply for RPL_LUSERCLIENT no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_LUSEROP
%% "<integer> :operator(s) online"
handle_numeric_reply(252, _Msg) ->
    io:format("Reply for RPL_LUSEROP no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_LUSERUNKNOWN
%% "<integer> :unknown connection(s)"
handle_numeric_reply(253, _Msg) ->
    io:format("Reply for RPL_LUSERUNKNOWN no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_LUSERCHANNELS
%% "<integer> :channels formed"
handle_numeric_reply(254, _Msg) ->
    io:format("Reply for RPL_LUSERCHANNELS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_LUSERME
%% ":I have <integer> clients and <integer> servers"
handle_numeric_reply(255, _Msg) ->
    io:format("Reply for RPL_LUSERME no implemented yet.~n~p~n", [_Msg]),
    ok;

%% When replying to an ADMIN message,  a server
%% is expected to use replies RPL_ADMINME
%% through to RPL_ADMINEMAIL and provide a text
%% message with each.  For RPL_ADMINLOC1 a
%% description of what city, state and country
%% the server is in is expected, followed by
%% details of the institution (RPL_ADMINLOC2)
%% and finally the administrative contact for the
%% server (an email address here is REQUIRED)
%% in RPL_ADMINEMAIL.

%% RPL_ADMINME
%% "<server> :Administrative info"
handle_numeric_reply(256, _Msg) ->
    io:format("Reply for RPL_ADMINME no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_ADMINLOC1
%% ":<admin info>"
handle_numeric_reply(257, _Msg) ->
    io:format("Reply for RPL_ADMINLOC1 no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_ADMINLOC2
%% ":<admin info>"
handle_numeric_reply(258, _Msg) ->
    io:format("Reply for RPL_ADMINLOC2 no implemented yet.~n~p~n", [_Msg]),
    ok;
%% RPL_ADMINEMAIL
%% ":<admin info>
handle_numeric_reply(259, _Msg) ->
    io:format("Reply for RPL_ADMINEMAIL no implemented yet.~n~p~n", [_Msg]),
    ok;

%% When a server drops a command without processing it,
%% it MUST use the reply RPL_TRYAGAIN to inform the
%% originating client.

%% RPL_TRYAGAIN
%% "<command> :Please wait a while and try again."
handle_numeric_reply(263, _Msg) ->
    io:format("Reply for RPL_TRYAGAIN no implemented yet.~n~p~n", [_Msg]),
    ok;

%% RPL_TOPICWHOTIME
handle_numeric_reply(333, _Msg) ->
    io:format("Reply for RPL_TOPICWHOTIME no implemented yet.~n~p~n", [_Msg]),
    ok;


%%%%%%%%%%%%% ERROR REPLIES %%%%%%%%%%%%%%%%%%%%%%%%

%% Used to indicate the nickname parameter supplied to a
%% command is currently unused.
%% ERR_NOSUCHNICK
%% "<nickname> :No such nick/channel"
handle_numeric_reply(401, _Msg) ->
    io:format("Reply for ERR_NOSUCHNICK no implemented yet.~n~p~n", [_Msg]),
    ok;

%% Used to indicate the server name given currently
%% does not exist.
%% ERR_NOSUCHSERVER
%% "<server name> :No such server"
handle_numeric_reply(402, _Msg) ->
    io:format("Reply for ERR_NOSUCHSERVER no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOSUCHCHANNEL
%% "<channel name> :No such channel"
%% Used to indicate the given channel name is invalid.
handle_numeric_reply(403, _Msg) ->
    io:format("Reply for ERR_NOSUCHCHANNEL no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_CANNOTSENDTOCHAN
%% "<channel name> :Cannot send to channel"
%% Sent to a user who is either (a) not on a channel
%% which is mode +n or (b) not a chanop (or mode +v) on
%% a channel which has mode +m set or where the user is
%% banned and is trying to send a PRIVMSG message to
%% that channel.
handle_numeric_reply(404, _Msg) ->
    io:format("Reply for ERR_CANNOTSENDTOCHAN no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_TOOMANYCHANNELS
%% "<channel name> :You have joined too many channels"
%% Sent to a user when they have joined the maximum
%% number of allowed channels and they try to join                    
%% another channel.
handle_numeric_reply(405, _Msg) ->
    io:format("Reply for ERR_TOOMANYCHANNELS no implemented yet.~n~p~n", [_Msg]),
    ok;


%% ERR_WASNOSUCHNICK
%% "<nickname> :There was no such nickname"
%% Returned by WHOWAS to indicate there is no history
%% information for that nickname.
handle_numeric_reply(406, _Msg) ->
    io:format("Reply for ERR_WASNOSUCHNICK no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_TOOMANYTARGETS
%% "<target> :<error code> recipients. <abort message>"
%% Returned to a client which is attempting to send a
%% PRIVMSG/NOTICE using the user@host destination format
%% and for a user@host which has several occurrences.
%% Returned to a client which trying to send a
%% PRIVMSG/NOTICE to too many recipients.
%% Returned to a client which is attempting to JOIN a safe
%% channel using the shortname when there are more than one
%% such channel.
handle_numeric_reply(407, _Msg) ->
    io:format("Reply for ERR_TOOMANYTARGETS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOSUCHSERVICE
%% "<service name> :No such service"
%% Returned to a client which is attempting to send a SQUERY
%% to a service which does not exist.
handle_numeric_reply(408, _Msg) ->
    io:format("Reply for ERR_NOSUCHSERVICE no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOORIGIN
%% ":No origin specified"
%% PING or PONG message missing the originator parameter.
handle_numeric_reply(409, _Msg) ->
    io:format("Reply for ERR_NOORIGIN no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NORECIPIENT
%% ":No recipient given (<command>)"
handle_numeric_reply(411, _Msg) ->
    io:format("Reply for ERR_NORECIPIENT no implemented yet.~n~p~n", [_Msg]),
    ok;


%% 412 - 415 are returned by PRIVMSG to indicate that
%% the message wasn't delivered for some reason.
%% ERR_NOTOPLEVEL and ERR_WILDTOPLEVEL are errors that
%% are returned when an invalid use of
%% "PRIVMSG $<server>" or "PRIVMSG #<host>" is attempted.

%% ERR_NOTEXTTOSEND
%% ":No text to send"
handle_numeric_reply(412, _Msg) ->
    io:format("Reply for ERR_NOTEXTTOSEND no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOTOPLEVEL
%% "<mask> :No toplevel domain specified"
handle_numeric_reply(413, _Msg) ->
    io:format("Reply for ERR_NOTOPLEVEL no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_WILDTOPLEVEL
%% "<mask> :Wildcard in toplevel domain"
handle_numeric_reply(414, _Msg) ->
    io:format("Reply for ERR_WILDTOPLEVEL no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_BADMASK
%% "<mask> :Bad Server/host mask"
handle_numeric_reply(415, _Msg) ->
    io:format("Reply for ERR_BADMASK no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_UNKNOWNCOMMAND
%% "<command> :Unknown command"
handle_numeric_reply(421, _Msg) ->
    io:format("Reply for ERR_UNKNOWNCOMMAND no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOMOTD
%% ":MOTD File is missing"
handle_numeric_reply(422, _Msg) ->
    io:format("Reply for ERR_NOMOTD no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOADMININFO
%% "<server> :No administrative info available"
%% Returned by a server in response to an ADMIN message
%% when there is an error in finding the appropriate
%% information.
handle_numeric_reply(423, _Msg) ->
    io:format("Reply for ERR_NOADMININFO no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_FILEERROR
%% ":File error doing <file op> on <file>"
%% Generic error message used to report a failed file
%% operation during the processing of a message.
handle_numeric_reply(424, _Msg) ->
    io:format("Reply for ERR_FILEERROR no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NONICKNAMEGIVEN
%% ":No nickname given"
%% Returned when a nickname parameter expected for a
%% command and isn't found.
handle_numeric_reply(431, _Msg) ->
    io:format("Reply for ERR_NONICKNAMEGIVEN no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_ERRONEUSNICKNAME
%% "<nick> :Erroneous nickname"
%% Returned after receiving a NICK message which contains
%% characters which do not fall in the defined set.  See
%% section 2.3.1 for details on valid nicknames.
handle_numeric_reply(432, _Msg) ->
    io:format("Reply for ERR_ERRONEUSNICKNAME no implemented yet.~n~p~n", [_Msg]),
    ok;


%% ERR_NICKNAMEINUSE
%% "<nick> :Nickname is already in use"
%% Returned when a NICK message is processed that results
%% in an attempt to change to a currently existing
%% nickname.
handle_numeric_reply(433, _Msg) ->
    io:format("Reply for ERR_NICKNAMEINUSE no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NICKCOLLISION
%% "<nick> :Nickname collision KILL from <user>@<host>"
%% Returned by a server to a client when it detects a
%% nickname collision (registered of a NICK that
%% already exists by another server).
handle_numeric_reply(436, _Msg) ->
    io:format("Reply for ERR_NICKCOLLISION no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_UNAVAILRESOURCE
%% "<nick/channel> :Nick/channel is temporarily unavailable"
%% Returned by a server to a user trying to join a channel
%% currently blocked by the channel delay mechanism.
%% Returned by a server to a user trying to change nickname
%% when the desired nickname is blocked by the nick delay
%% mechanism.
handle_numeric_reply(437, _Msg) ->
    io:format("Reply for ERR_UNAVAILRESOURCE no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_USERNOTINCHANNEL
%% "<nick> <channel> :They aren't on that channel"
%% Returned by the server to indicate that the target
%% user of the command is not on the given channel.
handle_numeric_reply(441, _Msg) ->
    io:format("Reply for ERR_USERNOTINCHANNEL no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOTONCHANNEL
%% "<channel> :You're not on that channel"
%% Returned by the server whenever a client tries to
%% perform a channel affecting command for which the
%% client isn't a member.
handle_numeric_reply(442, _Msg) ->
    io:format("Reply for ERR_NOTONCHANNEL no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_USERONCHANNEL
%% "<user> <channel> :is already on channel"
%% Returned when a client tries to invite a user to a
%% channel they are already on.
handle_numeric_reply(443, _Msg) ->
    io:format("Reply for ERR_USERONCHANNEL no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOLOGIN
%% "<user> :User not logged in"
%% Returned by the summon after a SUMMON command for a
%% user was unable to be performed since they were not                             
%% logged in.
handle_numeric_reply(444, _Msg) ->
    io:format("Reply for ERR_NOLOGIN no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_SUMMONDISABLED
%% ":SUMMON has been disabled"
%% Returned as a response to the SUMMON command.  MUST be
%% returned by any server which doesn't implement it.
handle_numeric_reply(445, _Msg) ->
    io:format("Reply for ERR_SUMMONDISABLED no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_USERSDISABLED
%% ":USERS has been disabled"
%% Returned as a response to the USERS command.  MUST be
%% returned by any server which does not implement it.
handle_numeric_reply(446, _Msg) ->
    io:format("Reply for ERR_USERSDISABLED no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOTREGISTERED
%% ":You have not registered"
%% Returned by the server to indicate that the client
%% MUST be registered before the server will allow it
%% to be parsed in detail.
handle_numeric_reply(451, _Msg) ->
    io:format("Reply for ERR_NOTREGISTERED no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NEEDMOREPARAMS
%% "<command> :Not enough parameters"
%% Returned by the server by numerous commands to
%% indicate to the client that it didn't supply enough
%% parameters.
handle_numeric_reply(461, _Msg) ->
    io:format("Reply for ERR_NEEDMOREPARAMS no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_ALREADYREGISTRED
%% ":Unauthorized command (already registered)"
%% Returned by the server to any link which tries to
%% change part of the registered details (such as
%% password or user details from second USER message).
handle_numeric_reply(462, _Msg) ->
    io:format("Reply for ERR_ALREADYREGISTRED no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOPERMFORHOST
%% ":Your host isn't among the privileged"
%% Returned to a client which attempts to register with
%% a server which does not been setup to allow
%% connections from the host the attempted connection
%% is tried.
handle_numeric_reply(463, _Msg) ->
    io:format("Reply for ERR_NOPERMFORHOST no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_PASSWDMISMATCH
%% ":Password incorrect"
%% Returned to indicate a failed attempt at registering
%% a connection for which a password was required and
%% was either not given or incorrect.
handle_numeric_reply(464, _Msg) ->
    io:format("Reply for ERR_PASSWDMISMATCH no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_YOUREBANNEDCREEP
%% ":You are banned from this server"
%% Returned after an attempt to connect and register
%% yourself with a server which has been setup to
%% explicitly deny connections to you.
handle_numeric_reply(465, _Msg) ->
    io:format("Reply for ERR_YOUREBANNEDCREEP no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_YOUWILLBEBANNED
%% Sent by a server to a user to inform that access to the
%% server will soon be denied.
handle_numeric_reply(466, _Msg) ->
    io:format("Reply for ERR_YOUWILLBEBANNED no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_KEYSET
%% "<channel> :Channel key already set"
handle_numeric_reply(467, _Msg) ->
    io:format("Reply for ERR_KEYSET no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_CHANNELISFULL
%% "<channel> :Cannot join channel (+l)"
handle_numeric_reply(471, _Msg) ->
    io:format("Reply for ERR_CHANNELISFULL no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_UNKNOWNMODE
%% "<char> :is unknown mode char to me for <channel>"
handle_numeric_reply(472, _Msg) ->
    io:format("Reply for ERR_UNKNOWNMODE no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_INVITEONLYCHAN
%% "<channel> :Cannot join channel (+i)"
handle_numeric_reply(473, _Msg) ->
    io:format("Reply for ERR_INVITEONLYCHAN no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_BANNEDFROMCHAN
%% "<channel> :Cannot join channel (+b)"
handle_numeric_reply(474, _Msg) ->
    io:format("Reply for ERR_BANNEDFROMCHAN no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_BADCHANNELKEY
%% "<channel> :Cannot join channel (+k)"
handle_numeric_reply(475, _Msg) ->
    io:format("Reply for ERR_BADCHANNELKEY no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_BADCHANMASK
%% "<channel> :Bad Channel Mask"
handle_numeric_reply(476, _Msg) ->
    io:format("Reply for ERR_BADCHANMASK no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOCHANMODES
%% "<channel> :Channel doesn't support modes"
handle_numeric_reply(477, _Msg) ->
    io:format("Reply for ERR_NOCHANMODES no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_BANLISTFULL
%% "<channel> <char> :Channel list is full"
handle_numeric_reply(478, _Msg) ->
    io:format("Reply for ERR_BANLISTFULL no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOPRIVILEGES
%% ":Permission Denied- You're not an IRC operator"
%% Any command requiring operator privileges to operate
%% MUST return this error to indicate the attempt was
%% unsuccessful.
handle_numeric_reply(481, _Msg) ->
    io:format("Reply for ERR_NOPRIVILEGES no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_CHANOPRIVSNEEDED
%% "<channel> :You're not channel operator"
%% Any command requiring 'chanop' privileges (such as
%% MODE messages) MUST return this error if the client
%% making the attempt is not a chanop on the specified
%% channel.
handle_numeric_reply(482, _Msg) ->
    io:format("Reply for ERR_CHANOPRIVSNEEDED no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_CANTKILLSERVER
%% ":You can't kill a server!"
%% Any attempts to use the KILL command on a server
%% are to be refused and this error returned directly
%% to the client.
handle_numeric_reply(483, _Msg) ->
    io:format("Reply for ERR_CANTKILLSERVER no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_RESTRICTED
%% ":Your connection is restricted!"
%% Sent by the server to a user upon connection to indicate
%% the restricted nature of the connection (user mode "+r").
handle_numeric_reply(484, _Msg) ->
    io:format("Reply for ERR_RESTRICTED no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_UNIQOPPRIVSNEEDED
%% ":You're not the original channel operator"
%% Any MODE requiring "channel creator" privileges MUST
%% return this error if the client making the attempt is not
%% a chanop on the specified channel.
handle_numeric_reply(485, _Msg) ->
    io:format("Reply for ERR_UNIQOPPRIVSNEEDED no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_NOOPERHOST
%% ":No O-lines for your host"
%% If a client sends an OPER message and the server has
%% not been configured to allow connections from the
%% client's host as an operator, this error MUST be
%% returned.
handle_numeric_reply(491, _Msg) ->
    io:format("Reply for ERR_NOOPERHOST no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_UMODEUNKNOWNFLAG
%% ":Unknown MODE flag"
%% Returned by the server to indicate that a MODE
%% message was sent with a nickname parameter and that
%% the a mode flag sent was not recognized.
handle_numeric_reply(501, _Msg) ->
    io:format("Reply for ERR_UMODEUNKNOWNFLAG no implemented yet.~n~p~n", [_Msg]),
    ok;

%% ERR_USERSDONTMATCH
%% ":Cannot change mode for other users"
%% Error sent to any user trying to view or change the
%% user mode for a user other than themselves.
handle_numeric_reply(502, _Msg) ->
    io:format("Reply for ERR_USERSDONTMATCH no implemented yet.~n~p~n", [_Msg]),
    ok;

handle_numeric_reply(Nr, Msg) ->
    io:format("Unknown numeric reply: ~p: ~p ~n",[Nr, Msg]).
%% 
%% 

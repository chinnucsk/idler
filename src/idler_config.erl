%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% The configuration module for idler.
%%% @end
%%% Created : 19 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(idler_config).
-include("../include/idler_irc.hrl").

-export([load_config/0]).


load_config() ->
    PrivDir = code:lib_dir(idler, priv),
    {ok, CfgTerms, _} = file:path_consult([PrivDir], "idler.cfg"),
    [ #serverconfig{name=Name, hostname=HostName, port=Port, nick=Nick, channels=Channels, modules=Modules}
                    || {server, Name, HostName, Port, Nick, Channels, Modules} <- CfgTerms ].


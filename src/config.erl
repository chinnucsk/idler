%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% The configuration module for idler.
%%% @end
%%% Created : 19 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(config).
-include("../include/irc.hrl").

-compile(export_all).


load_config() ->
    PrivDir = code:lib_dir(idler, priv),
    {ok, CfgTerms, _} = file:path_consult([PrivDir], "idler.cfg"),
    [ #serverconfig{name=Name, hostname=HostName, port=Port, nick=Nick, channels=Channels}
                    || {server, Name, HostName, Port, Nick, Channels} <- CfgTerms ].


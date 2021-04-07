%%%-------------------------------------------------------------------
%%% Created : 17. Jan 2021 10:31
%%%-------------------------------------------------------------------
-module(botVCs).

-import(string, [concat/2]).
-import(botVC, [start/4]).
%% API
-export([go/0]).

%% Entwurf 4.3
go() ->
  {ok, Config} = file:consult("botVC.cfg"),
  {ok, Bots} = vsutil:get_config_value(bots, Config),
  {ok, LifeTime} = vsutil:get_config_value(lifetime, Config),
  {ok, {Min, SendeIntervall}} = vsutil:get_config_value(sendeintervall, Config),
  {ok, Servername} = vsutil:get_config_value(servername, Config),
  {ok, Servernode} = vsutil:get_config_value(servernode, Config),
  {ok, HostName} = inet:gethostname(),
  LogFile = string:concat(string:concat("botVCSTARTER", string:concat("@", HostName)), ".log"),
  util:logging(LogFile, "botVG.cfg ausgelesen.\n"),
  case net_adm:ping(Servernode) of
    pong -> timer:sleep(1000),
      util:logging(LogFile, ("starter ping an Servernode war erfolgreich.\n")),
      spawn(fun() -> init(Bots, LifeTime, {Min, SendeIntervall}, {Servername, Servernode}, HostName, LogFile) end);
    pang -> % logge einen fehler hier
      util:logging(LogFile, ("PANG: Starter wurde nicht gestartet.\n")),
      nok
  end.

init(0, _LifeTime, _SendeIntervall, _Server, _HostName, LogFile) ->
  util:logging(LogFile, ("Alle Bots gestartet. \n"));

init(Bots, LifeTime, SendeIntervall, Server, HostName, LogFile) ->
  BotLogFile = string:concat(string:concat(string:concat("botVC_", util:to_String(Bots)), atom_to_list(node())), ".log"),
  spawn(fun() ->
    botVC:start(LifeTime, SendeIntervall, Server, BotLogFile) end),
  util:logging(LogFile, io_lib:format("Bot ~p wurde gestartet.\n", [Bots])),
  init(Bots - 1, LifeTime, SendeIntervall, Server, HostName, LogFile).
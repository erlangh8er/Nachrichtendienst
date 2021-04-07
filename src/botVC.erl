%%%-------------------------------------------------------------------
%%% Created : 17. Dez 2020 10:31
%%%-------------------------------------------------------------------
-module(botVC).

%% API
-export([start/4]).
%% LifeTime
%% Sendeintervall: {Min, SendeIntervall}
%% Server: {Servername, Servernode}
%% BotLogFile fuer den jeweiligen Bot
start(LifeTime, SendeIntervall, {Servername, Servernode}, BotLogFile) ->
  util:logging(BotLogFile, io_lib:format("Starte Bot mit LifeTime: ~p, Server: ~p. \n", [LifeTime, {Servername, Servernode}])),
  spawn(fun() ->
    startBot(LifeTime, SendeIntervall, {Servername, Servernode}, BotLogFile) end).

%% Start Methode des Prozesses
startBot(LifeTime, BotSendeintervall, Server, BotLogFile) ->
  BRPPID = spawn(fun() ->
    startBotReceiver(BotLogFile) end),
  util:logging(BotLogFile, io_lib:format("Bot Receiver Process PID: ~p wurde gestartet.\n", [BRPPID])),
  BSPPID = spawn(fun() ->
    startBotSender(BRPPID, BotLogFile) end),
  util:logging(BotLogFile, io_lib:format("Bot Sender Process PID: ~p wurde gestartet.\n", [BSPPID])),
  BMPPID = spawn(fun() ->
    startBotManager(Server, BSPPID, BRPPID, LifeTime, BotSendeintervall, BotLogFile) end),
  util:logging(BotLogFile, io_lib:format("Bot Manager Process PID: ~p wurde gestartet.\n", [BMPPID])),
  util:logging(BotLogFile, io_lib:format("Bot gestartet mit Manager:~p, Starter:~p, Receiver: ~p \n", [BMPPID, BSPPID, BRPPID])).

%% Start Methode des Managers
startBotManager(Server, BotSenderProzessPID, BotReceiverProzessPID, LifeTime, {Min, SendeIntervall}, BotLogFile) ->
  {Pnum, VCList} = getIDFromTower(BotReceiverProzessPID, self(), Server, BotLogFile),
  VT = vectorC:initVT(Pnum),
  util:logging(BotLogFile, io_lib:format("startBotManager: VT :~lp.\n", [VT])),
  %% Entwurf 3.1.3 orange
  BotReceiverProzessPID ! {go, self()},
  util:logging(BotLogFile, io_lib:format("startBotManager: BotReceiverProzessPID go gesendet mit PID:~p.\n", [self()])),
  %% Entwurf 3.1.3 gelb
  BotSenderProzessPID ! {go, self()},
  util:logging(BotLogFile, io_lib:format("startBotManager: BotSenderProzessPIDs go gesendet mit PID:~p.\n", [self()])),
  BotSendeintervall = rand:uniform(SendeIntervall - Min + 1) + Min - 1,
  util:logging(BotLogFile, io_lib:format("loopBotSender: BotSendeintervall: ~p\n", [BotSendeintervall])),
  SendTimer = vsutil:reset_timer(0, BotSendeintervall, sendInvoker),
  BotLocalEventIntervall = rand:uniform(BotSendeintervall),
  util:logging(BotLogFile, io_lib:format("loopBotSender: BotLocalEventIntervall: ~p\n", [BotLocalEventIntervall])),
  BotLocalEventIntervallTimer = vsutil:reset_timer(0, BotLocalEventIntervall, localEventInvoker),
  vsutil:reset_timer(0, LifeTime, kill),
  loopBotManager(VT, VCList, Server, BotSenderProzessPID, BotReceiverProzessPID, SendTimer, BotSendeintervall, BotLocalEventIntervallTimer, BotLocalEventIntervall, BotLogFile).

%% Entwurf 3.1.3
loopBotManager(VT, VCList, Server, BotSenderProzessPID, BotReceiverProzessPID, SendTimer, BotSendeintervall, BotLocalEventIntervallTimer, BotLocalEventIntervall, BotLogFile) ->
  receive
    %% Entwurf 3.1.3 lila
    %% Entwurf 4.3.2
    {sync, VTn} ->
      util:logging(BotLogFile, io_lib:format("loopBotManager: sync ~lp\n", [VTn])),
      NewVT = vectorC:tickVT(VT),
      NewVT2 = vectorC:syncVT(NewVT, VTn),
      util:logging(BotLogFile, io_lib:format("loopBotManager: sync: NewVT2:~lp\n", [NewVT2])),

      util:logging(BotLogFile, io_lib:format("loopBotManager: sync: setMessagestamp ~lp ~lp\n", [VTn, NewVT2])),
      Server ! {setMessagestamp, VTn, NewVT2, BotReceiverProzessPID},
      loopBotManager(NewVT2, VCList, Server, BotSenderProzessPID, BotReceiverProzessPID, SendTimer, BotSendeintervall, BotLocalEventIntervallTimer, BotLocalEventIntervall, BotLogFile);
    %% Entwurf 3.1.3 braun
    %% Entwurf 4.3.1
    {getVCList, Destination} ->
      util:logging(BotLogFile, io_lib:format("loopBotManager: getVCList Destination:~p \n", [Destination])),
      %% Entwurf 3.1.3 braun 1
      {_Pnum, NewVCList} = getIDFromTower(BotReceiverProzessPID, self(), Server, BotLogFile),
      NewVT = vectorC:tickVT(VT),
      util:logging(BotLogFile, io_lib:format("loopBotManager: getVCList NewVT:~lp\n", [NewVT])),

      util:logging(BotLogFile, io_lib:format("loopBotManager: getVCList setTimestamp ~p\n", [BotReceiverProzessPID])),
      %% Entwurf 3.1.3 braun 2
      Server ! {setTimestamp, NewVT, BotReceiverProzessPID},

      Destination ! {getVCList, NewVT, NewVCList},
      loopBotManager(NewVT, VCList, Server, BotSenderProzessPID, BotReceiverProzessPID, SendTimer, BotSendeintervall, BotLocalEventIntervallTimer, BotLocalEventIntervall, BotLogFile);
    %% Entwurf 3.1.3 weiss
    sendInvoker ->
      util:logging(BotLogFile, io_lib:format("loopBotManager: sendInvoker Destination:~p \n", [BotSenderProzessPID])),
      BotSenderProzessPID ! send,
      NewSendTimer = vsutil:reset_timer(SendTimer, BotSendeintervall, sendInvoker),
      loopBotManager(VT, VCList, Server, BotSenderProzessPID, BotReceiverProzessPID, NewSendTimer, BotSendeintervall, BotLocalEventIntervallTimer, BotLocalEventIntervall, BotLogFile);
    %% Entwurf 3.1.3 grau
    localEventInvoker ->
      util:logging(BotLogFile, io_lib:format("loopBotManager: localEventInvoker:~lp Destination:~p \n", [VT, BotSenderProzessPID])),
      NewVT = vectorC:tickVT(VT),
      util:logging(BotLogFile, io_lib:format("loopBotManager: localEventInvoker: NewVT:~lp\n", [NewVT])),
      Server ! {setTimestamp, NewVT, BotReceiverProzessPID},
      NewBotLocalEventIntervallTimer = vsutil:reset_timer(BotLocalEventIntervallTimer, BotLocalEventIntervall, localEventInvoker),
      loopBotManager(NewVT, VCList, Server, BotSenderProzessPID, BotReceiverProzessPID, SendTimer, BotSendeintervall, NewBotLocalEventIntervallTimer, BotLocalEventIntervall, BotLogFile);
    %% Entwurf 3.1.3 schwarz
    kill ->
      util:logging(BotLogFile, "startBotManager: kill\n"),
      util:logging(BotLogFile, "receiveBot: kill \n"),
      erlang:exit(BotSenderProzessPID, kill),
      timer:sleep(500),
      erlang:exit(BotReceiverProzessPID, kill),
      timer:sleep(500),
      erlang:exit(self(), kill)
  end.

%% Start Methode des Senders, wartet auf GO
startBotSender(BotReceiverProzessPID, BotLogFile) ->
  receive
    {go, ManagerPID} ->
      loopBotSender(ManagerPID, BotReceiverProzessPID, BotLogFile)
  end.

%% Entwurf 3.2.2 Abbildung 8 oberer Teil
loopBotSender(BMPPID, BotReceiverProzessPID, BotLogFile) ->
  receive
    send ->
      util:logging(BotLogFile, "loopBotSender: send\n"),
      BMPPID ! {getVCList, self()},
      receive
        {getVCList, VT, VCList} ->
          RandomBotPID = getRandomElementFromList(VCList),
          Data = string:concat(string:concat(string:concat("Pnum", util:to_String(vectorC:myVTid(VT))), "_"), vsutil:now2string(erlang:timestamp())),
          RandomBotPID ! {messageVT, Data, VT, BotReceiverProzessPID},
          util:logging(BotLogFile, io_lib:format("loopBotSender: send RBPID: ~p. Content: VT:~lp \n", [RandomBotPID, VT]))
      end,
      loopBotSender(BMPPID, BotReceiverProzessPID, BotLogFile)
  end.

%% Hilfsmethode zum shuffeln der VCList
getRandomElementFromList(List) ->
  [{_Pnum, Head} | _Tail] = util:shuffle(List),
  Head.

%% Start Methode des Receivers, wartet auf GO
startBotReceiver(BotLogFile) ->
  receive
    {go, ManagerPID} ->
      loopBotReceiver(ManagerPID, BotLogFile)
  end.

%% Entwurf 3.2.2 Abbildung 8 unterer Teil
loopBotReceiver(BMPPID, BotLogFile) ->
  receive
    {messageVT, Data, VTn, ReceiverPIDn} ->
      util:logging(BotLogFile, io_lib:format("loopBotReceiver: messageVT ~p ~lp ~p \n", [Data, VTn, ReceiverPIDn])),
      BMPPID ! {sync, VTn},
      loopBotReceiver(BMPPID, BotLogFile)
  end.

%% Hilfsmethode zu getID
getIDFromTower(BotReceiverProzessPID, BotManagerProzessPID, Server, BotLogFile) ->
  Server ! {getID, BotReceiverProzessPID, BotManagerProzessPID},
  receive
    {vc, Pnum_n, VCList} ->
      util:logging(BotLogFile, io_lib:format("getIDFromTower: receive getID: ~p \n", [Pnum_n])),
      {Pnum_n, VCList}
  after 5000 ->
    util:logging(BotLogFile, io:format("getIDFromTower: getID keine Antwort.\n"))
  end.
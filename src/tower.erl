%%%-------------------------------------------------------------------
%%% Created : 17. Dez 2020 11:42
%%%-------------------------------------------------------------------
-module(tower).
-import(string, [concat/2]).
%% API
-export([vcInit/0, vcInit/2]).

%% Entwurf 2.1.7
vcInit() ->
  LogFile = string:concat("Tower_", string:concat(atom_to_list(node()), ".log")),
  SVCTPID = startVektoruhrZentrale(false, LogFile),
  util:logging(LogFile, io_lib:format("Tower 'steffensVCTower' mit PID:~p wird gestartet...\n", [SVCTPID])),
  register(steffensVCTower, SVCTPID).

%% Entwurf 2.1.8
vcInit({HubName, HubNode}, Silent) ->
  LogFile = string:concat("Tower_", string:concat(atom_to_list(node()), ".log")),
  case net_adm:ping(HubNode) of
    pong ->
      timer:sleep(1000),
      util:logging(LogFile, io_lib:format("Tower ping an Hub: ~p war erfolgreich.\n", [HubNode])),
      VZPID = startVektoruhrZentrale(Silent, LogFile),
      {HubName, HubNode} ! {registerTower, VZPID, self()},
      util:logging(LogFile, io_lib:format("Tower 'steffensVCTower' mit PID ~p registrierte sich bei Hub: ~p\n", [VZPID, {HubName, HubName}])),
      register(steffensVCTower, VZPID);
    pang -> % logge einen fehler hier
      util:logging(LogFile, "Tower wurde nicht gestartet.\n")
  end.

%% Start Methode des Prozesses
%% Silent siehe Entwurf 2.1
startVektoruhrZentrale(Silent, LogFile) ->
  util:logging(LogFile, io_lib:format("Tower im Silent Mode? ~p\n", [Silent])),
  MPPID = spawn(fun() ->
    startMonitor([], [], LogFile) end),
  util:logging(LogFile, io_lib:format("Monitor Process PID: ~p wurde gestartet.\n", [MPPID])),
  TPPID = spawn(fun() ->
    startTower(MPPID, [], 0, Silent, LogFile) end),
  util:logging(LogFile, io_lib:format("Tower Process PID: ~p wurde gestartet.\n", [TPPID])),
  spawn(fun() ->
    receiveVektoruhrZentrale(TPPID, MPPID, LogFile) end).

%% Start Methode des Tower Prozesses
startTower(TPPID, VCIDList, Counter, Silent, LogFile) ->
  receive
    %% Entwurf 2.1.1
    {getID, ReceiverPIDn, PIDn} ->
      if
        Silent == false ->
          {NewVCIDList, NewCounter} = getID(ReceiverPIDn, PIDn, VCIDList, Counter),
          startTower(TPPID, NewVCIDList, NewCounter, Silent, LogFile);
        true -> util:logging(LogFile, io_lib:format("startTower: Silent=~p, daher keine Antworten.\n", [Silent]))
      end
  end.

%% Start Methode des Monitor Prozesses
startMonitor(TimestampList, MessagestampList, LogFile) ->
  receive
    %% Entwurf 2.1.2
    {setTimestamp, VTn, ReceiverPIDn} ->
      util:logging(LogFile, io_lib:format("startMonitor: setTimestamp ~lp ~p\n", [VTn, ReceiverPIDn])),
      NewTimestampList = setTimestamp(VTn, TimestampList, LogFile),
      startMonitor(NewTimestampList, MessagestampList, LogFile);
    %% Entwurf 2.1.3
    {setMessagestamp, FromVTn, ToVTn, ReceiverPIDn} ->
      util:logging(LogFile, io_lib:format("startMonitor: setMessagestamp ~lp ~lp ~p \n", [FromVTn, ToVTn, ReceiverPIDn])),
      NewMessagestampList = setMessagestamp(FromVTn, ToVTn, MessagestampList),
      NewTimestampList = setTimestamp(ToVTn, TimestampList, LogFile),
      startMonitor(NewTimestampList, NewMessagestampList, LogFile);
    %% Entwurf 2.1.4
    {printMessagestamp, PID} ->
      util:logging(LogFile, io_lib:format("startMonitor: printMessagestamp ~p \n", [PID])),
      spawn(fun() -> printMessagestamp(TimestampList, MessagestampList, LogFile) end),
      startMonitor(TimestampList, MessagestampList, LogFile);
    %% Entwurf 2.1.5
    {printTimeLine, Pnum, PID} ->
      util:logging(LogFile, io_lib:format("startMonitor: printTimeLine ~p ~p \n", [Pnum, PID])),
      spawn(fun() -> printTimeLine(Pnum, TimestampList, LogFile) end),
      startMonitor(TimestampList, MessagestampList, LogFile)
  end.

%% Prozess Schnittstelle nach aussen
%% Entwurf 3.1.2, weisser Kasten
receiveVektoruhrZentrale(Tower, Monitor, LogFile) ->
  receive
    %% Entwurf 2.1.1
    {getID, ReceiverPIDn, PIDn} ->
      spawn(fun() -> send(Tower, {getID, ReceiverPIDn, PIDn}) end),
      receiveVektoruhrZentrale(Tower, Monitor, LogFile);
    %% Entwurf 2.1.2
    {setTimestamp, VTn, ReceiverPIDn} ->
      spawn(fun() -> send(Monitor, {setTimestamp, VTn, ReceiverPIDn}) end),
      receiveVektoruhrZentrale(Tower, Monitor, LogFile);
    %% Entwurf 2.1.3
    {setMessagestamp, FromVTm, ToVTn, ReceiverPIDn} ->
      spawn(fun() -> send(Monitor, {setMessagestamp, FromVTm, ToVTn, ReceiverPIDn}) end),
      receiveVektoruhrZentrale(Tower, Monitor, LogFile);
    %% Entwurf 2.1.4
    {printMessagestamp, PID} ->
      spawn(fun() -> send(Monitor, {printMessagestamp, PID}) end),
      receiveVektoruhrZentrale(Tower, Monitor, LogFile);
    %% Entwurf 2.1.5
    {printTimeLine, Pnum, PID} ->
      spawn(fun() -> send(Monitor, {printTimeLine, Pnum, PID}) end),
      receiveVektoruhrZentrale(Tower, Monitor, LogFile);
    %% Entwurf 2.1.6
    kill ->
      util:logging(LogFile, "receiveVektoruhrZentrale: kill\n"),
      erlang:exit(Tower, kill),
      timer:sleep(1000),
      erlang:exit(Monitor, kill),
      timer:sleep(1000),
      erlang:exit(self(), kill);
    _Anything ->
      util:logging(LogFile, io:format("##################### receiveVektoruhrZentrale: ANYTHING ##################### \n")),
      receiveVektoruhrZentrale(Tower, Monitor, LogFile)
  end.

%% Hilfsmethode zu spawn(fun()-> XYZ end)
%% da es ohne Methode nicht ausgefuehrt wurde
send(Destination, Content) ->
  Destination ! Content.

%% Entwurf 2.1.1
getID(ReceiverPIDn, PIDn, VCIDList, Counter) ->
  {Result, {Pnum, _ReceiverPID}} = findReceiverPIDInVCIDList(ReceiverPIDn, VCIDList),
  if
    Result ->
      PIDn ! {vc, Pnum, VCIDList},
      {VCIDList, Counter};
    true ->
      NewCounter = Counter + 1,
      NewVCIDList = append(VCIDList, [{NewCounter, ReceiverPIDn}]),
      PIDn ! {vc, NewCounter, NewVCIDList},
      {NewVCIDList, NewCounter}
  end.

%% Hilfsmethode zu getID
%% liefert {Result,{Pnum,ReceiverPID}} zurueck
findReceiverPIDInVCIDList(ReceiverPID, []) ->
  {false, {0, ReceiverPID}};
findReceiverPIDInVCIDList(ReceiverPID, [{Pnum, ReceiverPIDVCList} | Tail]) ->
  if
    ReceiverPID == ReceiverPIDVCList -> {true, {Pnum, ReceiverPID}};
    true ->
      findReceiverPIDInVCIDList(ReceiverPID, Tail)
  end.

%% Entwurf 2.1.2
%% VTn = {3,[1,2,3]}
%% TimestampList = [{1,[[1],[2]]}, {2,[[1,2],[2,3]]}, {3,[[1,2,3],[2,3,4]]}]
%% LogFile, to log the end result
setTimestamp({Pnum, List}, TimestampList, LogFile) ->
  {Result, {TsLPnum, TsLList}, Position} = findPnumInTimestampList(Pnum, TimestampList, 1),
  if
    Result ->
      SortedPnumList = sortPnumList(TsLPnum, TsLList, List, [], LogFile),
      insertList(Position, Pnum, SortedPnumList, TimestampList, [], 1);
    true ->
      addOrderToTimestampList(TimestampList, {Pnum, List}, [])
  end.

%% Hilfsmethode fuer setTimestamp
%% Ordnet die erstmals erhaltenen VTns an die richtige Position der Liste. Entwurf 4.2
%% TimestampList = [{1,[[1],[2]]}, {2,[[1,2],[2,3]]}, {3,[[1,2,3],[2,3,4]]}]
addOrderToTimestampList([], {Pnum, List}, Result) ->
  append(Result, [{Pnum, [List]}]);
addOrderToTimestampList([{TsLPnum, TsLList} | TsLListTail], {Pnum, List}, Result) when Pnum < TsLPnum ->
  NewResult = append([{Pnum, [List]}], [{TsLPnum, TsLList} | TsLListTail]),
  append(Result, NewResult);
addOrderToTimestampList([{TsLPnum, TsLList} | TsLListTail], {Pnum, List}, Result) ->
  NewResult = append(Result, [{TsLPnum, TsLList}]),
  addOrderToTimestampList(TsLListTail, {Pnum, List}, NewResult).

%% Hilfsmethode fuer setTimestamp
%% liefert {Result, {Pnum,List}, Position} zurueck
findPnumInTimestampList(_Pnum, [], _Position) ->
  {false, {0, []}, 0};
findPnumInTimestampList(Pnum, [{PnumList, List} | Tail], Position) ->
  if
    Pnum == PnumList -> {true, {PnumList, List}, Position};
    true ->
      NewPosition = Position + 1,
      findPnumInTimestampList(Pnum, Tail, NewPosition)
  end.

%% Hilfsmethode fuer setTimestamp
%% sortiert die Liste, die findPnumInTimestampList zurueckliefert
sortPnumList(_Pnum, [], List, Result, _LogFile) ->
  append(Result, [List]);
sortPnumList(Pnum, [TsLListHead | TsLListTail], List, Result, LogFile) ->
  Timestamp = vectorC:compVT({Pnum, List}, {Pnum, TsLListHead}),
  case Timestamp of
    beforeVT ->
      append(append(Result, TsLListHead), TsLListTail);
    equalVT ->
      append(Result, TsLListTail);
    afterVT ->
      sortPnumList(Pnum, TsLListTail, List, append(Result, [TsLListHead]), LogFile);
    concurrentVT ->
      append(Result, TsLListTail)
  end.

%% Hilfsmethode fuer setTimestamp
%% Insert into the List at the Position the VTn is
%% Position = 2
%% VTn= [1,2,3,4,5,6,7]
%% TimestampList = [[1,{1,[1]}],[2,{2,[0,2]}]]
%% NewVTList = insertList(Pnum, List, IncreaseVT, [], 1),
insertList(_Position, _Pnum, _VTn, [], ResultList, _Counter) ->
  ResultList;
insertList(1, Pnum, VTn, [_TimestampListHead | TimestampListTail], [], _Counter) ->
  append([{Pnum, VTn}], TimestampListTail);

insertList(Position, Pnum, VTn, [TimestampListHead | TimestampListTail], ResultList, Counter) ->
  if
    Position == Counter ->
      NewResultList = append(ResultList, [{Pnum, VTn}]),
      insertList(Position, Pnum, VTn, TimestampListTail, NewResultList, Counter + 1);
    true ->
      NewResultList2 = append(ResultList, [TimestampListHead]),
      insertList(Position, Pnum, VTn, TimestampListTail, NewResultList2, Counter + 1)
  end.

append([H | T], Tail) ->
  [H | append(T, Tail)];
append([], Tail) ->
  Tail.

%% Entwurf 2.1.3
%% FromVTm = {3,[1,2,3]}
%% ToVTm = {4,[1,2,3,4]}
%% MessagestampList = [[FromVTm,ToVTn],[{FromVTmPnum, FromVTmList}, {ToVTnPnum, ToVTnList}],...]
setMessagestamp({FromVTmPnum, FromVTmList}, {ToVTnPnum, ToVTnList}, MessagestampList) ->
  append(MessagestampList, [[{FromVTmPnum, FromVTmList}, {ToVTnPnum, ToVTnList}]]).

%% Entwurf 2.1.5
%% Pnum = ID
%% TimestampList = [[1,{1,[[1],[2]]}],[2,{2,[0,2]}]]
printTimeLine(Pnum, TimestampList, LogFile) ->
  [RandomNumberFilename | _Tail] = util:randomliste(100),
  TimeLineFileName = string:concat(string:concat("timeLine", util:to_String(Pnum)), string:concat("_", util:to_String(RandomNumberFilename))),
  {Result, PnumList} = findTimeLineWithPnum(Pnum, TimestampList),
  if
    Result ->
      {ok, File} = file:open(util:attachEnding(TimeLineFileName, dot), [write]),
      io:format(File, "~s", ["digraph TimeLine\n{\n"]),
      printTimeLineList(File, Pnum, PnumList, LogFile),
      io:format(File, "~s", ["}"]),
      util:logging(LogFile, io_lib:format("Datei: ~p erfolgreich geschrieben. \n", [TimeLineFileName]));
    true -> util:logging(LogFile, io_lib:format("Nice try. Pnum:~p nicht gefunden. \n", [Pnum]))
  end.

%% Hilfsmethode fuer printTimeLine
%% Schreibt in dem gewuenschten Format in die Datei fuer die TimeLine
printTimeLineList(File, Pnum, [PnumListHeadFirst, PnumListHeadSecond | PnumListTail], LogFile) ->
  %% sadly wrap after 80 signs
  %% io:format(File, "\"~lp\" ->  \"~lp\" [label = ~p ];~n", [PnumListHeadFirst, PnumListHeadSecond, Pnum]),
  io:format(File, "\"~w\" ->  \"~w\" [label = ~p ];~n", [PnumListHeadFirst, PnumListHeadSecond, Pnum]),
  printTimeLineList(File, Pnum, append([PnumListHeadSecond], PnumListTail), LogFile);

printTimeLineList(_File, _Pnum, [_VT1 | []], _LogFile) -> ok;
printTimeLineList(_File, _Pnum, [], _LogFile) -> ok.

%% Hilfsmethode fuer printTimeLine
%% Liefert die Timestamp Liste des gewuenschten Pnum zurueck
findTimeLineWithPnum(_Pnum, []) ->
  {false, []};
findTimeLineWithPnum(Pnum, [{TimestampListHeadPnum, TimestampListHeadList} | TimestampListTail]) ->
  if
    Pnum == TimestampListHeadPnum -> {true, TimestampListHeadList};
    true -> findTimeLineWithPnum(Pnum, TimestampListTail)
  end.

%% Entwurf 2.1.4
%% Pnum = ID
%% TimestampList = [[1,{1,[[1],[2]]}],[2,{2,[0,2]}]]
%% MessagestampList = [[{FromVTmPnum1, FromVTmList1}, {ToVTnPnum1, ToVTnList1}],[{FromVTmPnum2, FromVTmList2}, {ToVTnPnum2, ToVTnList2}],...]
printMessagestamp(TimestampList, MessagestampList, LogFile) ->
  [RandomNumberFilename | _Tail] = util:randomliste(100),
  TimeMapFileName = string:concat("timeMap", util:to_String(RandomNumberFilename)),
  {ok, File} = file:open(util:attachEnding(TimeMapFileName, dot), [write]),
  io:format(File, "~s", ["digraph TimeMap\n{\n"]),
  printTimeMapTimestampList(File, TimestampList, LogFile),
  printTimeMapMessagestampList(File, MessagestampList, LogFile),
  io:format(File, "~s", ["}"]),
  util:logging(LogFile, io_lib:format("Datei: ~p erfolgreich geschrieben. \n", [TimeMapFileName])).

%% Hilfsmethode fuer printMessagestamo
%% Schreibt in dem gewuenschten Format die TimeLine in die Datei fuer die TimeMap
printTimeMapTimestampList(_File, [], _LogFile) ->
  ok;
printTimeMapTimestampList(File, [{TimestampListHeadPnum, TimestampListHeadList} | TimestampListTail], LogFile) ->
  printTimeLineList(File, TimestampListHeadPnum, TimestampListHeadList, LogFile),
  printTimeMapTimestampList(File, TimestampListTail, LogFile).

%% Hilfsmethode fuer printMessagestamo
%% Schreibt in dem gewuenschten Format die TimeMap in die Datei fuer die TimeMap
printTimeMapMessagestampList(_File, [], _LogFile) ->
  ok;
printTimeMapMessagestampList(File, [MessagestampListHead | MessagestampListTail], LogFile) ->
  printTimeMapList(File, MessagestampListHead),
  printTimeMapMessagestampList(File, MessagestampListTail, LogFile).

printTimeMapList(File, [{FromVTmPnum, FromVTmList}, {ToVTnPnum, ToVTnList}]) ->
  %% sadly wrap after 80 signs
  %%  io:format(File, "\"~lp\" ->  \"~lp\" [label = \"(~p,~p)\"];~n", [FromVTmList, ToVTnList, FromVTmPnum, ToVTnPnum]).
  io:format(File, "\"~w\" ->  \"~w\" [label = \"(~p,~p)\"];~n", [FromVTmList, ToVTnList, FromVTmPnum, ToVTnPnum]).
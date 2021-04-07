%%%-------------------------------------------------------------------
%%% Created : 17. Dez 2020 09:25
%%%-------------------------------------------------------------------
-module(vectorC).

%% API
-export([isVT/1, compVT/2, myVTid/1, myCount/1, initVT/1, tickVT/1, syncVT/2]).

%% Entwurf 4.1.1
isVT({Pnum, _List}) when Pnum =< 0 ->
  false;
isVT({Pnum, List}) ->
  Size = length(List),
  if
    Pnum =< Size -> true;
    true -> false
  end;

isVT(_VT) ->
  false.

%% Entwurf 4.1.2
compVT({_PnumVT1, VT1List}, {_PnumVT2, VT2List}) ->
  compVTLists(VT1List, length(VT1List), VT2List, length(VT2List)).

%% Hilfsmethoden für compVT
%% bringt die beiden Listen auf die gleiche Laenge
compVTLists(ListVT1, ListVT1Length, ListVT2, ListVT2Length) when ListVT1Length < ListVT2Length ->
  PnumsToAppend = ListVT2Length - ListVT1Length,
  NewListVT1 = appendPnums(ListVT1, PnumsToAppend),
  compVTLists(NewListVT1, ListVT2);

compVTLists(ListVT1, ListVT1Length, ListVT2, ListVT2Length) when ListVT1Length == ListVT2Length ->
  compVTLists(ListVT1, ListVT2);

compVTLists(ListVT1, ListVT1Length, ListVT2, ListVT2Length) when ListVT1Length > ListVT2Length ->
  PnumsToAppend = ListVT1Length - ListVT2Length,
  NewListVT2 = appendPnums(ListVT2, PnumsToAppend),
  compVTLists(ListVT1, NewListVT2).

%% Entwurf 4.1.2: Abbildung 9: compVT. Erster Schritt von Start zu den jeweiligen Zustaenden
compVTLists([ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail]) when ListVT1Head < ListVT2Head ->
  compVT1LessVT2(ListVT1Tail, ListVT2Tail);
compVTLists([ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail]) when ListVT1Head == ListVT2Head ->
  compVT1EqlVT2(ListVT1Tail, ListVT2Tail);
compVTLists([ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail]) when ListVT1Head > ListVT2Head ->
  compVT1GreaterVT2(ListVT1Tail, ListVT2Tail).

%% beforeVT Zustand
compVT1LessVT2([], []) ->
  beforeVT;
compVT1LessVT2([ListVT1Head | _ListVT1Tail], [ListVT2Head | _ListVT2Tail]) when ListVT1Head > ListVT2Head ->
  concurrentVT;
compVT1LessVT2([ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail]) when ListVT1Head =< ListVT2Head ->
  compVT1LessVT2(ListVT1Tail, ListVT2Tail).

%% equalVT Zustand
compVT1EqlVT2([], []) ->
  equalVT;
compVT1EqlVT2([ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail]) when ListVT1Head < ListVT2Head ->
  compVT1LessVT2(ListVT1Tail, ListVT2Tail);
compVT1EqlVT2([ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail]) when ListVT1Head > ListVT2Head ->
  compVT1GreaterVT2(ListVT1Tail, ListVT2Tail);
compVT1EqlVT2([ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail]) when ListVT1Head == ListVT2Head ->
  compVT1EqlVT2(ListVT1Tail, ListVT2Tail).

%% afterVT Zustand
compVT1GreaterVT2([], []) ->
  afterVT;
compVT1GreaterVT2([ListVT1Head | _ListVT1Tail], [ListVT2Head | _ListVT2Tail]) when ListVT1Head < ListVT2Head ->
  concurrentVT;
compVT1GreaterVT2([ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail]) when ListVT1Head >= ListVT2Head ->
  compVT1GreaterVT2(ListVT1Tail, ListVT2Tail).


%% Entwurf 4.1.3
myVTid({Pnum, _List}) ->
  Pnum.

%% Entwurf 4.1.4
myCount({Pnum, List}) ->
  if
    Pnum > length(List) -> 0;
    true -> findNElementInList(Pnum, List, 1)
  end.

%% Hilfsmethode zu myCount und tickVT
%% Liefert das Element an Position zurueck mittels Counter Vergleich, der inkrementiert wird
findNElementInList(0, _List, _Counter) ->
  0;
findNElementInList(Position, [Head | Tail], Counter) ->
  if
    Position == Counter -> Head;
    true ->
      findNElementInList(Position, Tail, Counter + 1)
  end.

%% Entwurf 4.1.5
initVT(Pnum) ->
  VTList = createVTList([], Pnum),
  {Pnum, VTList}.

%% Hilfsmethode zu initVT
%% Erzeugt eine Liste mit lauter 0'en in Anzahl Iteration
createVTList(List, 0) ->
  List;
createVTList(List, Iteration) ->
  createVTList([0 | List], Iteration - 1).

%% Entwurf 4.1.6
tickVT({Pnum, List}) ->
  if
    Pnum =< length(List) ->
      CurrentVT = findNElementInList(Pnum, List, 1),
      IncreaseVT = CurrentVT + 1,
      NewVTList = insertIntoVTList(Pnum, List, IncreaseVT, [], 1),
      {Pnum, NewVTList};
    true ->
      {Pnum, List}
  end.

%% Hilfsmethode zu tickVT
%% Setzt den erhoehten VT an die gewuenschte Stelle(Pnum)
insertIntoVTList(_Pnum, [], _IncreaseVT, NewVTList, _Counter) ->
  NewVTList;
insertIntoVTList(0, List, _IncreaseVT, _NewVTList, _Counter) ->
  List;
insertIntoVTList(Pnum, [Head | Tail], IncreaseVT, NewVTList, Counter) ->
  if
    Pnum == Counter ->
      VTList = append(NewVTList, [IncreaseVT]),
      insertIntoVTList(Pnum, Tail, IncreaseVT, VTList, Counter + 1);
    true ->
      VTList2 = append(NewVTList, [Head]),
      insertIntoVTList(Pnum, Tail, IncreaseVT, VTList2, Counter + 1)
  end.

append([H | T], Tail) ->
  [H | append(T, Tail)];
append([], Tail) ->
  Tail.

%% Entwurf 4.1.7
syncVT({Pnum, ListVT1}, {_Pnum, ListVT2}) ->
  ListVT1Length = length(ListVT1),
  ListVT2Length = length(ListVT2),
  VTList = syncVTLists(Pnum, ListVT1, ListVT1Length, ListVT2, ListVT2Length),
  {Pnum, VTList}.

%% Hilfsmethoden zu syncVT
%% Bringt die beiden VT Listen auf die gleiche Laenge
syncVTLists(Pnum, ListVT1, ListVT1Length, ListVT2, ListVT2Length) when ListVT1Length == ListVT2Length ->
  syncVTL1VTL2(Pnum, ListVT1, ListVT2, [], length(ListVT1));
syncVTLists(Pnum, ListVT1, ListVT1Length, ListVT2, ListVT2Length) when ListVT1Length < ListVT2Length ->
  PnumsToAppend = ListVT2Length - ListVT1Length,
  NewListVT1 = appendPnums(ListVT1, PnumsToAppend),
  syncVTL1VTL2(Pnum, NewListVT1, ListVT2, [], length(ListVT2));
syncVTLists(Pnum, ListVT1, ListVT1Length, ListVT2, ListVT2Length) when ListVT1Length > ListVT2Length ->
  PnumsToAppend = ListVT1Length - ListVT2Length,
  NewListVT2 = appendPnums(ListVT2, PnumsToAppend),
  syncVTL1VTL2(Pnum, ListVT1, NewListVT2, [], length(ListVT1)).

%% Hilfsmethode fuer synctVT und compVT
%% Haengt bei Differenz 0'en an
appendPnums(ListVT, 0) ->
  ListVT;
appendPnums(ListVT, PnumsToAppend) ->
  NewList = append(ListVT, [0]),
  appendPnums(NewList, PnumsToAppend - 1).

%% Entwurf 4.1.7
syncVTL1VTL2(_Pnum, _ListVT1, _ListVT2, Result, 0) ->
  Result;
%% Ausnahme des eigenen Pnums
syncVTL1VTL2(Pnum, [ListVT1Head | ListVT1Tail], [_ListVT2Head | ListVT2Tail], Result, Iterator) when Pnum == 1 ->
  VTList = append(Result, [ListVT1Head]),
  syncVTL1VTL2(Pnum - 1, ListVT1Tail, ListVT2Tail, VTList, Iterator - 1);
syncVTL1VTL2(Pnum, [ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail], Result, Iterator) when ListVT1Head < ListVT2Head ->
  VTList = append(Result, [ListVT2Head]),
  syncVTL1VTL2(Pnum - 1, ListVT1Tail, ListVT2Tail, VTList, Iterator - 1);
syncVTL1VTL2(Pnum, [ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail], Result, Iterator) when ListVT1Head == ListVT2Head ->
  VTList = append(Result, [ListVT2Head]),
  syncVTL1VTL2(Pnum - 1, ListVT1Tail, ListVT2Tail, VTList, Iterator - 1);
syncVTL1VTL2(Pnum, [ListVT1Head | ListVT1Tail], [ListVT2Head | ListVT2Tail], Result, Iterator) when ListVT1Head > ListVT2Head ->
  VTList = append(Result, [ListVT1Head]),
  syncVTL1VTL2(Pnum - 1, ListVT1Tail, ListVT2Tail, VTList, Iterator - 1).
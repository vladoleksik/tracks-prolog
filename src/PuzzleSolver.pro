inputFile('./puzzle.txt').
outputFile('./puzzle_output.txt').

%--------------------------------- VALIDATION ___________________________________
validate(T) :- checkTooManyCert(T), !, checkTooFewPoss(T),!, checkAdjacency(T),!, checkLoops(T).

checkTooManyCert(T) :- T=tracks(W,H,_,_,_), checkCertOnLines(T,H),!, checkCertOnCols(T,W).

checkCertOnLines(_,0).
checkCertOnLines(T,I) :- countCertLine(T, I, X), getLineHint(T, I, X2), X=<X2,!, NextI is I-1, checkCertOnLines(T,NextI).


checkCertOnCols(_,0).
checkCertOnCols(T,J) :- countCertCol(T, J, X), getColHint(T, J, X2), X=<X2,!, NextJ is J-1, checkCertOnCols(T,NextJ).

checkTooFewPoss(T) :- T=tracks(W,H,_,_,_), checkImpossOnLines(T,H),!, checkImpossOnCols(T,W).

checkImpossOnLines(_,0).
checkImpossOnLines(T,I) :- T=tracks(W,_,_,_,_), countImpossLine(T, I, X), getLineHint(T, I, X2), Poss is W-X, X2=<Poss,!, NextI is I-1, checkImpossOnLines(T,NextI).


checkImpossOnCols(_,0).
checkImpossOnCols(T,J) :- T=tracks(_,H,_,_,_), countImpossCol(T, J, X), getColHint(T, J, X2), Poss is H-X, X2=<Poss,!, NextJ is J-1, checkImpossOnCols(T,NextJ).


checkAdjacency(T) :- T=tracks(W,H,_,_,_), checkAdjacencyOnLines(T,H),!, checkAdjacencyOnCols(T,W).

checkAdjacencyOnLines(_,0).
checkAdjacencyOnLines(T,I) :- checkAdjOnLine(T,I), NextI is I-1,!, checkAdjacencyOnLines(T,NextI).

checkAdjacencyOnCols(_,0).
checkAdjacencyOnCols(T,J) :- checkAdjOnCol(T,J), NextJ is J-1,!, checkAdjacencyOnCols(T,NextJ).

checkAdjOnLine(T, I) :- T=tracks(W,H,_,_,C), getLine(W,H,I,1,C,L), checkAdjH(L).
checkAdjOnCol(T, J) :- T=tracks(W,H,_,_,C), getCol(W,H,1,J,C,L), checkAdjV(L).

checkAdjH([]).
checkAdjH([_]).
checkAdjH([C1|R]) :- R = [C2|_], checkCompatH(C1,C2),!, checkAdjH(R).

checkAdjV([]).
checkAdjV([_]).
checkAdjV([C1|R]) :- R = [C2|_], checkCompatV(C1,C2),!, checkAdjV(R).


checkCompatH(X,Y) :- (cellKnown(X), cellKnown(Y), ((isPointingToCellFromW(X), isPointingToCellFromE(Y));(not(isPointingToCellFromW(X)), not(isPointingToCellFromE(Y)))));(cellKnown(X),not(cellKnown(Y)),((isPointingToCellFromW(X),not(not(Y=cell(_,_,cert(_)))));not(isPointingToCellFromW(X))));(cellKnown(Y),not(cellKnown(X)),((isPointingToCellFromE(Y),not(not(X=cell(_,_,cert(_)))));not(isPointingToCellFromE(Y))));(not(cellKnown(X)), not(cellKnown(Y))).

checkCompatV(X,Y) :- (cellKnown(X), cellKnown(Y), ((isPointingToCellFromN(X), isPointingToCellFromS(Y));(not(isPointingToCellFromN(X)), not(isPointingToCellFromS(Y)))));(cellKnown(X),not(cellKnown(Y)),((isPointingToCellFromN(X),not(not(Y=cell(_,_,cert(_)))));not(isPointingToCellFromN(X))));(cellKnown(Y),not(cellKnown(X)),((isPointingToCellFromS(Y),not(not(X=cell(_,_,cert(_)))));not(isPointingToCellFromS(Y))));(not(cellKnown(X)), not(cellKnown(Y))).


checkLoops(T) :- T=tracks(W,H,_,_,_), checkTrackLoops(T,W,H).

checkTrackLoops(_,_,0).
checkTrackLoops(T,J,I) :- I>0, checkLoopsLine(T,J,I), !, NextI is I-1, checkTrackLoops(T,J,NextI).

checkLoopsLine(_,0,_).
checkLoopsLine(T,J,I) :- J>0, checkLoopsPos(T,I,J), !, NextJ is J-1, checkLoopsLine(T,NextJ, I).

checkLoopsPos(T,I,J) :- T=tracks(_,_,_,_,L), getCell(I,J,L,C), not(cellKnown(C)).
checkLoopsPos(T,I,J) :- T=tracks(W,H,_,_,L), getCell(I,J,L,C), cellKnown(C), !, startPass(H,W,L,C).

startPass(_,_,_,cell(_,J,cert(nw))) :- J=<1.
startPass(H,W,L,cell(I,J,cert(nw))) :- I>0, J>1, I=<H, J=<W, PrevJ is J-1, visitLoop(H,W,L,cell(I,J,cert(nw)),0,-1,I,PrevJ).

startPass(_,_,_,cell(_,J,cert(sw))) :- J=<1.
startPass(H,W,L,cell(I,J,cert(sw))) :- I>0, J>1, I=<H, J=<W, PrevJ is J-1, visitLoop(H,W,L,cell(I,J,cert(sw)),0,-1,I,PrevJ).

startPass(_,_,_,cell(_,J,cert(horiz))) :- J=<1.
startPass(H,W,L,cell(I,J,cert(horiz))) :- I>0, J>1, I=<H, J=<W, PrevJ is J-1, visitLoop(H,W,L,cell(I,J,cert(horiz)),0,-1,I,PrevJ).

startPass(_,W,_,cell(_,J,cert(ne))) :- J>=W.
startPass(H,W,L,cell(I,J,cert(ne))) :- I>0, J>0, I=<H, J<W, PrevJ is J+1, visitLoop(H,W,L,cell(I,J,cert(ne)),0,1,I,PrevJ).

startPass(_,W,_,cell(_,J,cert(se))) :- J>=W.
startPass(H,W,L,cell(I,J,cert(se))) :- I>0, J>0, I=<H, J<W, PrevJ is J+1, visitLoop(H,W,L,cell(I,J,cert(se)),0,1,I,PrevJ).

startPass(_,_,_,cell(I,_,cert(vert))) :- I=<1.
startPass(H,W,L,cell(I,J,cert(vert))) :- I>1, J>0, I=<H, J=<W, PrevI is I-1, visitLoop(H,W,L,cell(I,J,cert(vert)),-1,0,PrevI,J).



visitLoop(H,W,_,cell(I,J,_),_,_,_,_) :- I=<0; J=<0; I>H; J>W.
visitLoop(H,W,_,cell(I,J,_),_,_,I,J) :- I>0, J>0, I=<H, J=<W, !, fail.

visitLoop(_,_,_,cell(I,_,cert(nw)),0,-1,_,_) :- I=<1.
visitLoop(_,_,L,cell(I,J,cert(nw)),0,-1,_,_) :- I>1, NextI is I-1, getCell(NextI,J,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(nw)),0,-1,SI,SJ) :- I>1, NextI is I-1, getCell(NextI,J,L,C), cellKnown(C),!, visitLoop(H,W,L,C,1,0,SI,SJ).

visitLoop(_,_,_,cell(_,J,cert(nw)),-1,0,_,_) :- J=<1.
visitLoop(_,_,L,cell(I,J,cert(nw)),-1,0,_,_) :- J>1, NextJ is J-1, getCell(I,NextJ,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(nw)),-1,0,SI,SJ) :- J>1, NextJ is J-1, getCell(I,NextJ,L,C), cellKnown(C),!, visitLoop(H,W,L,C,0,1,SI,SJ).


visitLoop(H,_,_,cell(I,_,cert(sw)),0,-1,_,_) :- I>=H.
visitLoop(H,_,L,cell(I,J,cert(sw)),0,-1,_,_) :- I<H, NextI is I+1, getCell(NextI,J,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(sw)),0,-1,SI,SJ) :- I<H, NextI is I+1, getCell(NextI,J,L,C), cellKnown(C),!, visitLoop(H,W,L,C,-1,0,SI,SJ).

visitLoop(_,_,_,cell(_,J,cert(sw)),1,0,_,_) :- J=<1.
visitLoop(_,_,L,cell(I,J,cert(sw)),1,0,_,_) :- J>1, NextJ is J-1, getCell(I,NextJ,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(sw)),1,0,SI,SJ) :- J>1, NextJ is J-1, getCell(I,NextJ,L,C), cellKnown(C),!, visitLoop(H,W,L,C,0,1,SI,SJ).


visitLoop(_,_,_,cell(I,_,cert(ne)),0,1,_,_) :- I=<1.
visitLoop(_,_,L,cell(I,J,cert(ne)),0,1,_,_) :- I>1, NextI is I-1, getCell(NextI,J,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(ne)),0,1,SI,SJ) :- I>1, NextI is I-1, getCell(NextI,J,L,C), cellKnown(C),!, visitLoop(H,W,L,C,1,0,SI,SJ).

visitLoop(_,W,_,cell(_,J,cert(ne)),-1,0,_,_) :- J>=W.
visitLoop(_,W,L,cell(I,J,cert(ne)),-1,0,_,_) :- J<W, NextJ is J+1, getCell(I,NextJ,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(ne)),-1,0,SI,SJ) :- J<W, NextJ is J+1, getCell(I,NextJ,L,C), cellKnown(C),!, visitLoop(H,W,L,C,0,-1,SI,SJ).


visitLoop(H,_,_,cell(I,_,cert(se)),0,1,_,_) :- I>=H.
visitLoop(H,_,L,cell(I,J,cert(se)),0,1,_,_) :- I<H, NextI is I+1, getCell(NextI,J,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(se)),0,1,SI,SJ) :- I<H, NextI is I+1, getCell(NextI,J,L,C), cellKnown(C),!, visitLoop(H,W,L,C,-1,0,SI,SJ).

visitLoop(_,W,_,cell(_,J,cert(se)),1,0,_,_) :- J>=W.
visitLoop(_,W,L,cell(I,J,cert(se)),1,0,_,_) :- J<W, NextJ is J+1, getCell(I,NextJ,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(se)),1,0,SI,SJ) :- J<W, NextJ is J+1, getCell(I,NextJ,L,C), cellKnown(C),!, visitLoop(H,W,L,C,0,-1,SI,SJ).


visitLoop(_,_,_,cell(_,J,cert(horiz)),0,1,_,_) :- J=<1.
visitLoop(_,_,L,cell(I,J,cert(horiz)),0,1,_,_) :- J>1, NextJ is J-1, getCell(I,NextJ,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(horiz)),0,1,SI,SJ) :- J>1, NextJ is J-1, getCell(I,NextJ,L,C), cellKnown(C),!, visitLoop(H,W,L,C,0,1,SI,SJ).

visitLoop(_,W,_,cell(_,J,cert(horiz)),0,-1,_,_) :- J>=W.
visitLoop(_,W,L,cell(I,J,cert(horiz)),0,-1,_,_) :- J<W, NextJ is J+1, getCell(I,NextJ,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(horiz)),0,-1,SI,SJ) :- J<W, NextJ is J+1, getCell(I,NextJ,L,C), cellKnown(C),!, visitLoop(H,W,L,C,0,-1,SI,SJ).


visitLoop(_,_,_,cell(I,_,cert(vert)),1,0,_,_) :- I=<1.
visitLoop(_,_,L,cell(I,J,cert(vert)),1,0,_,_) :- I>1, NextI is I-1, getCell(NextI,J,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(vert)),1,0,SI,SJ) :- I>1, NextI is I-1, getCell(NextI,J,L,C), cellKnown(C),!, visitLoop(H,W,L,C,1,0,SI,SJ).

visitLoop(H,_,_,cell(I,_,cert(vert)),-1,0,_,_) :- I>=H.
visitLoop(H,_,L,cell(I,J,cert(vert)),-1,0,_,_) :- I<H, NextI is I+1, getCell(NextI,J,L,C), not(cellKnown(C)).
visitLoop(H,W,L,cell(I,J,cert(vert)),-1,0,SI,SJ) :- I<H, NextI is I+1, getCell(NextI,J,L,C), cellKnown(C),!, visitLoop(H,W,L,C,-1,0,SI,SJ).

%---------------------------------- INFERENCE ___________________________________
applyRules(T) :- applyAdjacencyRule(T,R3), !, applyNoTracksRemainingRule(T,R1),!,applyOnlyTracksRemainingRule(T,R2),!,applyLessThanTwoNeighborsRule(T,R6),!,applyTwoGoodNeighborsRule(T,R4),!,applyTwoGoodConnNeighborsRule(T,R5),!,applyBottleneckRule(T,R7),!, R is sign(R1+R2+R3+R4+R5+R6+R7),!, validate(T), !,continueRules(T,R).

continueRules(_,0).
continueRules(T,1) :- applyRules(T).

applyNoTracksRemainingRule(T, R):- T=tracks(W,H,_,_,_), applyNoTracksRemainingLines(T,H, R1), !, applyNoTracksRemainingCols(T,W,R2),!, R is sign(R1+R2).

applyNoTracksRemainingLines(_,0,0).
applyNoTracksRemainingLines(T,I,R) :- I>0, applyNoTracksRemainingLine(T,I,R1), NextI is I-1,!, applyNoTracksRemainingLines(T, NextI,R2),!, R is sign(R1+R2).

applyNoTracksRemainingCols(_,0,0).
applyNoTracksRemainingCols(T,J,R) :- J>0, applyNoTracksRemainingCol(T,J,R1), NextJ is J-1,!, applyNoTracksRemainingCols(T, NextJ,R2),!, R is sign(R1+R2).

applyNoTracksRemainingLine(T,I,0) :- countCertLine(T, I, X), getLineHint(T, I, X2), X<X2,!.
applyNoTracksRemainingLine(T,I,R) :- countCertLine(T, I, X), getLineHint(T, I, X2), X==X2,!, setLineImposs(T,I,R),!.

applyNoTracksRemainingCol(T,J,0) :- countCertCol(T, J, X), getColHint(T, J, X2), X<X2,!.
applyNoTracksRemainingCol(T,J,R) :- countCertCol(T, J, X), getColHint(T, J, X2), X==X2,!, 
setColImposs(T,J,R),!.



applyOnlyTracksRemainingRule(T, R):- T=tracks(W,H,_,_,_), applyOnlyTracksRemainingLines(T,H, R1), !, applyOnlyTracksRemainingCols(T,W,R2),!, R is sign(R1+R2).

applyOnlyTracksRemainingLines(_,0,0).
applyOnlyTracksRemainingLines(T,I,R) :- I>0, applyOnlyTracksRemainingLine(T,I,R1), NextI is I-1,!, applyOnlyTracksRemainingLines(T, NextI,R2), R is sign(R1+R2),!.

applyOnlyTracksRemainingCols(_,0,0).
applyOnlyTracksRemainingCols(T,J,R) :- J>0, applyOnlyTracksRemainingCol(T,J,R1), NextJ is J-1,!, applyOnlyTracksRemainingCols(T, NextJ,R2), R is sign(R1+R2),!.

applyOnlyTracksRemainingLine(T,I,0) :- T=tracks(W,_,_,_,_), countImpossLine(T, I, X), getLineHint(T, I, X2), Poss is W-X, X2<Poss,!.
applyOnlyTracksRemainingLine(T,I,R) :- T=tracks(W,_,_,_,_), countImpossLine(T, I, X), getLineHint(T, I, X2), Poss is W-X, X2==Poss,!, setLineCert(T,I,R),!.

applyOnlyTracksRemainingCol(T,J,0) :- T=tracks(_,H,_,_,_), countImpossCol(T, J, X), getColHint(T, J, X2), Poss is H-X, X2<Poss,!.
applyOnlyTracksRemainingCol(T,J,R) :- T=tracks(_,H,_,_,_), countImpossCol(T, J, X), getColHint(T, J, X2), Poss is H-X, X2==Poss,!, setColCert(T,J,R),!.



applyAdjacencyRule(T,R) :- T=tracks(_,H,_,_,_), applyAdjacencyRuleLines(T,H,R),!.

applyAdjacencyRuleLines(_,0,0).
applyAdjacencyRuleLines(T,I,R) :- I>0, applyAdjacencyLine(T,I,R1), NextI is I-1, !, applyAdjacencyRuleLines(T,NextI,R2), R is sign(R1+R2).

applyAdjacencyLine(T,I,R) :- T=tracks(W,_,_,_,_), applyAdjacencyPositions(T,I,W,R),!.

applyAdjacencyPositions(_,_,0,0).
applyAdjacencyPositions(T,I,J,R) :- J>0, applyAdjPos(T,I,J,R1),!, NextJ is J-1, applyAdjacencyPositions(T,I,NextJ,R2), R is sign(R1+R2).

applyAdjPos(T,I,J,0) :- T=tracks(_,_,_,_,L), getCell(I,J,L,C), not(cellKnown(C)).
applyAdjPos(T,I,J,R) :- T=tracks(_,_,_,_,L), getCell(I,J,L,C), cellKnown(C),!, applyAdjRule(T,C,R).

applyAdjRule(T,cell(I,J,cert(nw)),R) :- ((I=<1, R1=0); (I>1,I2 is I-1,getCellSafe(T,I2,J,C),not(cellImposs(C)), setCellCert(C,R1))),!,((J=<1,R2=0);(J>1,J2 is J-1,getCellSafe(T,I,J2,C2),not(cellImposs(C2)),setCellCert(C2,R2))), R is sign(R1+R2).

applyAdjRule(T,cell(I,J,cert(ne)),R) :- T=tracks(W,_,_,_,_),((I=<1, R1=0); (I>1,I2 is I-1,getCellSafe(T,I2,J,C),not(cellImposs(C)), setCellCert(C,R1))),!,((J>=W,R2=0);(J<W,J2 is J+1,getCellSafe(T,I,J2,C2),not(cellImposs(C2)),setCellCert(C2,R2))),R is sign(R1+R2).

applyAdjRule(T,cell(I,J,cert(sw)),R) :- T=tracks(_,H,_,_,_),((I>=H, R1=0); (I<H,I2 is I+1,getCellSafe(T,I2,J,C),not(cellImposs(C)), setCellCert(C,R1))),!,((J=<1,R2=0);(J>1,J2 is J-1,getCellSafe(T,I,J2,C2),not(cellImposs(C2)),setCellCert(C2,R2))),R is sign(R1+R2).

applyAdjRule(T,cell(I,J,cert(se)),R) :- T=tracks(W,H,_,_,_),((I>=H, R1=0); (I<H,I2 is I+1,getCellSafe(T,I2,J,C),not(cellImposs(C)), setCellCert(C,R1))),!,((J>=W,R2=0);(J<W,J2 is J+1,getCellSafe(T,I,J2,C2),not(cellImposs(C2)),setCellCert(C2,R2))),R is sign(R1+R2).

applyAdjRule(T,cell(I,J,cert(vert)),R) :- T=tracks(_,H,_,_,_),((I>=H, R1=0); (I<H,I2 is I+1,getCellSafe(T,I2,J,C),not(cellImposs(C)), setCellCert(C,R1))),!,((I=<1,R2=0);(I>1,I3 is I-1,getCellSafe(T,I3,J,C2),not(cellImposs(C2)),setCellCert(C2,R2))),R is sign(R1+R2).

applyAdjRule(T,cell(I,J,cert(horiz)),R) :- T=tracks(W,_,_,_,_),((J>=W, R1=0); (J<W,J2 is J+1,getCellSafe(T,I,J2,C),not(cellImposs(C)), setCellCert(C,R1))),!,((J=<1,R2=0);(J>1,J3 is J-1,getCellSafe(T,I,J3,C2),not(cellImposs(C2)),setCellCert(C2,R2))),R is sign(R1+R2).



applyTwoGoodNeighborsRule(T,R) :- T=tracks(_,H,_,_,_), applyTwoGoodNeighborsRuleLines(T,H,R),!.

applyTwoGoodNeighborsRuleLines(_,0,0).
applyTwoGoodNeighborsRuleLines(T,I,R) :- I>0, applyTwoConnectingCellsLine(T,I,R1), NextI is I-1, !, applyTwoGoodNeighborsRuleLines(T,NextI,R2), R is sign(R1+R2).

applyTwoConnectingCellsLine(T,I,R) :- T=tracks(W,_,_,_,_), applyTwoConnectingCellsPositions(T,I,W,R),!.

applyTwoConnectingCellsPositions(_,_,0,0).
applyTwoConnectingCellsPositions(T,I,J,R) :- J>0, applyTwoConnPos(T,I,J,R1),!, NextJ is J-1, applyTwoConnectingCellsPositions(T,I,NextJ,R2), R is sign(R1+R2).

applyTwoConnPos(T,I,J,0) :- T=tracks(_,_,_,_,L), getCell(I,J,L,C), not(cellCert(C)).
applyTwoConnPos(T,I,J,0) :- T=tracks(_,_,_,_,L), getCell(I,J,L,C), cellCert(C), cellKnown(C).
applyTwoConnPos(T,I,J,R) :- T=tracks(_,_,_,_,L), getCell(I,J,L,C), cellCert(C), not(cellKnown(C)), applyTwoConnRule(T,C,R).

applyTwoConnRule(T,cell(I,J,_),0) :- IAbove is I-1, IBelow is I+1, JLeft is J-1, JRight is J+1, getCellSafe(T,IAbove,J,N1), getCellSafe(T,IBelow,J,N2), getCellSafe(T,I,JLeft,N3), getCellSafe(T,I,JRight,N4), countPossibleConnectionN(N1,C1), countPossibleConnectionS(N2,C2), countPossibleConnectionW(N3,C3), countPossibleConnectionE(N4,C4), S is C1+C2+C3+C4, S\==2.

applyTwoConnRule(T,cell(I,J,X),R) :- IAbove is I-1, IBelow is I+1, JLeft is J-1, JRight is J+1, getCellSafe(T,IAbove,J,N1), getCellSafe(T,IBelow,J,N2), getCellSafe(T,I,JLeft,N3), getCellSafe(T,I,JRight,N4), countPossibleConnectionN(N1,C1), countPossibleConnectionS(N2,C2), countPossibleConnectionW(N3,C3), countPossibleConnectionE(N4,C4), S is C1+C2+C3+C4, S==2, fillCell(X,C1,C2,C3,C4,R).



applyTwoGoodConnNeighborsRule(T,R) :- T=tracks(_,H,_,_,_), applyTwoGoodConnNeighborsRuleLines(T,H,R),!.

applyTwoGoodConnNeighborsRuleLines(_,0,0).
applyTwoGoodConnNeighborsRuleLines(T,I,R) :- I>0, applyTwoKnownConnectingCellsLine(T,I,R1),!, NextI is I-1, !, applyTwoGoodConnNeighborsRuleLines(T,NextI,R2), R is sign(R1+R2).

applyTwoKnownConnectingCellsLine(T,I,R) :- T=tracks(W,_,_,_,_), applyTwoKnownConnectingCellsPositions(T,I,W,R),!.

applyTwoKnownConnectingCellsPositions(_,_,0,0).
applyTwoKnownConnectingCellsPositions(T,I,J,R) :- J>0, applyTwoKnownConnPos(T,I,J,R1),!, NextJ is J-1, !, applyTwoKnownConnectingCellsPositions(T,I,NextJ,R2), R is sign(R1+R2).

applyTwoKnownConnPos(T,I,J,0) :- T=tracks(_,_,_,_,L), getCell(I,J,L,C), cellKnown(C).
applyTwoKnownConnPos(T,I,J,R) :- T=tracks(_,_,_,_,L), getCell(I,J,L,C), not(cellKnown(C)), applyTwoKnownConnRule(T,C,R).

applyTwoKnownConnRule(T,cell(I,J,_),0) :- IAbove is I-1, IBelow is I+1, JLeft is J-1, JRight is J+1, getCellSafe(T,IAbove,J,N1), getCellSafe(T,IBelow,J,N2), getCellSafe(T,I,JLeft,N3), getCellSafe(T,I,JRight,N4), countCertainConnectionN(N1,C1), countCertainConnectionS(N2,C2), countCertainConnectionW(N3,C3), countCertainConnectionE(N4,C4), S is C1+C2+C3+C4, S\==2.

applyTwoKnownConnRule(T,cell(I,J,X),R) :- IAbove is I-1, IBelow is I+1, JLeft is J-1, JRight is J+1, getCellSafe(T,IAbove,J,N1), getCellSafe(T,IBelow,J,N2), getCellSafe(T,I,JLeft,N3), getCellSafe(T,I,JRight,N4), countCertainConnectionN(N1,C1), countCertainConnectionS(N2,C2), countCertainConnectionW(N3,C3), countCertainConnectionE(N4,C4), S is C1+C2+C3+C4, S==2, fillCell(X,C1,C2,C3,C4,R).



fillCell(X,0,0,1,1,1) :- X=cert(horiz),!.
fillCell(X,0,1,0,1,1) :- X=cert(se),!.
fillCell(X,0,1,1,0,1) :- X=cert(sw),!.
fillCell(X,1,0,0,1,1) :- X=cert(ne),!.
fillCell(X,1,0,1,0,1) :- X=cert(nw),!.
fillCell(X,1,1,0,0,1) :- X=cert(vert),!.
fillCell(_,_,_,_,_,0).


applyLessThanTwoNeighborsRule(T,R) :- T=tracks(_,H,_,_,_), applyLessThanTwoNeighborsRuleLines(T,H,R),!.

applyLessThanTwoNeighborsRuleLines(_,0,0).
applyLessThanTwoNeighborsRuleLines(T,I,R) :- I>0, applyLessThanTwoConnectingCellsLine(T,I,R1),!, NextI is I-1, !, applyLessThanTwoNeighborsRuleLines(T,NextI,R2), R is sign(R1+R2).

applyLessThanTwoConnectingCellsLine(T,I,R) :- T=tracks(W,_,_,_,_), applyLessThanTwoConnectingCellsPositions(T,I,W,R),!.

applyLessThanTwoConnectingCellsPositions(_,_,0,0).
applyLessThanTwoConnectingCellsPositions(T,I,J,R) :- J>0, applyLessThanTwoConnPos(T,I,J,R1),!, NextJ is J-1, !, applyLessThanTwoConnectingCellsPositions(T,I,NextJ,R2), R is sign(R1+R2).

applyLessThanTwoConnPos(T,I,J,0) :- T=tracks(_,_,_,_,L), getCell(I,J,L,C), cellImposs(C),!.
applyLessThanTwoConnPos(T,I,J,0) :- T=tracks(W,H,_,_,L), getCell(I,J,L,C), not(cellImposs(C)), C=cell(_,_,X), cellKnown(C), isStartEndCell(I,J,W,H,X),!.
applyLessThanTwoConnPos(T,I,J,R) :- T=tracks(W,H,_,_,L), getCell(I,J,L,C), not(cellImposs(C)), C=cell(_,_,X), (not(cellKnown(C));(cellKnown(C),not(isStartEndCell(I,J,W,H,X)))),!, applyLessThanTwoConnRule(T,C,R),!.

applyLessThanTwoConnRule(T,cell(I,J,_),0) :- IAbove is I-1, IBelow is I+1, JLeft is J-1, JRight is J+1, getCellSafe(T,IAbove,J,N1), getCellSafe(T,IBelow,J,N2), getCellSafe(T,I,JLeft,N3), getCellSafe(T,I,JRight,N4), countPossibleConnectionN(N1,C1), countPossibleConnectionS(N2,C2), countPossibleConnectionW(N3,C3), countPossibleConnectionE(N4,C4), S is C1+C2+C3+C4, S>=2,!.

applyLessThanTwoConnRule(T,cell(I,J,X),1) :- IAbove is I-1, IBelow is I+1, JLeft is J-1, JRight is J+1, getCellSafe(T,IAbove,J,N1), getCellSafe(T,IBelow,J,N2), getCellSafe(T,I,JLeft,N3), getCellSafe(T,I,JRight,N4), countPossibleConnectionN(N1,C1), countPossibleConnectionS(N2,C2), countPossibleConnectionW(N3,C3), countPossibleConnectionE(N4,C4), S is C1+C2+C3+C4, S<2, X=imposs,!.


applyBottleneckRule(T,R) :- startPoint(C), T=tracks(W,H,Hc,Hl,_), !, startNavigateB(T,W,H,C,Ll, Lc), !, applyBRuleLines(T,Ll,Hl,R1),!, applyBRuleCols(T,Lc,Hc,R2), R is sign(R1+R2),!.

startNavigateB(T,W,H,cell(1,JS,cert(nw)),Ll,Lc) :- NextJ is JS-1, getCellSafe(T,1,NextJ,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,0,1,OLl,OLc),incrementList(OLl,1,Ll), incrementList(OLc, JS, Lc).
startNavigateB(T,W,H,cell(IS,1,cert(nw)),Ll,Lc) :- NextI is IS-1, getCellSafe(T,NextI,1,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,1,0,OLl,OLc),incrementList(OLl,IS,Ll), incrementList(OLc, 1, Lc).
startNavigateB(T,W,H,cell(1,JS,cert(ne)),Ll,Lc) :- NextJ is JS+1, getCellSafe(T,1,NextJ,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,0,-1,OLl,OLc),incrementList(OLl,1,Ll), incrementList(OLc, JS, Lc).
startNavigateB(T,W,H,cell(IS,W,cert(ne)),Ll,Lc) :- NextI is IS-1, getCellSafe(T,NextI,W,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,1,0,OLl,OLc),incrementList(OLl,IS,Ll), incrementList(OLc, W, Lc).
startNavigateB(T,W,H,cell(H,JS,cert(se)),Ll,Lc) :- NextJ is JS+1, getCellSafe(T,H,NextJ,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,0,-1,OLl,OLc),incrementList(OLl,H,Ll), incrementList(OLc, JS, Lc).
startNavigateB(T,W,H,cell(IS,W,cert(se)),Ll,Lc) :- NextI is IS+1, getCellSafe(T,NextI,W,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,-1,0,OLl,OLc),incrementList(OLl,IS,Ll), incrementList(OLc, W, Lc).
startNavigateB(T,W,H,cell(H,JS,cert(sw)),Ll,Lc) :- NextJ is JS-1, getCellSafe(T,H,NextJ,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,0,1,OLl,OLc),incrementList(OLl,H,Ll), incrementList(OLc, JS, Lc).
startNavigateB(T,W,H,cell(IS,1,cert(sw)),Ll,Lc) :- NextI is IS+1, getCellSafe(T,NextI,1,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,-1,0,OLl,OLc),incrementList(OLl,IS,Ll), incrementList(OLc, 1, Lc).
startNavigateB(T,W,H,cell(1,JS,cert(vert)),Ll,Lc) :- NextI is 1+1, getCellSafe(T,NextI,JS,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,-1,0,OLl,OLc),incrementList(OLl,1,Ll), incrementList(OLc, JS, Lc).
startNavigateB(T,W,H,cell(H,JS,cert(vert)),Ll,Lc) :- NextI is H-1, getCellSafe(T,NextI,JS,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,1,0,OLl,OLc),incrementList(OLl,H,Ll), incrementList(OLc, JS, Lc).
startNavigateB(T,W,H,cell(IS,1,cert(horiz)),Ll,Lc) :- NextJ is 1+1, getCellSafe(T,IS,NextJ,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,0,-1,OLl,OLc),incrementList(OLl,IS,Ll), incrementList(OLc, 1, Lc).
startNavigateB(T,W,H,cell(IS,W,cert(horiz)),Ll,Lc) :- NextJ is W-1, getCellSafe(T,IS,NextJ,C), T=tracks(_,_,_,_,L),!, navigateB(T,H,W,L,C,0,1,OLl,OLc),incrementList(OLl,IS,Ll), incrementList(OLc, W, Lc).

%incrementList(L,I,Nl) :- append(P,[pair(I,X)],R), append(R,S,L), NX is X+1, append(P,[pair(I,NX)],Q), append(Q, S, Nl),!.

incrementList(L,I,Nl) :- maplist(condinc(I),L,Nl).

condinc(I,pair(I,X),pair(I,Y)) :- Y is X+1.
condinc(I,pair(K,X),pair(K,X)) :- K\==I.

navigateB(_,_,_,_,C,_,_,Ll,Lc) :- width(W), height(H), not(cellKnown(C)),findall(pair(X,0),between(1,H,X),Ll),findall(pair(X,0),between(1,W,X),Lc),!.
navigateB(_,_,_,_,C,_,_,NLl,NLc) :- width(W), height(H), endPoint(C),findall(pair(X,0),between(1,H,X),Ll),findall(pair(X,0),between(1,W,X),Lc),C=cell(I,J,_),incrementList(Ll,I,NLl),incrementList(Lc,J,NLc),!.


navigateB(_,_,_,_,cell(I,_,cert(nw)),0,-1,_,_) :- I=<1, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(nw)),0,-1,Ll, Lc) :- I>1, NextI is I-1, !, getCell(NextI,J,L,C),!, navigateB(T,H,W,L,C,1,0,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).

navigateB(_,_,_,_,cell(_,J,cert(nw)),-1,0,_,_) :- J=<1, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(nw)),-1,0,Ll, Lc) :- J>1, NextJ is J-1, !, getCell(I,NextJ,L,C), !, navigateB(T,H,W,L,C,0,1,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).


navigateB(_,H,_,_,cell(I,_,cert(sw)),0,-1,_,_) :- I>=H, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(sw)),0,-1,Ll, Lc) :- I<H, NextI is I+1, !, getCell(NextI,J,L,C), !, navigateB(T,H,W,L,C,-1,0,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).

navigateB(_,_,_,_,cell(_,J,cert(sw)),1,0,_,_) :- J=<1, !,fail.
navigateB(T,H,W,L,cell(I,J,cert(sw)),1,0,Ll, Lc) :- J>1, NextJ is J-1, !, getCell(I,NextJ,L,C), !, navigateB(T,H,W,L,C,0,1,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).


navigateB(_,_,_,_,cell(I,_,cert(ne)),0,1,_,_) :- I=<1, !,fail.
navigateB(T,H,W,L,cell(I,J,cert(ne)),0,1,Ll, Lc) :- I>1, NextI is I-1, !, getCell(NextI,J,L,C), !, navigateB(T,H,W,L,C,1,0,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).

navigateB(_,_,W,_,cell(_,J,cert(ne)),-1,0,_,_) :- J>=W, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(ne)),-1,0,Ll, Lc) :- J<W, NextJ is J+1, !, getCell(I,NextJ,L,C), !, navigateB(T,H,W,L,C,0,-1,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).


navigateB(_,H,_,_,cell(I,_,cert(se)),0,1,_,_) :- I>=H, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(se)),0,1,Ll, Lc) :- I<H, NextI is I+1, !, getCell(NextI,J,L,C),!, navigateB(T,H,W,L,C,-1,0,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).

navigateB(_,_,W,_,cell(_,J,cert(se)),1,0,_,_) :- J>=W, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(se)),1,0,Ll, Lc) :- J<W, NextJ is J+1, !, getCell(I,NextJ,L,C), !, navigateB(T,H,W,L,C,0,-1,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).


navigateB(_,_,_,_,cell(_,J,cert(horiz)),0,1,_,_) :- J=<1, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(horiz)),0,1,Ll, Lc) :- J>1, NextJ is J-1, !, getCell(I,NextJ,L,C), !, navigateB(T,H,W,L,C,0,1,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).

navigateB(_,_,W,_,cell(_,J,cert(horiz)),0,-1,_,_) :- J>=W, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(horiz)),0,-1,Ll, Lc) :- J<W, NextJ is J+1, !, getCell(I,NextJ,L,C), !, navigateB(T,H,W,L,C,0,-1,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).


navigateB(_,_,_,_,cell(I,_,cert(vert)),1,0,_,_) :- I=<1, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(vert)),1,0,Ll, Lc) :- I>1, NextI is I-1, !, getCell(NextI,J,L,C), !, navigateB(T,H,W,L,C,1,0,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).

navigateB(_,H,_,_,cell(I,_,cert(vert)),-1,0,_,_) :- I>=H, !, fail.
navigateB(T,H,W,L,cell(I,J,cert(vert)),-1,0,Ll, Lc) :- I<H, NextI is I+1, !, getCell(NextI,J,L,C), !, navigateB(T,H,W,L,C,-1,0,OLl,OLc), incrementList(OLl,I,Ll), incrementList(OLc, J, Lc).


applyBRuleLines(_,[],[],0).
applyBRuleLines(_,[LH|_], [HH|_], 0) :- LH=pair(_,X),HH=Y, X>Y,!,fail.
applyBRuleLines(T,[LH|LT], [HH|HT], R) :- LH=pair(_,X),HH=Y, X<Y, !, applyBRuleLines(T,LT,HT,R).
applyBRuleLines(T,[LH|LT], [HH|HT], R) :- LH=pair(I,X),HH=Y, X==Y, !, applyBRuleLine(T,I,R1),!,applyBRuleLines(T,LT,HT,R2), R is sign(R1+R2).

applyBRuleCols(_,[],[],0).
applyBRuleCols(_,[LH|_], [HH|_], 0) :- LH=pair(_,X),HH=Y, X>Y,!,fail.
applyBRuleCols(T,[LH|LT], [HH|HT], R) :- LH=pair(_,X),HH=Y, X<Y, !, applyBRuleCols(T,LT,HT,R).
applyBRuleCols(T,[LH|LT], [HH|HT], R) :- LH=pair(J,X),HH=Y, X==Y, !, applyBRuleCol(T,J,R1),!,applyBRuleCols(T,LT,HT,R2), R is sign(R1+R2).


applyBRuleLine(T,I,R) :- endPoint(C), C=cell(IE,_,_),!, bottleneckLine(T,I,IE,R).

bottleneckLine(_,I,IE,0) :- I==IE,!.
bottleneckLine(T,I,IE,R) :- I<IE, T=tracks(_,_,_,_,C), makeCellsImpossLine(C,1,I,R),!.
bottleneckLine(T,I,IE,R) :- I>IE, T=tracks(_,H,_,_,C), makeCellsImpossLine(C,I,H,R),!.

applyBRuleCol(T,J,R) :- endPoint(C), C=cell(_,JE,_),!, bottleneckCol(T,J,JE,R).


bottleneckCol(_,J,JE,0) :- J==JE,!.
bottleneckCol(T,J,JE,R) :- J<JE, T=tracks(_,_,_,_,C), makeCellsImpossCol(C,1,J,R),!.
bottleneckCol(T,J,JE,R) :- J>JE, T=tracks(W,_,_,_,C), makeCellsImpossCol(C,J,W,R),!.

makeCellsImpossLine([],_,_,0).
makeCellsImpossLine([CH|CT],S,E,R) :- CH=cell(I,_,_), (I<S; I>E; cellImposs(CH); cellKnown(CH)), !, makeCellsImpossLine(CT,S,E,R).
makeCellsImpossLine([CH|CT],S,E,1) :- CH=cell(I,_,X), I>=S, I=<E, not(cellImposs(CH)), not(cellKnown(CH)),!, X=imposs, !, makeCellsImpossLine(CT,S,E,_).

makeCellsImpossCol([],_,_,0).
makeCellsImpossCol([CH|CT],S,E,R) :- CH=cell(_,J,_), (J<S; J>E; cellImposs(CH); cellKnown(CH)),!, makeCellsImpossCol(CT,S,E,R).
makeCellsImpossCol([CH|CT],S,E,1) :- CH=cell(_,J,X), J>=S, J=<E, not(cellImposs(CH)), not(cellKnown(CH)),!, X=imposs, !, makeCellsImpossCol(CT,S,E,_).


%---------------------------------- COMPLETION __________________________________
complete(T) :- T=tracks(W,H,_,_,_), checkSolved(H,W,T).

checkSolved(0, 0, _).
checkSolved(0, J, T):- JNext is J - 1, countKnownCol(T, J, KCC), getColHint(T, J, CH), CH = KCC,!, checkSolved(0, JNext, T).
checkSolved(I, J, T):- I > 0, J > 0, INext is I - 1, countKnownLine(T, I, KLC), getLineHint(T, I, LH), LH = KLC,!, checkSolved(INext, J, T).



%----------------------------------- SOLVING ____________________________________
%solvePuzzle(T, T) :- applyRules(T), validate(T).
solvePuzzle(T, T) :- applyRules(T), validate(T),!, continueSolving(T).

startSolving(T) :- T=tracks(W,H,_,_,L), bagof(cell(I,J,S),(member(cell(I,J,S),L), cellKnown(cell(I,J,S)), isStartEndCell(I,J,W,H,S)),SP), SP=[SP1,SP2], asserta(startPoint(SP1)),asserta(endPoint(SP2)),!,solvePuzzle(T,T).

continueSolving(T) :- complete(T),!,retractall(startPoint(_)),retractall(endPoint(_)).
continueSolving(T) :- not(complete(T)), findCell(T,I,J),!, generateGuess(T,I,J), solvePuzzle(T,T).


findCell(T,I,J) :- startPoint(C),!, T=tracks(W,H,_,_,_), startNavigate(T,W,H,C,I,J).

findEndpoint(T,I,J) :- T=tracks(W,H,_,_,L), member(C,L), cellKnown(C), C=cell(I,J,S), isStartEndCell(I,J,W,H,S). 

startNavigate(T,W,H,cell(1,JS,cert(nw)),I,J) :- NextJ is JS-1, getCellSafe(T,1,NextJ,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,0,1,I,J).
startNavigate(T,W,H,cell(IS,1,cert(nw)),I,J) :- NextI is IS-1, getCellSafe(T,NextI,1,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,1,0,I,J).
startNavigate(T,W,H,cell(1,JS,cert(ne)),I,J) :- NextJ is JS+1, getCellSafe(T,1,NextJ,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,0,-1,I,J).
startNavigate(T,W,H,cell(IS,W,cert(ne)),I,J) :- NextI is IS-1, getCellSafe(T,NextI,W,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,1,0,I,J).
startNavigate(T,W,H,cell(H,JS,cert(se)),I,J) :- NextJ is JS+1, getCellSafe(T,H,NextJ,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,0,-1,I,J).
startNavigate(T,W,H,cell(IS,W,cert(se)),I,J) :- NextI is IS+1, getCellSafe(T,NextI,W,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,-1,0,I,J).
startNavigate(T,W,H,cell(H,JS,cert(sw)),I,J) :- NextJ is JS-1, getCellSafe(T,H,NextJ,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,0,1,I,J).
startNavigate(T,W,H,cell(IS,1,cert(sw)),I,J) :- NextI is IS+1, getCellSafe(T,NextI,1,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,-1,0,I,J).
startNavigate(T,W,H,cell(1,JS,cert(vert)),I,J) :- NextI is 1+1, getCellSafe(T,NextI,JS,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,-1,0,I,J).
startNavigate(T,W,H,cell(H,JS,cert(vert)),I,J) :- NextI is H-1, getCellSafe(T,NextI,JS,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,1,0,I,J).
startNavigate(T,W,H,cell(IS,1,cert(horiz)),I,J) :- NextJ is 1+1, getCellSafe(T,IS,NextJ,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,0,-1,I,J).
startNavigate(T,W,H,cell(IS,W,cert(horiz)),I,J) :- NextJ is W-1, getCellSafe(T,IS,NextJ,C), T=tracks(_,_,_,_,L),!, navigate(H,W,L,C,0,1,I,J).


navigate(_,_,_,C,_,_,I,J) :- C=cell(I,J,_),not(cellKnown(C)),!.


navigate(_,_,_,cell(I,_,cert(nw)),0,-1,_,_) :- I=<1, !, fail.
navigate(H,W,L,cell(I,J,cert(nw)),0,-1,SI,SJ) :- I>1, NextI is I-1, getCell(NextI,J,L,C),!, navigate(H,W,L,C,1,0,SI,SJ).

navigate(_,_,_,cell(_,J,cert(nw)),-1,0,_,_) :- J=<1, !, fail.
navigate(H,W,L,cell(I,J,cert(nw)),-1,0,SI,SJ) :- J>1, NextJ is J-1, getCell(I,NextJ,L,C), !, navigate(H,W,L,C,0,1,SI,SJ).


navigate(H,_,_,cell(I,_,cert(sw)),0,-1,_,_) :- I>=H, !, fail.
navigate(H,W,L,cell(I,J,cert(sw)),0,-1,SI,SJ) :- I<H, NextI is I+1, getCell(NextI,J,L,C), !, navigate(H,W,L,C,-1,0,SI,SJ).

navigate(_,_,_,cell(_,J,cert(sw)),1,0,_,_) :- J=<1, !,fail.
navigate(H,W,L,cell(I,J,cert(sw)),1,0,SI,SJ) :- J>1, NextJ is J-1, getCell(I,NextJ,L,C), !, navigate(H,W,L,C,0,1,SI,SJ).


navigate(_,_,_,cell(I,_,cert(ne)),0,1,_,_) :- I=<1, !,fail.
navigate(H,W,L,cell(I,J,cert(ne)),0,1,SI,SJ) :- I>1, NextI is I-1, getCell(NextI,J,L,C), !, navigate(H,W,L,C,1,0,SI,SJ).

navigate(_,W,_,cell(_,J,cert(ne)),-1,0,_,_) :- J>=W, !, fail.
navigate(H,W,L,cell(I,J,cert(ne)),-1,0,SI,SJ) :- J<W, NextJ is J+1, getCell(I,NextJ,L,C), !, navigate(H,W,L,C,0,-1,SI,SJ).


navigate(H,_,_,cell(I,_,cert(se)),0,1,_,_) :- I>=H, !, fail.
navigate(H,W,L,cell(I,J,cert(se)),0,1,SI,SJ) :- I<H, NextI is I+1, getCell(NextI,J,L,C),!, navigate(H,W,L,C,-1,0,SI,SJ).

navigate(_,W,_,cell(_,J,cert(se)),1,0,_,_) :- J>=W, !, fail.
navigate(H,W,L,cell(I,J,cert(se)),1,0,SI,SJ) :- J<W, NextJ is J+1, getCell(I,NextJ,L,C), !, navigate(H,W,L,C,0,-1,SI,SJ).


navigate(_,_,_,cell(_,J,cert(horiz)),0,1,_,_) :- J=<1, !, fail.
navigate(H,W,L,cell(I,J,cert(horiz)),0,1,SI,SJ) :- J>1, NextJ is J-1, getCell(I,NextJ,L,C), !, navigate(H,W,L,C,0,1,SI,SJ).

navigate(_,W,_,cell(_,J,cert(horiz)),0,-1,_,_) :- J>=W, !, fail.
navigate(H,W,L,cell(I,J,cert(horiz)),0,-1,SI,SJ) :- J<W, NextJ is J+1, getCell(I,NextJ,L,C), !, navigate(H,W,L,C,0,-1,SI,SJ).


navigate(_,_,_,cell(I,_,cert(vert)),1,0,_,_) :- I=<1, !, fail.
navigate(H,W,L,cell(I,J,cert(vert)),1,0,SI,SJ) :- I>1, NextI is I-1, getCell(NextI,J,L,C), !, navigate(H,W,L,C,1,0,SI,SJ).

navigate(H,_,_,cell(I,_,cert(vert)),-1,0,_,_) :- I>=H, !, fail.
navigate(H,W,L,cell(I,J,cert(vert)),-1,0,SI,SJ) :- I<H, NextI is I+1, getCell(NextI,J,L,C), !, navigate(H,W,L,C,-1,0,SI,SJ).




generateGuess(T,I,J) :- setCell(I, J, T, cert(vert)).
generateGuess(T,I,J) :- setCell(I, J, T, cert(se)).
generateGuess(T,I,J) :- setCell(I, J, T, cert(ne)).
generateGuess(T,I,J) :- setCell(I, J, T, cert(sw)).
generateGuess(T,I,J) :- setCell(I, J, T, cert(nw)).
generateGuess(T,I,J) :- setCell(I, J, T, cert(horiz)).

%----------------------------------- COUNTING ___________________________________
countKnownLine(T, I, X) :- T=tracks(W,H,_,_,C), getLine(W,H,I,1,C, L), countKnown(L,X).
countKnownCol(T, J, X) :- T=tracks(W,H,_,_,C), getCol(W,H,1,J,C, L), countKnown(L,X).

countKnown([],0).
countKnown(L,X):- L=[H|T],!, countKnown(T,R), countKnownCell(H,Count), X is Count+R.

countImpossLine(T, I, X) :- T=tracks(W,H,_,_,C), getLine(W,H,I,1,C, L), countImposs(L,X).
countImpossCol(T, J, X) :- T=tracks(W,H,_,_,C), getCol(W,H,1,J,C, L), countImposs(L,X).

countImposs([],0).
countImposs(L,X):- L=[H|T],!, countImposs(T,R), countImpossibleCell(H,Count), X is Count+R.


countCertLine(T, I, X) :- T=tracks(W,H,_,_,C), getLine(W,H,I,1,C, L), countCert(L,X).
countCertCol(T, J, X) :- T=tracks(W,H,_,_,C), getCol(W,H,1,J,C, L), countCert(L,X).

countCert([],0).
countCert(L,X):- L=[H|T], !,countCert(T,R), countCertainCell(H,Count), X is Count+R.


countCertainCell(cell(_,_,X), 1) :- not(X=imposs).
countCertainCell(cell(_,_,X), 0) :- not(not(X=imposs)).

countKnownCell(X,1) :- cellKnown(X).
countKnownCell(X,0) :- not(cellKnown(X)).

%countImpossibleCell(cell(_,_,X), 1) :- X=imposs.
%countImpossibleCell(cell(_,_,X), 0) :- X\==imposs.

countImpossibleCell(cell(_,_,X), 1) :- not(X=cert(_)).
countImpossibleCell(cell(_,_,X), 0) :- not(not(X=cert(_))).


countPossibleConnectionN(X,0) :- cellImposs(X).
countPossibleConnectionN(X,1) :- not(cellImposs(X)), not(cellKnown(X)).
countPossibleConnectionN(X,0) :- not(cellImposs(X)), cellKnown(X), not(isPointingToCellFromN(X)).
countPossibleConnectionN(X,1) :- not(cellImposs(X)), cellKnown(X), isPointingToCellFromN(X).

countPossibleConnectionS(X,0) :- cellImposs(X).
countPossibleConnectionS(X,1) :- not(cellImposs(X)), not(cellKnown(X)).
countPossibleConnectionS(X,0) :- not(cellImposs(X)), cellKnown(X), not(isPointingToCellFromS(X)).
countPossibleConnectionS(X,1) :- not(cellImposs(X)), cellKnown(X), isPointingToCellFromS(X).

countPossibleConnectionW(X,0) :- cellImposs(X).
countPossibleConnectionW(X,1) :- not(cellImposs(X)), not(cellKnown(X)).
countPossibleConnectionW(X,0) :- not(cellImposs(X)), cellKnown(X), not(isPointingToCellFromW(X)).
countPossibleConnectionW(X,1) :- not(cellImposs(X)), cellKnown(X), isPointingToCellFromW(X).

countPossibleConnectionE(X,0) :- cellImposs(X).
countPossibleConnectionE(X,1) :- not(cellImposs(X)), not(cellKnown(X)).
countPossibleConnectionE(X,0) :- not(cellImposs(X)), cellKnown(X), not(isPointingToCellFromE(X)).
countPossibleConnectionE(X,1) :- not(cellImposs(X)), cellKnown(X), isPointingToCellFromE(X).


countCertainConnectionN(X,0) :- not(cellKnown(X)).
countCertainConnectionN(X,0) :- cellKnown(X), not(isPointingToCellFromN(X)).
countCertainConnectionN(X,1) :- cellKnown(X), isPointingToCellFromN(X).

countCertainConnectionW(X,0) :- not(cellKnown(X)).
countCertainConnectionW(X,0) :- cellKnown(X), not(isPointingToCellFromW(X)).
countCertainConnectionW(X,1) :- cellKnown(X), isPointingToCellFromW(X).

countCertainConnectionS(X,0) :- not(cellKnown(X)).
countCertainConnectionS(X,0) :- cellKnown(X), not(isPointingToCellFromS(X)).
countCertainConnectionS(X,1) :- cellKnown(X), isPointingToCellFromS(X).

countCertainConnectionE(X,0) :- not(cellKnown(X)).
countCertainConnectionE(X,0) :- cellKnown(X), not(isPointingToCellFromE(X)).
countCertainConnectionE(X,1) :- cellKnown(X), isPointingToCellFromE(X).


%----------------------------------- OBSERVERS __________________________________

cellKnown(cell(_,_,X)) :- (not(X=cert(vert));not(X=cert(horiz))),!,not(X=imposs).

cellImposs(cell(_,_,X)) :- not(X=cert(_)).

cellCert(cell(_,_,X)) :- not(X=imposs).


isPointingToCellFromN(cell(_,_,X)):- not(not((X = cert(vert); X = cert(se); X = cert(sw)))),!.
isPointingToCellFromE(cell(_,_,X)):- not(not((X = cert(horiz); X = cert(nw); X = cert(sw)))),!.
isPointingToCellFromS(cell(_,_,X)):- not(not((X = cert(ne); X = cert(nw); X = cert(vert)))),!.
isPointingToCellFromW(cell(_,_,X)):- not(not((X = cert(ne); X = cert(se); X = cert(horiz)))),!.

isStartEndCell(1, _, _, _, cert(vert)). % top edge
isStartEndCell(1, _, _, _, cert(ne)).
isStartEndCell(1, _, _, _, cert(nw)).

isStartEndCell(_, 1, _, _, cert(horiz)). % left edge
isStartEndCell(_, 1, _, _, cert(sw)).
isStartEndCell(_, 1, _, _, cert(nw)).

isStartEndCell(_, W, W, _, cert(horiz)). % right edge
isStartEndCell(_, W, W, _, cert(ne)).
isStartEndCell(_, W, W, _, cert(se)).

isStartEndCell(H, _, _, H, cert(vert)). % bottom edge
isStartEndCell(H, _, _, H, cert(se)).
isStartEndCell(H, _, _, H, cert(sw)).



%----------------------------------- SETTERS ____________________________________
setCell(I, J, T, State) :- T=tracks(_,_,_,_,C), member(cell(I,J,X),C), X=State.

setLineCert(T,I,R) :- T=tracks(W,H,_,_,C), getLine(W,H,I,1,C,L),!, setCertIfPos(L,R),!.
setColCert(T,J,R) :- T=tracks(W,H,_,_,C), getCol(W,H,1,J,C,L),!, setCertIfPos(L,R),!.

setCertIfPos([],0).
setCertIfPos([H|L],R) :- setCellCert(H,R1), !, setCertIfPos(L,R2), R is sign(R1+R2).

setCellCert(X,0) :- cellImposs(X).
setCellCert(X,0) :- not(cellImposs(X)), cellCert(X).
setCellCert(X,1) :- not(cellImposs(X)), not(cellCert(X)), X=cell(_,_,S), S=cert(_).

setLineImposs(T,I,R) :- T=tracks(W,H,_,_,C), getLine(W,H,I,1,C,L), setImpossIfPos(L,R),!.
setColImposs(T,J,R) :- T=tracks(W,H,_,_,C), getCol(W,H,1,J,C,L), setImpossIfPos(L,R),!.

setImpossIfPos([],0).
setImpossIfPos([H|L],R) :- setCellImposs(H,R1), !, setImpossIfPos(L,R2), R is sign(R1+R2).

setCellImposs(X,0) :- cellCert(X).
setCellImposs(X,0) :- not(cellCert(X)), cellImposs(X).
setCellImposs(X,1) :- not(cellCert(X)), not(cellImposs(X)), X=cell(_,_,S), S=imposs.



setCellsInCol(I, _, X, _) :- X=tracks(_,H,_,_,_), I > H.
setCellsInCol(I, J, T, State) :- I > 0, J > 0, T=tracks(_,_,_,_,C), getCell(I, J, C, CS), ((CS \== imposs, setCell(I, J, T, cell(I, J, State))); not(not(CS=imposs))), INext is I + 1, !,setCellsInCol(INext, J, T, State).

setCellsInLine(_, J, X, _) :- X=tracks(W,_,_,_,_), J > W.
setCellsInLine(I, J, T, State) :- I > 0, J > 0, T=tracks(_,_,_,_,C), getCell(I, J, C, CS), ((CS \== imposs, setCell(I, J, T, cell(I, J, State))); not(not(CS=imposs))), JNext is J + 1, !, setCellsInLine(I, JNext, T, State).


%----------------------------------- GETTERS ____________________________________
%getLine(0, _, _, _, _, []).
%getLine(W, H, I, J, C, L) :- W>0, getCell(I,J,C,Cell), !, NextW is W-1, NextJ is J+1, getLine(NextW, H, I, NextJ, C, NextL), append([Cell], NextL, L),!.

getLine(_, _, _, _, [], []):-!.
getLine(W, H, I, J, [CH|CT], L) :- W>0, not(CH=cell(I,_,_)), !, getLine(W,H,I,J,CT,L), !.
getLine(W, H, I, J, [CH|CT], L) :- W>0, CH=cell(I,_,_), !, getLine(W,H,I,J,CT,LT), append([CH], LT, L),!.

%getLine(_,_,I,_,C,L) :- findall(cell(I,J,X),member(cell(I,J,X),C),L),!.

getLineHint(T, I, X) :- T=tracks(_,H,_,Hl,_), I>0, I=<H, Idx is I-1, nth0(Idx, Hl, X).
getColHint(T, J, X) :- T=tracks(W,_,Hc,_,_), J>0, J=<W, Idx is J-1, nth0(Idx, Hc, X).

%getCol(_, 0, _, _, _, []).
%getCol(W, H, I, J, C, L) :- H>0, getCell(I,J,C,Cell), !, NextH is H-1, NextI is I+1, getCol(W, NextH, NextI, J, C, NextL), append([Cell], NextL, L),!.

getCol(_, _, _, _, [], []):-!.
getCol(W, H, I, J, [CH|CT], L) :- W>0, not(CH=cell(_,J,_)), !, getCol(W,H,I,J,CT,L), !.
getCol(W, H, I, J, [CH|CT], L) :- W>0, CH=cell(_,J,_), !, getCol(W,H,I,J,CT,LT), append([CH], LT, L),!.


%getCell(I, J, L, C) :- member(cell(I,J,X),L), C=cell(I,J,X),!.
getCell(I, J, L, C) :- width(W), Idx is (I-1)*W+J-1, nth0(Idx, L, C),!.

getCellSafe(T,I,J,C) :- T=tracks(W,H,_,_,_), (I<1;J<1;I>H;J>W), C=cell(I,J,imposs).
getCellSafe(T,I,J,C) :- T=tracks(W,H,_,_,L), I>=1,J>=1,I=<H,J=<W,!, getCell(I,J,L,C).

getNeighbours(I, J, C, N, E, S, W) :- ITop is I - 1, IBottom is I + 1, JLeft is J - 1, JRight is J + 1, getCell(ITop, J, C, N),!, getCell(I, JRight, C, E),!, getCell(IBottom, J, C, S),!, getCell(I, JLeft, C, W),!.

%--------------------------------------------------------------------------------

%----------------------------------- READING ____________________________________

ignorePuzzles :- get_char(_), get_char(_), get_char(_), get_char(_), get_char(_), get_char(_), get_char(_), get_char(_).
ignoreSize :- get_char(_), get_char(_), get_char(_), get_char(_), get_char(_).


readInt(N):- get_code(M), handleCode(M,N).

handleCode(M,N):- is_number_code(M,N1), !, continueInt(N1,N).
handleCode(-1,_):- !, fail. /* EOF */
handleCode(_,N):- readInt(N).

continueInt(O,N):- get_code(M), is_number_code(M,M1), !, H is 10*O+M1, continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):- N>=48, N<58, N1 is N-48.

ignoreSpace :- peek_char(X), X\==' ',X\=='\n',X\=='\r',!.
ignoreSpace :- peek_char(X), (X==' ';X=='\n';X=='\r'), get_char(_), ignoreSpace.


%Tracks(5,5,[2,3,3,5,4],[2,4,5,4,2],[Cell(3,1,NW),Cell(5,3,SE)]).
readPuzzle(T) :- ignoreSize, readInt(W), readInt(H), readCHints(W, Hc), ignoreSpace, readRest(W, H, 1, 1, Hl, C), T=tracks(W,H,Hc,Hl,C).

readCHints(0, []).
readCHints(W, Hc) :- W>0, readInt(Hint), !, Next is W-1, readCHints(Next, HcNext), append([Hint],HcNext,Hc).

readRest(_, 0, _, _, [], []).
readRest(W, H, I, _, Hl, C) :- H>0, readLine(W, H, I, 1, L), readInt(Hint), !, Next is H-1, NextI is I+1, readRest(W, Next, NextI, 1, HlNext, CNext), append([Hint],HlNext, Hl), append(L,CNext,C).

readLine(0, _, _, _, []).
readLine(W, H, I, J, L) :- W>0, readCell(C), get_char(_), !, Next is W-1, NextJ is J+1, readLine(Next, H, I, NextJ, LNext), append([cell(I,J,C)], LNext, L).

readCell(C) :- get_code(X), decode(X, C).


decode(95, _).
decode(X, C) :- decode_track(X,C).

decode_track(61, cert(horiz)).
decode_track(9552, cert(horiz)).
decode_track(9565, cert(nw)).
decode_track(9562, cert(ne)).
decode_track(9559, cert(sw)).
decode_track(9553, cert(vert)).
decode_track(9556, cert(se)).
%--------------------------------------------------------------------------------

%----------------------------------- WRITING ____________________________________

writeTracks(T) :- T=tracks(W,H,Hc,Hl,C), write('size '), write(W), write('x'), write(H), nl, writeCHints(Hc), nl, writeLines(W, H, 1, 1, Hl, C).

writeCHints([]).
writeCHints(Hc) :- Hc=[H|T], write(H), write(' '), !, writeCHints(T).

writeLines(_, 0, _, _, _, _).
writeLines(W, H, I, _, Hl, C) :- H>0, getLine(W, H, I, 1, C, L), writeLine(L), Hl=[HlH| HlT], write(HlH), nl, !, NextH is H-1, NextI is I+1, writeLines(W, NextH, NextI, 1, HlT, C).

writeLine([]).

writeLine(L) :- L=[H|T], convertToString(H, S), write(S), write(' '), !, writeLine(T),!.

convertToString(X, ' ') :- not(cellImposs(X)), not(cellCert(X)),!.
convertToString(X, ' ') :- cellImposs(X),!.
convertToString(X, 'C') :- cellCert(X), not(cellKnown(X)),!.
convertToString(cell(_,_,cert(horiz)), '═').
convertToString(cell(_,_,cert(vert)), '║').
convertToString(cell(_,_,cert(nw)), '╝').
convertToString(cell(_,_,cert(ne)), '╚').
convertToString(cell(_,_,cert(sw)), '╗').
convertToString(cell(_,_,cert(se)), '╔').
%----------------------------------------------------------------------------------

input_output(IF,OF):- current_prolog_flag(argv,['--io',IF,OF]),!.
input_output(IF,OF):- inputFile(IF), outputFile(OF).

run :- input_output(IF,OF), see(IF), tell(OF), ignorePuzzles, readInt(N), write('puzzles '), write(N), nl, processPuzzles(N), seen, told, !.

processPuzzles(0).
processPuzzles(N) :- N>0, readPuzzle(T), !, T=tracks(W,H,_,_,_), asserta(height(H)), asserta(width(W)), !, startSolving(T),!, writeTracks(T), !, retract(height(H)), retract(width(W)), !, Next is N-1, processPuzzles(Next).

%:- leash(-all).

%:-trace.

:- run.

:- halt.
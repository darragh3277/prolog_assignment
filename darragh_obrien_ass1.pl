flight(london,dublin,aerlingus,500,45,150).
flight(rome,london,ba,1500,150,400).
flight(rome,paris,airfrance,1200,120,500). 
flight(paris,dublin,airfrance,600,60,200).
flight(berlin,moscow,lufthansa,3000,300,900).
flight(paris,amsterdam,airfrance,400,30,100).
flight(berlin,dublin,lufthansa,1200,120,900).
flight(london,newyork,ba,5000,700,1100).
flight(dublin,newyork,aerlingus,4500,360,800).
flight(dublin,cork,ryanair,300,50,50).
flight(dublin,rome,ryanair,2000,150,70).
flight(dublin,chicago,aerlingus,5500,480,890).
flight(amsterdam,hongkong,klm,7000,660,750).
flight(london,hongkong,ba,7500,700,1000).
flight(dublin,amsterdam,ryanair,1000,90,60).
flight(moscow,newyork,aerflot,9000,720,1000).
flight(moscow,hongkong,aerflot,5500,420,500).
flight(newyork,chicago,aa,3000,240,430).
flight(dublin,london,aerlingus,500,45,150).
flight(london,rome,ba,1500,150,400).
flight(paris,rome,airfrance,1200,120,500). 
flight(dublin,paris,airfrance,600,60,200).
flight(moscow,berlin,lufthansa,3000,300,900).
flight(amsterdam,paris,airfrance,400,30,100).
flight(dublin,berlin,lufthansa,1200,120,900).
flight(newyork,london,ba,5000,700,1100).
flight(newyork,dublin,aerlingus,4500,360,800).
flight(cork,dublin,ryanair,300,50,50).
flight(rome,dublin,ryanair,2000,150,70).
flight(chicago,dublin,aerlingus,5500,480,890).
flight(hongkong,amsterdam,klm,7000,660,750).
flight(hongkong,london,ba,7500,700,1000).
flight(amsterdam,dublin,ryanair,1000,90,60).
flight(newyork,moscow,aerflot,9000,720,1000).
flight(hongkong,moscow,aerflot,5500,420,500).
flight(chicago,newyork,aa,3000,240,430).

country(dublin,ireland).
country(cork,ireland).
country(london,uk).
country(rome,italy).
country(moscow,russia).
country(hongkong,china).
country(amsterdam,holland).
country(berlin,germany).
country(paris,france).
country(newyork,usa).
country(chicago,usa).

%
% HELPER PREDICATES
%

%length of list
list_len([],0).
list_len([_|T],L):-list_len(T,L1), L is L1 + 1.

%get index of element
el_index([H|_],H,0).
el_index([_|T],X,R):-el_index(T,X,R1), R is R1 + 1.

%get trips avoiding input airline
trip_noairline(X,Y,[X|P],A) :-  fly_noairline(X,Y,[],P,A).
fly_noairline(X,Y,V,[Y],A) :- flight(X,Y,A1,_,_,_), A \= A1, not(member(Y,V)).
fly_noairline(X,Y,V,[H|P],A) :- flight(X,H,A1,_,_,_), A \= A1, not(member(H,V)), fly_noairline(H,Y,[X|V],P,A).

%get element at index
el_at([H|_],0,H).
el_at([_|T],I,R):- I1 is I - 1, el_at(T,I1,R).

%remove last item from list
remove_last([_], []).
remove_last([H|T],[H|T1]) :- remove_last(T, T1).

%remove index
remove_index([_|T],0,T).
remove_index([H|T],E,R) :- E>0, E1 is E-1,remove_index(T,E1,R1), append([H],R1,R).

%get the min of a 2d array
mymin([],CR,CC,CR,CC).
mymin([[_|[Y|_]]|T],CR,CC,NR,NC):- Y >= CC, mymin(T,CR,CC,NR,NC).
mymin([[X|[Y|_]]|T],_,CC,NR,NC):- CC > Y, mymin(T,X,Y,NR,NC).
mymin([[X|[Y|_]]|T],R,C):- mymin(T,X,Y,R,C).

%get trips and times
trip_time(X,Y,[[X|P]|[C]]) :-  fly_time(X,Y,[],P,C).
fly_time(X,Y,V,[Y],C) :- flight(X,Y,_,_,C,_), not(member(Y,V)).
fly_time(X,Y,V,[H|P],C) :- flight(X,H,_,_,C1,_), not(member(H,V)), fly_time(H,Y,[X|V],P,C2), C is C1+C2.

%
% PART 1
%

%question 1.1
list_airport(X,L):-findall(A,country(A,X),L).

%question 1.2
trip(X,Y,[X|P]) :-  fly(X,Y,[],P).
fly(X,Y,V,[Y]) :- flight(X,Y,_,_,_,_), not(member(Y,V)).
fly(X,Y,V,[H|P]) :- flight(X,H,_,_,_,_), not(member(H,V)), fly(H,Y,[X|V],P).

%question 1.3
all_trip(X,Y,T):-findall(R,trip(X,Y,R),T).

%question 1.4
trip_dist(X,Y,[[X|P]|[D]]) :-  fly_dist(X,Y,[],P,D).
fly_dist(X,Y,V,[Y],D) :- flight(X,Y,_,D,_,_), not(member(Y,V)).
fly_dist(X,Y,V,[H|P],D) :- flight(X,H,_,D1,_,_), not(member(H,V)), fly_dist(H,Y,[X|V],P,D2), D is D1+D2.

%question 1.5
trip_cost(X,Y,[[X|P]|[C]]) :-  fly_cost(X,Y,[],P,C).
fly_cost(X,Y,V,[Y],C) :- flight(X,Y,_,_,_,C), not(member(Y,V)).
fly_cost(X,Y,V,[H|P],C) :- flight(X,H,_,_,_,C1), not(member(H,V)), fly_cost(H,Y,[X|V],P,C2), C is C1+C2.

%question 1.6
trip_change(X,Y,[[X|P]|[C]]) :-  fly_change(X,Y,[],P,C).
fly_change(X,Y,V,[Y],0) :- flight(X,Y,_,_,_,_), not(member(Y,V)).
fly_change(X,Y,V,[H|P],C) :- flight(X,H,_,_,_,_), not(member(H,V)), fly_change(H,Y,[X|V],P,_), list_len([H|P],C).

%question 1.7
all_trip_noairline(X,Y,T,A):-findall(R,trip_noairline(X,Y,R,A),T).

%question 1.8
cheapest(X,Y,T,C):-findall(R,trip_cost(X,Y,R),L), mymin(L,T,C).
shortest(X,Y,T,C):-findall(R,trip_dist(X,Y,R),L), mymin(L,T,C).
fastest(X,Y,T,C):-findall(R,trip_time(X,Y,R),L), mymin(L,T,C).

%question 1.9
%ok if flies to dublin -> cork?
trip_to_nation(X,Y,T):-country(Z,Y),trip(X,Z,T).

%question 1.10
%ok if flies to dublin -> cork?
all_trip_to_nation(X,Y,T):-findall(R,trip_to_nation(X,Y,R),T).

%
% PART 2
%

%[[b,c,f],[a,d,g],[h,e]]
%question 2.1
print_status([]).
print_status([H|T]) :- is_list(H),print_status(H),write('|'),writeln(''),print_status(T).
print_status([H|T]) :- not(is_list(H)),write('|'),write(H),print_status(T).

%question 2.2
count_blocks([],[]).
count_blocks([H|T],C):- list_len(H,L1), count_blocks(T,L2), append([L1],L2,C).

%question 2.3
high([H|_],X,R):-el_index(H,X,R).
high([_|T],X,R):-high(T,X,R).

%question 2.4
all_same_height(B,H,L):-findall(A,high(B,A,H),L).

%question 2.5
same_height(B,X,Y):-high(B,X,R1), high(B,Y,R2), R1 == R2.

%question 2.6
ontop(B,X,S):-S1 is S-1, el_at(B,S1,BS), reverse(BS,[H|_]), H=X.		
moveblock(B,X,S1,S2):-	S2 >= 1,
						S2 =< 3,
						ontop(B,X,S1),
						print_status(B),
						findall(R,popBlock(B,X,S1,R),RB),
						findall(R1,pushBlock(RB,X,S2,R1),AB),
						writeln(''),
						print_status(AB).

popBlock([H|_],_,I,H):- I \= 1.
popBlock([H|_],_,1,R):- remove_last(H,R).
popBlock([_|T],X,S,R):- S1 is S-1, popBlock(T,X,S1,R).

pushBlock([H|_],_,I,H):- I \= 1.					
pushBlock([H|_],X,1,R):-append(H,[X],R).
pushBlock([_|T],X,S,R):- S1 is S-1, pushBlock(T,X,S1,R).

%
% PART 3
%

/*
Find A
Is it on bottom?
	Yes
		Is next lowest on top of A?
			Yes - Repeat
			No - Clear above A
	No 
		Find location of next lowest
		Clear stack to non a col.
		Move A to here.
		Find location of next lowest
		Clear above.
		Move onto A
		
	[[b,c,f],[a,d,g],[h,e]]
*/

moveBlock(B,X,S1,S2,AB):-	S2 >= 1,
							S2 =< 3,
							ontop(B,X,S1),
							findall(R,popBlock(B,X,S1,R),RB),
							findall(R1,pushBlock(RB,X,S2,R1),AB),
							writeln(''),
							print_status(AB).
							
%move block by indexes
moveBlockIndex(B,C,I1,I2,RB):-S1 is I1+1, S2 is I2+1, moveBlock(B,C,S1,S2,RB).

%get list of all blocks
allBlocks([],[]).
allBlocks([H|T],R):- allBlocks(T,R1), append(H,R1,R).

%get all blocks sorted	
allBlocksSorted(B,R):-allBlocks(B,X), sort(X,R).

%which stack is the block in
stack([H|_],X,0):- member(X,H).
stack([H|T],X,R):- not(member(X,H)), stack(T,X,R1), R is R1+1.

%return next block
next([H|_],H).

%block location B = blocks, F = block to find, X = stack, y = height, 
blockPos(B,F,X,Y):-stack(B,F,X),high(B,F,Y).

%get X,Y of next block
nextBlockPos(B,[H|_],X,Y):- blockPos(B,H,X,Y).
nextBlockPos(B,[H|T],T,X,Y):- blockPos(B,H,X,Y).

%get which block is on top
topBlock(S,H):-reverse(S,[H|_]).

%move all blocks in S1 to S2
clearStack(B,I1,_,B,0):- el_at(B,I1,[]). 
clearStack(B,I1,I2,AB,M):- el_at(B,I1,S1), topBlock(S1,MB), P1 is I1+1, P2 is I2+1, moveBlock(B,MB,P1,P2,NO), clearStack(NO,I1,I2,AB,MS), M is 1+MS.

%move all blocks above X to S
clearAbove(B,X,_,B,0):- blockPos(B,X,S,_), P1 is S+1, ontop(B,X,P1). 
clearAbove(B,X,I,AB,M):- P2 is I+1, blockPos(B,X,S,_), P1 is S+1, not(ontop(B,X,P1)), el_at(B,S,XB), topBlock(XB,MB), moveBlock(B,MB,P1,P2,NO), clearAbove(NO,X,I,AB,MS), M is 1+MS.

%index of lists excluding input
indexOtherLists(E,R):- delete([0,1,2],E,R).

%get remaining stack
dump([H|[T|_]],R):- delete([0,1,2],H,R1),delete(R1,T,[R|_]).

%remove all instances of a list from another list
removeFromList(X,[],X).
removeFromList(X,[H|T],R):-delete(X,H,X1),removeFromList(X1,T,R).

%smallest list excluding
smallestListExcluding(B,E,T):-indexOtherLists(E,[H|[T|_]]), count_blocks(B,CB), el_at(CB,H,S1), el_at(CB,T,S2), S1>=S2.
smallestListExcluding(B,E,H):-indexOtherLists(E,[H|[T|_]]), count_blocks(B,CB), el_at(CB,H,S1), el_at(CB,T,S2), S2>S1.

%remaining blocks to sort
removeSortedBlocks([],B,B).
removeSortedBlocks([SH|_],[AH|AT],[AH|AT]):- SH \= AH.
removeSortedBlocks([SH|ST],[AH|AT],R):- SH = AH, removeSortedBlocks(ST,AT,R).
remainingBlocks(B,I,RB):- allBlocksSorted(B,ABS), el_at(B,I,S), removeSortedBlocks(S,ABS,RB).

%last sorted block in list
lastSortedBlock(B,RB,I,R):- el_at(B,I,S), removeFromList(S,RB,SB), reverse(SB,[R|_]).

%if first block is on the bottom and the next unsorted is in a different
%stack return the remaining stack index as the dump
placeFirstBlock(B,B,D,BX,0):-		allBlocksSorted(B,SB),
									nextBlockPos(B,SB,BX,BY),
									remainingBlocks(B,BX,[NB|_]),
									blockPos(B,NB,NBX,_),
									NBX \= BX,
									dump([BX,NBX],D),
									BY=0.
%if first block is on the bottom and the next unsorted is in the same stack
%find the index of the tallest remaining stack to use as the dump
placeFirstBlock(B,B,D,BX,0):-		allBlocksSorted(B,SB),
									nextBlockPos(B,SB,BX,BY),
									remainingBlocks(B,BX,[NB|_]),
									blockPos(B,NB,NBX,_),
									NBX = BX,
									smallestListExcluding(B,BX,SL),
									dump([BX,SL],D),
									BY=0.
placeFirstBlock(B,B3,D,SL,M):-		allBlocksSorted(B,SB),				%SB = all blocks sorted
									nextBlockPos(B,SB,BX,BY), 			%RB = remaining blocks after 1 popped off
									BY\=0,
									next(SB,PB),
									smallestListExcluding(B,BX,SL),
									dump([BX,SL],D),					%get the stack we are going to clear to
									clearStack(B,SL,D,B1,M1),
									clearAbove(B1,PB,D,B2,M2),
									moveBlockIndex(B2,PB,BX,SL,B3),		%move first block to empty stack
									M is M1 + M2 + 1.

sortBlocks(B,[],_,_,B,1).
sortBlocks(B,RB,M,FS,B3,TM):-	next(RB,NB),						%get next unsorted block
								nextBlockPos(B,RB,RB1,NBX,_),		%get next unsorted block position
								dump([FS,NBX],D),					%decide where to move to
								clearAbove(B,NB,D,B1,M1),			%clear all blocks above next unsorted block to dump
								moveBlockIndex(B1,NB,NBX,FS,B2),	%move unsorted block to correct location
								M2 is M1 + 1,
								sortBlocks(B2,RB1,M2,FS,B3,M3),		%repeat
								TM is M + M3.

%My process for ordering the blocks was
%Print the initial state of the blocks
%Find the first block
	%If the block is not on the ground find the smaller of the remaining two stacks
	%Clear the smallest of the stacks to the largest of the two stacks.
	%Clear above the first block the large stack
	%place A on the ground.
	
	%If the first block is on the ground already, find the next unorderd block.
		%If next unorderd block is in the same stack, clear the smallest stack to the largest.
		%If next unorderd block is in a different stack. Clear that stack to the remaining stack.
	%Then find the next unorderd block in your stack and clear it and all blocks above to the dump stack.
	
%Finally use the recursive predicate to alternate pop blocks off the next unorderd block until it is the top block
%then move that block onto to sorted stack.
%Repeat until stack is all in order.
order_blocks(B,NO2,M):- 		print_status(B), 						%print starting order
								placeFirstBlock(B,NO,D,FS,M1),			%get first block to ground position
								remainingBlocks(NO,FS,RB1),			%find out what blocks are left to arrange
								lastSortedBlock(NO,RB1,FS,LB),
								clearAbove(NO,LB,D,NO1,M2),
								M3 is M1 + M2,
								sortBlocks(NO1,RB1,M3,FS,NO2,M). 		%sort remaining blocks								
/*


%get list of remaining blocks to sort
nextUnsortedBlock(_,[],_,_,_). %all blocks stacked correctly
nextUnsortedBlock(B,S,X,Y,S):- nextBlockPos(B,S,_,FX,FY), X = FX, N is Y+1, N\=FY. %same stack wrong height
nextUnsortedBlock(B,S,X,_,S):- nextBlockPos(B,S,_,FX,_), X \= FX. %wrong stack
nextUnsortedBlock(B,S,X,Y,RB1):- nextBlockPos(B,S,RB,FX,FY), X = FX, N is Y+1, N=FY, nextUnsortedBlock(B,RB,FX,FY,RB1).

nextBlockInDiffStack(B,[H|_],E,H):-blockPos(B,H,X,_), X\=E.
nextBlockInDiffStack(B,[H|T],E,R):-blockPos(B,H,X,_), X=E, nextBlockInDiffStack(B,T,E,R).

order_blocks(B,UB,S):- 	S is 0,
						allBlocksSorted(B,SB), 				%get a list of all blocks in sorted order
						nextBlockPos(B,SB,RB,BX,BY), 		%return remaining blocks and X,Y of first block
						BY=0, 								%if first block is on the bottom
						nextUnsortedBlock(B,RB,BX,BY,UB),	%get the next unsorted block
						UB = [],							%if no more blocks finish.
						print_status(B), 
						writeln("Finished").
						
order_blocks(B,CB,M):- 	allBlocksSorted(B,SB), 				%get a list of all blocks in sorted order
						nextBlockPos(B,SB,RB,BX,BY), 		%return remaining blocks and X,Y of first block
						BY=0, 								%if first block is on the bottom
						nextUnsortedBlock(B,RB,BX,BY,UB),	%get the next unsorted block
						UB \= [],							%if no more blocks finish.
						nextBlockPos(B,UB,RUB,UBX,UBY),		%get postion of next block to move
						UBX = BX,							%if next block in same stack
						smallestListExcluding(B,BX,SL),		%get smallest list index SL excluding stack first block is in
						dump([BX,SL],D),					%get the stack we are going to clear to
						clearStack(B,SL,D,CB,M).
						
order_blocks(B,CB,M):- 	allBlocksSorted(B,SB), 				%get a list of all blocks in sorted order
						nextBlockPos(B,SB,RB,BX,BY), 		%return remaining blocks and X,Y of first block
						BY=0, 								%if first block is on the bottom
						nextUnsortedBlock(B,RB,BX,BY,UB),	%get the next unsorted block
						UB \= [],							%if no more blocks finish.
						nextBlockPos(B,UB,RUB,UBX,UBY),		%get postion of next block to move
						UBX \= BX,							%if next block in same stack
						dump([BX,UBX],D),					%get the stack we are going to clear to
						next(RB,NB),
						clearAbove(B,NB,D,CB1,M).

%remove from list if value less than input E
removeLowerEqual([],_,[]).
removeLowerEqual([H|T],E,R):- H =< E, removeLowerEqual(T,E,R).
removeLowerEqual([H|T],E,R):- H > E, removeLowerEqual(T,E,L), append([H],L,R).

%convert list of chars to ints
charToInt(X,Y):- char_code(X,Y).
charsToInts(B,R):- maplist(charToInt, B, R).

%convert list of ints to chars
intToChar(X,Y):- char_code(Y,X).
intsToChars(B,R):- maplist(intToChar, B, R).

%sort stack of blocks
sortStack(B,R):-charsToInts(B,I), sort(I,SI), intsToChars(SI,R).

%get first block in stack
firstBlockInStack(B,R):- sortStack(B,[R|_]).

%get first block in all stacks
firstBlock(B,R):- allBlocks(B,S), firstBlockInStack(S,R).

%get next block after input E in stack
nextBlockInStack(B,E,R):- charToInt(E,EC), charsToInts(B,C), removeLowerEqual(C,EC,T), intsToChars(T,[R|_]).

%get next block after input E in all stacks
nextBlock(B,E,R):- charToInt(E,EC), allBlocksSorted(B,S), charsToInts(S,C), removeLowerEqual(C,EC,T), intsToChars(T,[R|_]).

%check if block A is above input block C
blockAbove(B,C,A):- blockPos(B,C,CX,CY), blockPos(B,A,AX,AY), CY = AY, DX is AX - CX, DX = 1.

%get stack by index
getStack(B,I,S):-el_at(B,I,S).

%clear stack
%clearStack(B,I,R):-getStack(B,I,S), list_len(S,L), L > 0, moveblock(B,X,S1,S2)



%first block on the bottom
%test(B,R):- firstBlock(B,F), blockPos(B,F,X,Y), X = 0, nextBlock(B,F,N), blockAbove(B,F,N).





*/
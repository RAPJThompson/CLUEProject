/* 
Created In Collaboration By:
Robert Thompson 
Selene Baez


DOCUMENTATION

This system assits a user to play Clue, the boardgame. It has the ability to:
	set the number of players (2-6) and their names
	set the which player is using the system
	set how many cards the user holds (3-9) and which are those
	select which player starts the round, and determine the order of turns according to the order the names were input
During any other player's turn, the system is able to 
	keep track if that player made a suggestion, which it was and if someone showed the player a card.
During the user's turn, the system is able to
	tell the user if it is time to make an accusation
	show the database on demand
	make a proposal as to what suggestion to make
	keep track of what the user suggested and if he or she was shown a card

Currently, the system generates a proposal by looking at what the envelope has. It it knows for sure one of the cards in it, it then uses the information to create a proposal. if no information about the envelope is known, then the system generates a suggestion by searching a possiblility.

Some improvements involve:
	generate proposals by taking into account what the user holds and thus eliminate cards more easily
	make the code for setPlayers and myHand cleaner

The systems has to be loaded into SWI. Run it by typing 
		start.
Termination can happen if at any point the user just inputs
		.

*/
:- dynamic has/2, player/1, doesnotHave/2, asked/4, mightHave/4, nextTurn/2, currentTurn/1.

player(envelope).  

suspect(green).
suspect(plum).
suspect(mustard).
suspect(peacock).
suspect(scarlet).
suspect(white).

weapon(candlestick).
weapon(knife).
weapon(pipe).
weapon(revolver).
weapon(rope).
weapon(wrench).

room(conservatory).
room(lounge).
room(kitchen).
room(library).
room(hall).
room(study).
room(ballroom).
room(dining).
room(billiard).

envelopeHas(X) :- noPlayerHas(X) -> asserta(has(envelope,X)).

noOneHas(Card) :- noPlayerHas(Card), not(has(envelope, Card)).

noPlayerHas(Card) :- not((player(Player), Player \= envelope, (suspect(Card); weapon(Card); room(Card)), has(Player,Card))).

/*  ----------------------------------------- Initialization -------------------------------------------*/
/* Set basic information about the game */

start :- write('Hello! Ready for a game?'), nl, 
	 write('If at any point you want to stop the program, just type in a dot  -> . '), nl,
	 nl, setPlayers, nl, setMe, nl, myHand, nl, firstTurn.

/* set how many players will play */
setPlayers :- write('So, how many players will there be today?'), nl, read(PlayerCount), 
	      write('Please type in the names clockwise'), nl,
	      setPlayers(PlayerCount).

setPlayers(6):- write('Who is the first player?'), nl,
		read(P1), assert(player(P1)),
	 	write('Who is the second player?'), nl,
		read(P2), assert(player(P2)), assert(nextTurn(P1,P2)),
		write('Who is the third player?'), nl,
		read(P3), assert(player(P3)), assert(nextTurn(P2,P3)),
		write('Who is the fourth player?'), nl,
		read(P4), assert(player(P4)), assert(nextTurn(P3,P4)),
		write('Who is the fifth player?'), nl,
		read(P5), assert(player(P5)), assert(nextTurn(P4,P5)),
		write('Who is the sixth player?'), nl,
		read(P6), assert(player(P6)), assert(nextTurn(P5,P6)), assert(nextTurn(P6,P1)),
		write('Ok, we have players: '), write(P1),  write(', '), write(P2), write(', '), write(P3), write(', '), write(P4), write(', '), write(P5), write(' and '), write(P6), nl.

setPlayers(5):- write('Who is the first player?'), nl,
		read(P1), assert(player(P1)),
	 	write('Who is the second player?'), nl,
		read(P2), assert(player(P2)), assert(nextTurn(P1,P2)),
		write('Who is the third player?'), nl,
		read(P3), assert(player(P3)), assert(nextTurn(P2,P3)),
		write('Who is the fourth player?'), nl,
		read(P4), assert(player(P4)), assert(nextTurn(P3,P4)),
		write('Who is the fifth player?'), nl,
		read(P5), assert(player(P5)), assert(nextTurn(P4,P5)), assert(nextTurn(P5,P1)),
		write('Ok, we have players: '), write(P1),  write(', '), write(P2), write(', '), write(P3), write(', '), write(P4), write(' and '), write(P5), nl.

setPlayers(4):- write('Who is the first player?'), nl,
		read(P1), assert(player(P1)),
	 	write('Who is the second player?'), nl,
		read(P2), assert(player(P2)), assert(nextTurn(P1,P2)),
		write('Who is the third player?'), nl,
		read(P3), assert(player(P3)), assert(nextTurn(P2,P3)),
		write('Who is the fourth player?'), nl,
		read(P4), assert(player(P4)), assert(nextTurn(P3,P4)), assert(nextTurn(P4,P1)),
		write('Ok, we have players: '), write(P1),  write(', '), write(P2), write(', '), write(P3), write(' and '), write(P4), nl.

setPlayers(3):- write('Who is the first player?'), nl,
		read(P1), assert(player(P1)),
	 	write('Who is the second player?'), nl,
		read(P2), assert(player(P2)), assert(nextTurn(P1,P2)),
		write('Who is the third player?'), nl,
		read(P3), assert(player(P3)), assert(nextTurn(P2,P3)), assert(nextTurn(P3,P1)),
		write('Ok, we have players: '), write(P1),  write(', '), write(P2), write(' and '), write(P3), nl.

setPlayers(2):- write('Who is the first player?'), nl,
		read(P1), assert(player(P1)),
	 	write('Who is the second player?'), nl,
		read(P2), assert(player(P2)), assert(nextTurn(P1,P2)), assert(nextTurn(P2,P1)),
		write('Ok, we have players: '), write(P1), write(' and '), write(P2), nl.

/* set which player is the user */
setMe :- write('Who am I?'), nl,
	 read(ME), assert(us(ME)), nl.

myHand :- write('How many cards do we have?'), nl, read(CardCount), myHand(CardCount).

myHand(3) :- write('What is my first card?'), nl, printReminder,
		read(C1),
	  write('What is my second card?'), nl,
		read(C2),
          write('What is my third card?'), nl, 
		read(C3),
          us(W),
          assert(has(W, C1)), assert(has(W, C2)), assert(has(W, C3)),
          write('Ok, we have cards: '), write(C1),  write(', '), write(C2), write(' and '), write(C3), nl.

myHand(4) :- write('What is my first card?'), nl, printReminder,
		read(C1),
	  write('What is my second card?'), nl,
		read(C2),
          write('What is my third card?'), nl, 
		read(C3),
	  write('What is my fourth card?'), nl, 
		read(C4),
          us(W),
          assert(has(W, C1)), assert(has(W, C2)), assert(has(W, C3)), assert(has(W, C4)),
          write('Ok, we have cards: '), write(C1),  write(', '), write(C2), write(', '), write(C3), write(' and '), write(C4), nl.

myHand(5) :- write('What is my first card?'), nl, printReminder,
		read(C1),
	  write('What is my second card?'), nl,
		read(C2),
          write('What is my third card?'), nl, 
		read(C3),
          write('What is my fourth card?'), nl, 
		read(C4),
	  write('What is my fifth card?'), nl, 
		read(C5),
          us(W),
          assert(has(W, C1)), assert(has(W, C2)), assert(has(W, C3)), assert(has(W, C4)), assert(has(W, C5)),
          write('Ok, we have cards: '), write(C1),  write(', '), write(C2), write(', '), write(C3), write(', '), write(C4), write(' and '), write(C5), nl.

myHand(6) :- write('What is my first card?'), nl, printReminder,
		read(C1),
	  write('What is my second card?'), nl,
		read(C2),
          write('What is my third card?'), nl, 
		read(C3),
          write('What is my fourth card?'), nl, 
		read(C4),
	  write('What is my fifth card?'), nl, 
		read(C5),
	  write('What is my sixth card?'), nl, 
		read(C6),
          us(W),
          assert(has(W, C1)), assert(has(W, C2)), assert(has(W, C3)), assert(has(W, C4)), assert(has(W, C5)), assert(has(W, C6)),
          write('Ok, we have cards: '), write(C1),  write(', '), write(C2), write(', '), write(C3), write(', '), write(C4), write(', '), write(C5), write(' and '), write(C6), nl.

myHand(7) :- write('What is my first card?'), nl, printReminder,
		read(C1),
	  write('What is my second card?'), nl,
		read(C2),
          write('What is my third card?'), nl, 
		read(C3),
          write('What is my fourth card?'), nl, 
		read(C4),
	  write('What is my fifth card?'), nl, 
		read(C5),
	  write('What is my sixth card?'), nl, 
		read(C6),
	  write('What is my seventh card?'), nl, 
		read(C7),
          us(W),
          assert(has(W, C1)), assert(has(W, C2)), assert(has(W, C3)), assert(has(W, C4)), assert(has(W, C5)), assert(has(W, C6)), assert(has(W, C7)),
          write('Ok, we have cards: '), write(C1),  write(', '), write(C2), write(', '), write(C3), write(', '), write(C4), write(', '), write(C5), write(', '), write(C6), write(' and '), write(C7), nl.

myHand(8) :- write('What is my first card?'), nl, printReminder,
		read(C1),
	  write('What is my second card?'), nl,
		read(C2),
          write('What is my third card?'), nl, 
		read(C3),
          write('What is my fourth card?'), nl, 
		read(C4),
	  write('What is my fifth card?'), nl, 
		read(C5),
	  write('What is my sixth card?'), nl, 
		read(C6),
	  write('What is my seventh card?'), nl, 
		read(C7),
	  write('What is my eighth card?'), nl, 
		read(C8),
          us(W),
          assert(has(W, C1)), assert(has(W, C2)), assert(has(W, C3)), assert(has(W, C4)), assert(has(W, C5)), assert(has(W, C6)), assert(has(W, C7)), assert(has(W, C8)),
          write('Ok, we have cards: '), write(C1),  write(', '), write(C2), write(', '), write(C3), write(', '), write(C4), write(', '), write(C5), write(', '), write(C6), write(', '), write(C7), write(' and '), write(C8), nl.

myHand(9) :- write('What is my first card?'), nl, printReminder,
		read(C1),
	  write('What is my second card?'), nl,
		read(C2),
          write('What is my third card?'), nl, 
		read(C3),
          write('What is my fourth card?'), nl, 
		read(C4),
	  write('What is my fifth card?'), nl, 
		read(C5),
	  write('What is my sixth card?'), nl, 
		read(C6),
	  write('What is my seventh card?'), nl, 
		read(C7),
	  write('What is my eighth card?'), nl, 
		read(C8),
	  write('What is my eighth card?'), nl, 
		read(C9),
          us(W),
          assert(has(W, C1)), assert(has(W, C2)), assert(has(W, C3)), assert(has(W, C4)), assert(has(W, C5)), assert(has(W, C6)), assert(has(W, C7)), assert(has(W, C8)), assert(has(W, C9)),
          write('Ok, we have cards: '), write(C1),  write(', '), write(C2), write(', '), write(C3), write(', '), write(C4), write(', '), write(C5), write(', '), write(C6), write(', '), write(C7), write(', '), write(C8), write(' and '), write(C9), nl.

/* SET ORDER */
firstTurn :-   write('Who starts?'), nl,
	 	read(X), assert(currentTurn(X)), nl,
		((us(X)-> myTurn);
		((not(us(X)))-> otherTurn)).

/* print reminder to avoid misspelling*/
printReminder :- write('Reminder: The suspects are: green, plum, mustard, peacock, white, scarlet.'), nl, write('Reminder: The weapons are: candlestick, knife, pipe, revolver, rope, wrench.'), nl, write('Reminder: The rooms are: conservatory, lounge, kitchen, library, hall, study, ballroom, dining, billiard.'), nl.


/* -------------------------------------- general for turn  ----------------------------------*/

%%has(Player,Card).
%%doesnotHave(X, Card1).
%%mightHave(X,C1,C2,C3)

/* calculate next player and determine if it is us or not*/ 
switchTurn :- currentTurn(X), 
		nextTurn(X,Y), 
		retract(currentTurn(X)), 
		assert(currentTurn(Y)), nl,
		(((us(Y))-> (write('Our turn.'), myTurn));
		((not(us(Y)))-> (write(Y), write('\'s turn.'), otherTurn))).

/* retract useless information to avoid getting memory issues with database*/
clean :- ((mightHave(P1,C1,C2,C3), (has(P2,C1); doesnotHave(P1,C1)), (has(P2,C2); doesnotHave(P1,C2))) -> 
	 	(assert(has(P1,C3)), retract(mightHave(P1,C1,C2,C3)))),
	 ((mightHave(P1,C1,C2,C3), (has(P2,C1); doesnotHave(P1,C1)), (has(P2,C3); doesnotHave(P1,C3))) -> 
	 	(assert(has(P1,C2)), retract(mightHave(P1,C1,C2,C3)))),
	 ((mightHave(P1,C1,C2,C3), (has(P2,C2); doesnotHave(P1,C2)), (has(P2,C3); doesnotHave(P1,C3))) -> 
	 	(assert(has(P1,C1)), retract(mightHave(P1,C1,C2,C3)))).

/* make inferences about who has a card based on several card it might have */
infer :- ((mightHave(P1,C1,C2,C3), mightHave(P1,C1,C4,C5), C2\=C4, C3\=C5) -> 
                (assert(has(P1,C1)), retract(mightHave(P1,C1,C2,C3)), retract(mightHave(P1,C1,C4,C5)))),
	 ((mightHave(P1,C1,C4,C3), mightHave(P1,C2,C4,C5), C1\=C2, C3\=C5) -> 
                (assert(has(P1,C2)), retract(mightHave(P1,C1,C4,C3)), retract(mightHave(P1,C2,C4,C5)))),
         ((mightHave(P1,C1,C2,C3), mightHave(P1,C5,C4,C3), C1\=C5, C2\=C4) -> 
                (assert(has(P1,C3)), retract(mightHave(P1,C1,C2,C3)), retract(mightHave(P1,C5,C4,C3)))).

/* assert that everyone who did not discard my suggestion does not have any of the cards i asked about */
couldNotAnswer(FromPlayer, ToPlayer, Card1, Card2, Card3) :- (nextTurn(FromPlayer,Y), Y \= ToPlayer) -> 
                                                                               (asserta(doesnotHave(Y,Card1)), 
                                                                                asserta(doesnotHave(Y,Card2)), 
                                                                                asserta(doesnotHave(Y,Card3)), 
                                                                                couldNotAnswer(Y, ToPlayer, Card1, Card2, Card3)).
/* assert that whoever dicards a suggestion might have one of the cards asked for*/
answer(Answerer, C1, C2, C3) :- asserta(mightHave(Answerer,C1, C2, C3)).


/* ------------------------------ other's turn --------------------------------------------*/
/* make sure the player made a suggestion on its turn*/
otherTurn :- currentTurn(X), write('Did '), write(X), write(' guess? y/n'), nl, read(N), nl, (((N = 'y') -> otherTurnGuess(X));((N = 'n') -> switchTurn)).

/* get information about the guess and if someone answered*/
otherTurnGuess(X) :- write('What did '), write(X), write(' guess?'), nl, printReminder, nl, write('Which suspect was asked about?'), nl, 
		read(C1),
	  write('What weapon was asked about?'), nl,
		read(C2),
          write('What room was asked about?'), nl, 
		read(C3),
	 write('Did anyone Answer? y/n'), nl,
	  	read(R),
 	 (((R = 'y') ->  write('Who answered?'), nl, read(Z), answer(Z, C1, C2, C3), 
			 (couldNotAnswer(X, Z, C1, C2, C3);true));
	((R = 'n') ->  (asserta(mightHave(X, C1,C2,C3))), (
    			((has(X,C1), has(X,C2))-> (asserta(has(envelope,C3))));
			(((has(X,C1), has(X,C3))-> (asserta(has(envelope,C2)))));
			(((has(X,C2), has(X,C3))-> (asserta(has(envelope,C1)))));
			(true)
            ))),
	  ((infer,clean);true),
	  switchTurn.

/* ----------------------------------my turn ------------------------------------*/
/* make sure we got a chance to make a guess and suggest the user what to guess */
myTurn :- us(X),(solved;true), suggestGuess, nl, currentTurn(X), 
	  write('Do you want to check what we know by now? y/n'), nl, read(A), nl,
		(((A = 'y')->(database));true), nl,
	  write('Did we guess? y/n'), nl, read(N), nl, (((N = 'y') -> myTurnGuess(X));((N = 'n') -> switchTurn);true).

/* get information about what we guessed and what was shown to us */
myTurnGuess(X) :-  write('What did we guess?'), nl, printReminder, nl, write('Which suspect did we ask about?'), nl, read(C1),
	  write('What weapon did we ask about?'), nl,
		read(C2),
          write('What room did we ask about?'), nl, 
		read(C3),
	  write('Did anyone Answer? y/n'), nl,
	  	read(R),
 	 (((R = 'y') ->  write('Who answered?'), nl, read(Z), 
			 write('What did they show?'), nl, read(Card), shown(Z, Card), 
			 (couldNotAnswer(X, Z, C1, C2, C3);true));
	((R = 'n') ->  (
   			((has(X,C1), has(X,C2))-> (asserta(has(envelope,C3))));
			(((has(X,C1), has(X,C3))-> (asserta(has(envelope,C2)))));
			(((has(X,C2), has(X,C3))-> (asserta(has(envelope,C1)))));
			(true)
            ))),
	  ((infer,clean);true),
	  (solved;true), nl,
	  switchTurn.

/* suggest user based on what the envelope has*/
suggestGuess :- ((((has(envelope,X), suspect(X)) -> (write('We know '), write(X), write(' did it. So guess about '), write(X), nl));randomSuspectGuess),
		(((has(envelope,Y), weapon(Y)) -> (write('We know it was done with the '), write(Y), write('. So guess about the '), write(Y), nl));randomWeaponGuess),
		(((has(envelope,Z), room(Z)) -> (write('We know it was done in the '),write(Z), write('. So guess about '), write(Z), nl));randomRoomGuess));
		randomGuess.

/* choose whatever is left to make a suggestion*/
randomSuspectGuess :- suspect(X), noOneHas(X), !, write('We don\'t know who did it. So guess about '), write(X), nl.
randomWeaponGuess :- weapon(Y), noOneHas(Y), !, write('We don\'t know what it was done with. So guess about the '), write(Y), nl.
randomRoomGuess :- room(Z), noOneHas(Z), !, write('We don\'t know where it was done. So guess about the '), write(Z), nl.

/* check if the mistery is solved*/
solved :- (suspect(X), has(envelope,X), !, weapon(Y), has(envelope,Y), !, room(Z),  has(envelope,Z)) -> write('Hey, we can win now! Accusation: '), write(X), write(' with the '), write(Y), write(' in the '), write(Z), nl.

/* complete guess if nothing is known*/
randomGuess :- suspect(X),  noOneHas(X), !, 
         weapon(Y),noOneHas(Y), !, 
         room(Z), noOneHas(Z), !, 
         write('We should guess '), write(X), write(' with the '), write(Y), write(' in the '), write(Z), nl.

/* assert what cards we were shown by who*/
shown(Player, Card) :- asserta(has(Player, Card)).

/* print what we now*/
database :- forall(has(X,Y), printInfo(X,Y)).
printInfo(X,Y) :- write(X), write(' has the card '), write(Y), nl.




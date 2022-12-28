:- consult('io.pl').
:- consult('logic.pl').


%start.

start:-
  welcome_message,
  main_menu.


%main_menu.

main_menu :-
  repeat,
  format('What would you like to do?~n1 - Play.~n2 - Change gamemode.~n3 - Change board size.~n4 - leave.~n', []),
  read_line(Codes),
  catch(number_codes(Option, Codes),_, fail),
  Option > 0,
  Option < 5,
  (
    Option =:= 4;
    main_menu(Option),
    fail
  ).

main_menu(1):- play.
main_menu(2):- get_gamemode.
main_menu(3):- get_boardsize.

%play.
play:-
  size(Size),
  initial_state(Size, _),
  pvp.
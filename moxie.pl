:- consult('logic.pl').


%start.
%prints welcome message and starts menu loop.
start:-
  clear,
  welcome_message,
  main_menu.


%main_menu.
%menu with options to change some settings and play the game.
main_menu :-
  repeat,
  format('What would you like to do?~n1 - Play.~n2 - Change gamemode.~n3 - Change board size.~n4 - leave.~n', []),
  read_line(Codes),
  catch(number_codes(Option, Codes),_, fail),
  Option > 0,
  Option < 5,
  (
    Option is 4;
    Option is 1,
    clear,
    main_menu(1);
    clear,
    main_menu(Option),
    fail
  ).

main_menu(1):- play.
main_menu(2):- get_gamemode.
main_menu(3):- get_boardsize.

%play.
%starts the game loop
play:-
  size(Size),
  initial_state(Size),
  gamemode(Gamemode),
  clear,
  display_game,
  game(Gamemode).

%game(+Gamemode).
%main game loop
game(_):-
  game_over(Winner), 
  congratulate(Winner),
  start.

game(P1/P2):-
  game_state(T,_,_,_),
  Player is T mod 2,
  Player is 1,
  player_turn(1, P1),
  clear,
  display_game,
  line_win(1),
  game(P1/P2).

game(P1/P2):-
  game_state(T, _, _, _),
  Player is T mod 2,
  Player is 0,
  player_turn(2, P2),
  clear,
  display_game,
  line_win(2),
  game(P1/P2).
  




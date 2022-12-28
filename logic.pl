:-use_module(library(lists)).

%initial_state(+Size, ?Gamestate).

initial_state(Size, Board-1):-
    Size > 0,
    create_board(Size, Board),
    fill_board(Size, Board).

%create_board(+Size, -Board).
create_board(Size, Board):-
  length(Board, Size).

%fill_board(+Size, +Board).
fill_board(Size, Board):-
  maplist(fill_row(Size), Board).

% fill_row(+Size, -Row).
fill_row(Size, Row) :-
  length(Row, Size),
  maplist(=(0), Row).
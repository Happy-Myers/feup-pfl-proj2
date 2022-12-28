:-use_module(library(lists)).
:-consult('io.pl').

%initial_state(+Size, ?Gamestate).

initial_state(Size, Board-1):-
    Size > 0,
    create_board(Size, Board),
    fill_board(Size, Board),
    retract(board(_)),
    assert(board(Board)).

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

%pvp
pvp:-
  clear,
  format('pvp~n', []),
  game_state(T,_,_,_),
  P is T mod 2,
  display_game,
  player_turn(P).

player_turn(1):-
  format('Player 1 choose your move~n1 - move piece~n2 - place piece~n3 - capture piece~n', []),
  read_line(Code),
  catch(number_codes(C, Code), _, fail),
  choose_play(C,1).
player_turn(0):-
  format('Player 2 choose your move~n1 - move piece~n2 - place piece~n3 - capture piece~n', []),
  read_line(Code),
  catch(number_codes(C, Code), _, fail),
  choose_play(C,2).

player_turn(N):-
  format('invalid input~n', []),
  player_turn(N).

choose_play(1,P):-
  format('input row~n', []),
  read_line(Row1Code),
  catch(number_codes(Row1, Row1Code), _, fail),
  format('input col~n', []),
  read_line(Col1Code),
  catch(number_codes(Col1, Col1Code), _, fail),
  format('input row2~n', []),
  read_line(Row2Code),
  catch(number_codes(Row2, Row2Code), _, fail),
  format('input col2~n', []),
  read_line(Col2Code),
  catch(number_codes(Col2, Col2Code), _, fail),
  NCol1 is Col1 - 1,
  NCol2 is Col2 - 1,
  NRow1 is Row1 - 1,
  NRow2 is Row2 - 1,
  move_piece(P,NRow1,NCol1,NRow2,NCol2),
  retract(turnNum(N)),
  N1 is N+1,
  assert(turnNum(N1)),
  pvp.

choose_play(2,P):-
  format('input row~n', []),
  read_line(Row1Code),
  catch(number_codes(Row1, Row1Code), _, fail),
  format('input col~n', []),
  read_line(Col1Code),
  catch(number_codes(Col1, Col1Code), _, fail),
  NCol1 is Col1 - 1,
  NRow1 is Row1 - 1,
  put_piece(P,NRow1,NCol1),
  retract(turnNum(N)),
  N1 is N+1,
  assert(turnNum(N1)),
  pvp.

choose_play(3,P):-
  format('input row~n', []),
  read_line(Row1Code),
  catch(number_codes(Row1, Row1Code), _, fail),
  format('input col~n', []),
  read_line(Col1Code),
  catch(number_codes(Col1, Col1Code), _, fail),
  format('input row2~n', []),
  read_line(Row2Code),
  catch(number_codes(Row2, Row2Code), _, fail),
  format('input col2~n', []),
  read_line(Col2Code),
  catch(number_codes(Col2, Col2Code), _, fail),
  NCol1 is Col1 - 1,
  NCol2 is Col2 - 1,
  NRow1 is Row1 - 1,
  NRow2 is Row2 - 1,
  eat_piece(P,NRow1,NCol1,NRow2,NCol2),
  retract(turnNum(N)),
  N1 is N+1,
  assert(turnNum(N1)),
  pvp.

choose_play(_,_):-
  format('invalid input~n', []),
  pvp.


%board functions

%game_state(-turnNum,-P1State,-P2State,-Board)
game_state(T,P1,P2,B):-
  turnNum(T),
  p1state(P1),
  p2state(P2),
  board(B).

:- dynamic turnNum/1, p1state/1, p2state/1.
turnNum(1).
p1state(0).
p2state(0).

%board functions

%get_position(+Row,+Col,?N)
get_position(Row,Col,N):-
  Row >=0,
  Row <4,
  Col >=0,
  Col <4,
  board(B),
  get_position_aux(Row,Col,B,N).

get_position_aux(0,0,[[H|_]|_],H).
get_position_aux(0,Col,[[_|T]|_],N):-
  Col1 is Col -1,
  get_position_aux(0,Col1,[T|_],N).
get_position_aux(Row,Col,[_|T],N):-
  Row1 is  Row-1,
  get_position_aux(Row1,Col,T,N).

%is_empty(+Row,+Col)
is_empty(Row,Col):-
  get_position(Row,Col,N),
  !,
  N is 0.

%move functions

%put_piece(+C,+Row,+Col)
put_piece(C,Row,Col):-
  is_empty(Row,Col),
  board(B),
  retract(board(B)),
  replace(B,Row,Col,C,B2),
  assert(board(B2)).
/*put_piece(_,_,_):-
  print('Invalid play\n').*/

replace([[_|T]|Tail],0,0,C,[[C|T]|Tail]).
replace([[H|T]|Tail],0,Col,C,[[H|T2]|Tail2]):-
  Col1 is Col -1,
  replace([T|Tail],0,Col1,C,[T2|Tail2]).
replace([H|T],Row,Col,C,[H|T2]):-
  Row1 is Row -1,
  replace(T,Row1,Col,C,T2).

%move_piece(+C,+Row,+Col,+Row2,+Col2)
move_piece(C,Row,Col,Row2,Col2):-
  distance(Row,Row2),
  distance(Col,Col2),
  is_empty(Row2,Col2),
  get_position(Row,Col,Num),
  !,
  Num is C,
  board(B),
  retract(board(B)),
  replace(B,Row,Col,0,B2),
  replace(B2,Row2,Col2,C,B3),
  assert(board(B3)).
/*move_piece(_,_,_,_,_):-
  print('Invalid play\n').*/

distance(N1,N2):-
  N1 is N2.
distance(N1,N2):-
  N1 is N2 -1.
distance(N1,N2):-
  N1 is N2 +1.

%eat_piece(+C,+Row,+Col,+Row2,+Col2)
eat_piece(C,Row,Col,Row2,Col2):-
  check_pieces(Row,Col,Row2,Col2,Row3,Col3),
  get_position(Row,Col,Num1),
  !,
  Num1 is C,
  get_position(Row2,Col2,Num2),
  !,
  \+ Num1 is Num2,
  board(B),
  retract(board(B)),
  replace(B,Row,Col,0,B2),
  replace(B2,Row2,Col2,0,B3),
  replace(B3,Row3,Col3,C,B4),
  assert(board(B4)).
/*eat_piece(_,_,_,_,_):-
  print('Invalid play\n').*/

check_pieces(Row,Col,Row2,Col2,Row3,Col3):-
  Row is Row2 +1,
  Col is Col2 +1,
  Row3 is Row2 -1,
  Col3 is Col2 -1.
check_pieces(Row,Col,Row2,Col2,Row3,Col3):-
  Row is Row2 -1,
  Col is Col2 -1,
  Row3 is Row2 +1,
  Col3 is Col2 +1.
check_pieces(Row,Col,Row2,Col2,Row3,Col3):-
  Row is Row2 +1,
  Col is Col2 -1,
  Row3 is Row2 -1,
  Col3 is Col2 +1.
check_pieces(Row,Col,Row2,Col2,Row3,Col3):-
  Row is Row2 -1,
  Col is Col2 +1,
  Row3 is Row2 +1,
  Col3 is Col2 -1.

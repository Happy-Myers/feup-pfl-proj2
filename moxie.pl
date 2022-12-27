%menu functions

%play
play:-
  print('1. Play\n'),
  print('2. Settings\n'),
  get_char(C),
  clear_buffer,
  read_input(C).

read_input('1'):-
  print('Playing\n'),
  play_menu.
read_input('2'):-
  print('Settings\n'),
  settings_menu.
read_input(_):-
  print('invalid input try again\n'),
  play.

play_menu:-
  print('Choose game mode\n'),
  print('1. PvP\n'),
  print('2. PvE\n'),
  print('3. EvE\n'),
  get_char(C),
  clear_buffer,
  read_play_input(C).

read_play_input('1'):-
  print('Playing PvP\n'),
  pvp.
read_play_input('2'):-
  print('PvE\n').
read_play_input('3'):-
  print('EvE\n').
read_play_input(_):-
  print('Invalid input try again\n'),
  play_menu.

%pvp
pvp:-
  board_size(N),
  create_empty_board(N,B),
  assert(board(B,N)),
  print_board.

%board functions

:- dynamic board_size/1.
board_size(4).

%create_empty_board(+N,?B)
create_empty_board(_,B):-
  B is [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]].


:- dynamic board/2.

%get_position(+Row,+Col,?N)
get_position(Row,Col,N):-
  Row >=0,
  Row <4,
  Col >=0,
  Col <4,
  board(B,_),
  get_position_aux(Row,Col,B,N).

get_position_aux(0,0,[[H|_]|_],H).
get_position_aux(0,Col,[[_|T]|_],N):-
  Col1 is Col -1,
  get_position_aux(0,Col1,[T|_],N).
get_position_aux(Row,Col,[_|T],N):-
  Row1 is  Row-1,
  get_position_aux(Row1,Col,T,N).

%print_board
print_board:-
  board([A,B,C,D],_),
  print_line(A),
  print_line(B),
  print_line(C),
  print_line(D).

print_line([A,B,C,D]):-
  print(A),
  print(B),
  print(C),
  print(D),
  print('\n').

%is_empty(+Row,+Col)
is_empty(Row,Col):-
  get_position(Row,Col,N),
  !,
  N is 0.

%move functions

%put_piece(+C,+Row,+Col)
put_piece(C,Row,Col):-
  is_empty(Row,Col),
  board(B,N),
  retract(board(B,N)),
  replace(B,Row,Col,C,B2),
  assert(board(B2,N)).

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
  board(B,N),
  retract(board(B,N)),
  replace(B,Row,Col,0,B2),
  replace(B2,Row2,Col2,C,B3),
  assert(board(B3,N)).

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
  board(B,N),
  retract(board(B,N)),
  replace(B,Row,Col,0,B2),
  replace(B2,Row2,Col2,0,B3),
  replace(B3,Row3,Col3,C,B4),
  assert(board(B4,N)).
  

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


% clear_buffer.
% Clears the input buffer.
clear_buffer:-
    repeat,
    get_char(C),
    C = '\n'.
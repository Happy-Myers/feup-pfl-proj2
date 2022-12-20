board([
  [1,1,0,0],
  [0,2,0,0],
  [0,2,1,0],
  [0,0,1,0]]).

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

print_board:-
  board([A,B,C,D]),
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

put_piece(1,Row,Col):-
  is_empty(Row,Col).


is_empty(Row,Col):-
  get_position(Row,Col,N),
  !,
  N is 0.
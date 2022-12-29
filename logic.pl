:-use_module(library(lists)).
:-consult('io.pl').

%initial_state(+Size, ?Gamestate).

initial_state(Size, Board):-
    Size > 0,
    create_board(Size, Board),
    fill_board(Size, Board),
    retract(board(_)),
    assert(board(Board)),
    retract(p1state(_)),
    retract(p2state(_)),
    retract(turnNum(_)),
    assert(p1state(0)),
    assert(p2state(0)),
    assert(turnNum(1)),
    Npieces is Size * Size // 2,
    retractall(pieces(_, _)),
    assert(pieces(1, Npieces)),
    assert(pieces(2, Npieces)).

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

%game_over(Winner).

game_over(Winner):-
  p1state(6),
  Winner is 1.

game_over(Winner):-
  p2state(6),
  Winner is 2.

% player_turn(+Player, +PlayerType).

player_turn(Player, h):-
  board(B),
  check_eat(Player, B),
  format('Player ~a choose your move~n1 - capture piece~n', [Player]),
  read_line(Code),
  catch(number_codes(1, Code), _, fail),
  choose_play(3,Player).

player_turn(Player, h):-
  board(B),
  check_board(Player, B),
  format('Player ~a choose your move~n1 - move piece~n2 - place piece~n', [Player]),
  read_line(Code),
  catch(number_codes(C, Code), _, fail),
  C>0, C<3,
  choose_play(C,Player).

player_turn(Player, h):-
  format('Player ~a choose your move~n1 - place piece~n', [Player]),
  read_line(Code),
  catch(number_codes(1, Code), _, fail),
  choose_play(2,Player).

player_turn(Player, h):-
  format('invalid input~n', []),
  player_turn(Player).

%check_board(+Player, +Board).
check_board(Player, [[Player|_]|_]).

check_board(Player, [[H|T]|Tail]):-
  H \= Player,
  check_board(Player, [T|Tail]).

check_board(Player, [[]|Tail]):-
  check_board(Player, Tail).


%get_coord(-X, -Y).
get_coord(X, Y):-
  format('input row~n', []),
  read_line(YCode),
  catch(number_codes(Y, YCode), _, fail),
  format('input col~n', []),
  read_line(XCode),
  catch(number_codes(X, XCode), _, fail).


%choose_play(+Play,+Player)
choose_play(1,Player):- % move
  format('piece coordinates:~n', []),
  get_coord(Row1, Col1),
  format('destination coordinates:~n', []),
  get_coord(Row2, Col2),
  NCol1 is Col1 - 1,
  NCol2 is Col2 - 1,
  NRow1 is Row1 - 1,
  NRow2 is Row2 - 1,
  move_piece(Player,NRow1,NCol1,NRow2,NCol2),
  retract(turnNum(N)),
  N1 is N+1,
  assert(turnNum(N1)).

choose_play(2,Player):- % place
  format('Where do you want to place the piece? ~n'),
  get_coord(Row, Col),
  NCol is Col - 1,
  NRow is Row - 1,
  put_piece(Player,NRow,NCol),
  retract(turnNum(N)),
  N1 is N+1,
  assert(turnNum(N1)).

choose_play(3,Player):- % eat
  format('Piece to move:~n', []),
  get_coord(Row1, Col1),
  format('Piece to eat:~n', []),
  get_coord(Row2, Col2),
  NCol1 is Col1 - 1,
  NCol2 is Col2 - 1,
  NRow1 is Row1 - 1,
  NRow2 is Row2 - 1,
  eat_piece(Player,NRow1,NCol1,NRow2,NCol2),
  retract(turnNum(N)),
  N1 is N+1,
  assert(turnNum(N1)),
  add_point(Player).

choose_play(_,_):-
  format('invalid input~n', []).

%add_point(Player)
add_point(1):-
  retract(p1state(Points)),
  Points1 is Points +1,
  assert(p1state(Points1)).

add_point(2):-
  retract(p2state(Points)),
  Points1 is Points +1,
  assert(p2state(Points1)).

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

%line_of_three(+Board, +Player).

line_of_three(Board, Player) :- %horizontal
  member(Row, Board),
  member(Player, Row),
  nth0(I, Row, Player),
  I1 is I + 1,
  I2 is I + 2,
  nth0(I1, Row, Player),
  nth0(I2, Row, Player).
line_of_three(Board, Player) :- %vertical
  transpose(Board, TransposedBoard),
  member(Col, TransposedBoard),
  member(Player, Col),
  nth0(I, Col, Player),
  I1 is I + 1,
  I2 is I + 2,
  nth0(I1, Col, Player),
  nth0(I2, Col, Player).
line_of_three(Board, Player) :- %diagonal (top left to bottom right)
  member(Row, Board),
  nth0(I, Board, Row),
  I1 is I + 1,
  I2 is I + 2,
  nth0(I1, Board, Row1),
  nth0(I2, Board, Row2),
  nth0(N, Row, Player),
  N1 is N + 1,
  N2 is N + 2,
  nth0(N1, Row1, Player),
  nth0(N2, Row2, Player).
line_of_three(Board, Player) :- %diagonal (top right to bottom left)
  member(Row, Board),
  nth0(I, Board, Row),
  I1 is I + 1,
  I2 is I + 2,
  nth0(I1, Board, Row1),
  nth0(I2, Board, Row2),
  nth0(N, Row2, Player),
  N1 is N + 1,
  N2 is N + 2, 
  nth0(N1, Row1, Player),
  nth0(N2, Row, Player).


%line_win(+Player).

line_win(Player):-
  Player =:= 1,
  board(B),
  line_of_three(B, Player),
  retract(p1state(_)),
  assert(p1state(6)).
line_win(Player):-
  Player =:= 2,
  board(B),
  line_of_three(B, Player),
  retract(p2state(_)),
  assert(p2state(6)). 
line_win(_).

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

%replace(+Board,+Row,+Col,)
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

%distance(+N1,+N2)
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
  get_position(Row3,Col3,Num3),
  !,
  Num3 is 0,
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

%check_eat(+Player, +Board).

check_eat(Player,[[]|Tail]):-
  check_eat(Player,Tail).

check_eat(Player,[[Player|T]|[H1,H2|_]]):-
  length(T,Len1),
  Len1 > 0,
  length([H|T1],Len1),
  append(_,[H|T1],H1),
  H \= Player,
  H \= 0,
  Len2 is Len1-1,
  Len2 > -1,
  length([H3|T2],Len2),
  append(_,[H3|T2],H2),
  H3 is 0.

check_eat(Player,[[Player|T]|[H1,H2|_]]):-
  length(T,Len),
  Len1 is Len +2,
  length([H|T1],Len1),
  append(_,[H|T1],H1),
  H \= Player,
  H \= 0,
  Len2 is Len1+1,
  length([H3|T2],Len2),
  append(_,[H3|T2],H2),
  H3 is 0.

check_eat(Player,[[0|T]|[H1,H2|_]]):-
  length(T,Len1),
  Len1 > -1,
  length([H|T1],Len1),
  append(_,[H|T1],H1),
  H \= Player,
  H \= 0,
  Len2 is Len1-1,
  Len2 > -1,
  length([H3|T2],Len2),
  append(_,[H3|T2],H2),
  H3 is Player.

check_eat(Player,[[0|T]|[H1,H2|_]]):-
  length(T,Len),
  Len1 is Len +2,
  length([H|T1],Len1),
  append(_,[H|T1],H1),
  H \= Player,
  H \= 0,
  Len2 is Len1+1,
  length([H3|T2],Len2),
  append(_,[H3|T2],H2),
  H3 is Player.

check_eat(Player,[[_|T]|Tail]):-
  check_eat(Player,[T|Tail]).

  

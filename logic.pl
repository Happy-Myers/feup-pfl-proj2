:-use_module(library(lists)).
:-consult('io.pl').

%initial_state(+Size, ?Gamestate).

initial_state(Size, Board-1):-
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
    assert(turnNum(1)).

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
  game_state(_,6,_,_),
  clear,
  display_game,
  format('Player 1 wins by captures~n',[]),
  main_menu.

pvp:-
  game_state(_,_,6,_),
  clear,
  display_game,
  format('Player 2 wins by captures~n',[]),
  main_menu.

pvp:-
  game_state(_,_,_,B),
  win_by_line(B),
  display_game,
  main_menu.
pvp:-
  %clear,
  game_state(T,Points1,Points2,_),
  P is T mod 2,
  format('Player1 points: ~d~nPlayer2 points: ~d~n', [Points1, Points2]),
  display_game,
  player_turn(P).

%player_turn(+Player)
player_turn(1):-
  check_eat(1),
  format('Player 1 choose your move~n1 - capture piece~n', []),
  read_line(Code),
  catch(number_codes(1, Code), _, fail),
  choose_play(3,1).
player_turn(0):-
  check_eat(2),
  format('Player 2 choose your move~n1 - capture piece~n', []),
  read_line(Code),
  catch(number_codes(1, Code), _, fail),
  choose_play(3,2).
player_turn(1):-
  board(B),
  check_board(1, B),
  format('Player 1 choose your move~n1 - move piece~n2 - place piece~n', []),
  read_line(Code),
  catch(number_codes(C, Code), _, fail),
  C>0, C<3,
  choose_play(C,1).
player_turn(0):-
  board(B),
  check_board(2, B),
  format('Player 2 choose your move~n1 - move piece~n2 - place piece~n', []),
  read_line(Code),
  catch(number_codes(C, Code), _, fail),
  C>0, C<3,
  choose_play(C,2).
player_turn(1):-
  format('Player 1 choose your move~n1 - place piece~n', []),
  read_line(Code),
  catch(number_codes(1, Code), _, fail),
  choose_play(2,1).
player_turn(0):-
  format('Player 2 choose your move~n1 - place piece~n', []),
  read_line(Code),
  catch(number_codes(1, Code), _, fail),
  choose_play(2,2).
player_turn(N):-
  format('invalid input~n', []),
  player_turn(N).

%check_board(+Player, +Board).
check_board(P, [[H|_]|_]):-
  P =:= H.
check_board(P, [[H|T]|Tail]):-
  H \= P,
  check_board(P, [T|Tail]).
check_board(P, [[]|Tail]):-
  check_board(P, Tail).


%choose_play(+Play,+Player)
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
  add_point(P),
  pvp.
choose_play(_,_):-
  format('invalid input~n', []),
  pvp.

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

%win_by_line(+Board)
win_by_line(B):-
  win_by_line_aux(B,0,P,0),
  format('Player ~d wins by horizontal line~n',[P]).
win_by_line(B):-
  transpose(B,B1),
  win_by_line_aux(B1,0,P,0),
  format('Player ~d wins by line vertical~n',[P]).
win_by_line(B):-
  win_by_line_aux2(B,0,P,0),
  format('Player ~d wins by diagonar 1 line~n',[P]).
win_by_line(B):-
  win_by_line_aux3(B,P),
  format('Player ~d wins by dioganla 2 line~n',[P]).

win_by_line_aux(_,3,P,P).
win_by_line_aux([[]|Tail],_,P,_):-
  win_by_line_aux(Tail,0,P,0).
win_by_line_aux([[0|T]|Tail],_,P,_):-
  win_by_line_aux([T|Tail],0,P,0).
win_by_line_aux([[1|T]|Tail],_,P,0):-
  win_by_line_aux([T|Tail],1,P,1).
win_by_line_aux([[2|T]|Tail],_,P,0):-
  win_by_line_aux([T|Tail],1,P,2).
win_by_line_aux([[1|T]|Tail],Aux,P,1):-
  Aux1 is Aux+1,
  win_by_line_aux([T|Tail],Aux1,P,1).
win_by_line_aux([[2|T]|Tail],Aux,P,2):-
  Aux1 is Aux+1,
  win_by_line_aux([T|Tail],Aux1,P,2).
win_by_line_aux([[1|T]|Tail],_,P,2):-
  win_by_line_aux([T|Tail],0,P,1).
win_by_line_aux([[2|T]|Tail],_,P,1):-
  win_by_line_aux([T|Tail],0,P,2).

win_by_line_aux2(_,3,P,P).
win_by_line_aux2([[0|T]|Tail],_,P,_):-
  win_by_line_aux2([T|Tail],0,P,0).
win_by_line_aux2([[H|T]|[Head|Tail]],Aux,P,H):-
  \+H is 0,
  Aux1 is Aux+1,
  length(T,Len),
  length(List,Len),
  append(_,List,Head),
  Aux1 is Aux+1,
  win_by_line_aux2([List|Tail],Aux1,P,H).
win_by_line_aux2([[H|T]|[Head|Tail]],_,P,_):-
  length(T,Len),
  length(List,Len),
  append(_,List,Head),
  win_by_line_aux2([List|Tail],1,P,H).

win_by_line_aux3([[]|Tail],P):-
  win_by_line_aux3(Tail,P).
win_by_line_aux3([[H|T]|[Head,Head2|_]],H):-
  \+H is 0,
  length(T,Len),
  length([H1|T1],Len),
  append(_,[H1|T1],Head),
  H1 is H,
  Len1 is Len-1,
  Len1 > -1,
  length([H2|T2],Len1),
  append(_,[H2|T2],Head2),
  H2 is H.
win_by_line_aux3([[H|T]|[Head,Head2|_]],H):-
  \+H is 0,
  length(T,Len),
  Len1 is Len +2,
  length([H1|T1],Len1),
  append(_,[H1|T1],Head),
  H1 is H,
  Len2 is Len1+1,
  length([H2|T2],Len2),
  append(_,[H2|T2],Head2),
  H2 is H.
win_by_line_aux3([[_|T]|Tail],P):-
  win_by_line_aux3([T|Tail],P).

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

%replace(Board,Row,Col,)
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

%check_eat(+Player)
check_eat(P):-
  board(B),
  check_eat_aux(P,B).

check_eat_aux(P,[[]|Tail]):-
  check_eat_aux(P,Tail).
check_eat_aux(P,[[P|T]|[H1,H2|_]]):-
  length(T,Len1),
  Len1 > -1,
  length([H|T1],Len1),
  append(_,[H|T1],H1),
  \+ H is P,
  \+ H is 0,
  Len2 is Len1-1,
  Len2 > -1,
  length([H3|T2],Len2),
  append(_,[H3|T2],H2),
  H3 is 0.
check_eat_aux(P,[[P|T]|[H1,H2|_]]):-
  length(T,Len),
  Len1 is Len +2,
  length([H|T1],Len1),
  append(_,[H|T1],H1),
  \+ H is P,
  \+ H is 0,
  Len2 is Len1+1,
  length([H3|T2],Len2),
  append(_,[H3|T2],H2),
  H3 is 0.
check_eat_aux(P,[[0|T]|[H1,H2|_]]):-
  length(T,Len1),
  Len1 > -1,
  length([H|T1],Len1),
  append(_,[H|T1],H1),
  \+ H is P,
  \+ H is 0,
  Len2 is Len1-1,
  Len2 > -1,
  length([H3|T2],Len2),
  append(_,[H3|T2],H2),
  H3 is P.
check_eat_aux(P,[[0|T]|[H1,H2|_]]):-
  length(T,Len),
  Len1 is Len +2,
  length([H|T1],Len1),
  append(_,[H|T1],H1),
  \+ H is P,
  \+ H is 0,
  Len2 is Len1+1,
  length([H3|T2],Len2),
  append(_,[H3|T2],H2),
  H3 is P.
check_eat_aux(P,[[_|T]|Tail]):-
  check_eat_aux(P,[T|Tail]).
  

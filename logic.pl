:-use_module(library(lists)).
:-consult('io.pl').

%initial_state(+Size, ?Gamestate).

initial_state(Size, Board):-
    Size > 0,
    create_board(Size, Board),
    fill_board(Size, Board),
    retract(board(_)),
    assert(board(Board)),
    retractall(player_state(_,_)),
    retract(turnNum(_)),
    assert(player_state(1,0)),
    assert(player_state(2,0)),
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
  player_state(Winner,6).


% player_turn(+Player, +PlayerType).

player_turn(Player, h):-
  check_eat(Player),
  format('Player ~d choose your move~n1 - capture piece~n', [Player]),
  read_line(Code),
  catch(number_codes(1, Code), _, fail),
  choose_play(3,Player).

player_turn(Player, h):-
  pieces(Player, 0),
  format('Player ~d choose your move~n1 - move piece~n', [Player]),
  read_line(Code),
  catch(number_codes(1, Code), _, fail),
  choose_play(1, Player).

player_turn(Player, h):-
  board(B),
  check_board(Player, B),
  format('Player ~d choose your move~n1 - move piece~n2 - place piece~n', [Player]),
  read_line(Code),
  catch(number_codes(C, Code), _, fail),
  C>0, C<3,
  choose_play(C,Player).

player_turn(Player, h):-
  format('Player ~d choose your move~n1 - place piece~n', [Player]),
  read_line(Code),
  catch(number_codes(1, Code), _, fail),
  choose_play(2,Player).

player_turn(Player, h):-
  format('invalid input~n', []),
  player_turn(Player, h).

%check_board(+Player, +Board).
check_board(Player, Board):-
  member(Row, Board),
  member(Player, Row).

%get_coord(-X, -Y).
get_coord(X, Y):-
  format('input row~n', []),
  read_line(XCode),
  catch(number_codes(X, XCode), _, fail),
  format('input col~n', []),
  read_line(YCode),
  catch(number_codes(Y, YCode), _, fail).


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
  format('Where do you want to place the piece? ~n', []),
  get_coord(Row, Col),
  NCol is Col - 1,
  NRow is Row - 1,
  place_piece(Player,NRow,NCol),
  remove_piece(Player),
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
add_point(Player):-
  retract(player_state(Player, Points)),
  Points1 is Points +1,
  assert(player_state(Player, Points1)).


%remove_piece(Player)

remove_piece(Player):-
  retract(pieces(Player, Pieces)),
  Pieces1 is Pieces - 1,
  assert(pieces(Player, Pieces1)).

%board functions

%game_state(-turnNum,-P1State,-P2State,-Board)
game_state(T,P1,P2,B):-
  turnNum(T),
  player_state(1, P1),
  player_state(2, P2),
  board(B).

:- dynamic turnNum/1, player_state/2.
turnNum(1).
player_state(1, 0).
player_state(2, 0).

%board functions

%get_position(+Row,+Col,?N)
get_position(Row, Col, N):-
  size(Size),
  board(B),
  Row >= 0,
  Row < Size,
  Col >= 0, 
  Col < Size,
  nth0(Row, B, Row1),
  nth0(Col, Row1, N).

%is_empty(+Row,+Col).
is_empty(Row,Col):-
  get_position(Row,Col,0).

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
  board(B),
  line_of_three(B, Player),
  retract(player_state(Player, _)),
  assert(player_state(Player, 6)).

%move functions

%place_piece(+C,+Row,+Col)
place_piece(Player,Row,Col):-
  valid_place(Player, Row, Col),
  board(B),
  retract(board(B)),
  replace(B,Player,Row,Col,B2),
  assert(board(B2)).


replace(Board, Value, Row, Col, Result):-
  nth0(Row, Board, Row1),
  nth0(Col, Row1, Element),
  append(P1, [Element|T], Row1),
  length(P1, Col),
  append(P1, [Value|T], Row2),
  append(P2, [Row1|Tail], Board),
  length(P2, Row),
  append(P2, [Row2|Tail], Result).
  


%move_piece(+C,+Row,+Col,+Row2,+Col2)
move_piece(Player,Row1,Col1,Row2,Col2):-
  valid_move(Player, Row1, Col1, Row2, Col2),
  board(B),
  retract(board(B)),
  replace(B,0,Row1,Col1,B2),
  replace(B2,Player,Row2,Col2,B3),
  assert(board(B3)).



%eat_piece(+Player,+Row,+Col,+Row2,+Col2)
eat_piece(Player, Row1,Col1,Row2,Col2):-
  valid_eat(Player, Row1, Col1, Row2, Col2),
  retract(board(B)),
  get_landing(Row1, Col1, Row2, Col2, Row3, Col3),
  replace(B,0,Row1,Col1,B2),
  replace(B2,0,Row2,Col2,B3),
  replace(B3,Player,Row3,Col3,B4),
  assert(board(B4)).

/*eat_piece(_,_,_,_,_):-
  print('Invalid play\n').*/

%check_eat(+Player).
check_eat(Player):-
  findall((X1-Y1, X2-Y2), valid_eat(Player, X1, Y1, X2, Y2), [_|_]).

%valid_place(+Player, +Row, +Col).
valid_place(Player, Row, Col):-
  pieces(Player, Pieces),
  Pieces > 0,
  is_empty(Row, Col).

%valid_move(+Player, +Row, +Col, +DestRow, +DestCol).
valid_move(Player, Row, Col, DestRow, DestCol):-
  get_position(Row, Col, Player),
  adjacent(Row, Col, DestRow, DestCol),
  is_empty(DestRow, DestCol).

%valid_eat(+Player, +Row, +Col, +DestRow, +DestCol).
valid_eat(Player, Row, Col, DestRow, DestCol):-
  get_position(Row, Col, Player),
  adjacent(Row, Col, DestRow, DestCol),
  Player2 is Player mod 2 +1,
  get_position(DestRow, DestCol, Player2),
  get_landing(Row, Col, DestRow, DestCol, FinalRow, FinalCol),
  is_empty(FinalRow, FinalCol).

get_landing(Row, Col, DestRow, DestCol, FinalRow, FinalCol):-
  MoveX is DestRow - Row,
  MoveY is DestCol - Col,
  FinalRow is DestRow + MoveX,
  FinalCol is DestCol + MoveY.

  
%adjacent(+Row1, +Col1, +Row2, +Col2).
adjacent(Row1, Col1, Row2, Col2) :- 
  Row1 =:= Row2-1, 
  Col1 =:= Col2. 
adjacent(Row1, Col1, Row2, Col2) :- 
  Row1 =:= Row2+1, 
  Col1 =:= Col2. 
adjacent(Row1, Col1, Row2, Col2) :- 
  Row1 =:= Row2, 
  Col1 =:= Col2-1. 
adjacent(Row1, Col1, Row2, Col2) :- 
  Row1 =:= Row2, 
  Col1 =:= Col2+1. 
adjacent(Row1, Col1, Row2, Col2) :- 
  Row1 =:= Row2-1, 
  Col1 =:= Col2-1.
adjacent(Row1, Col1, Row2, Col2) :- 
  Row1 =:= Row2+1, 
  Col1 =:= Col2-1.  
adjacent(Row1, Col1, Row2, Col2) :- 
  Row1 =:= Row2-1, 
  Col1 =:= Col2+1.  
adjacent(Row1, Col1, Row2, Col2) :- 
  Row1 =:= Row2+1, 
  Col1 =:= Col2+1.  
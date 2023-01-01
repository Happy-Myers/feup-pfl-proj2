:-use_module(library(lists)).
:-use_module(library(between)).
:-use_module(library(random)).
:-use_module(library(system)).
:-consult('io.pl').

%init_random_state.
%initializes seed for random number generator
init_random_state:-
  now(X),
  setrand(X).

%initial_state(+Size, ?Size).
%initializes all important variables.
initial_state(Size):-
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
%crates a list with adequate size.
create_board(Size, Board):-
  length(Board, Size).

%fill_board(+Size, +Board).
%fills the board list with empty lists.
fill_board(Size, Board):-
  maplist(fill_row(Size), Board).

% fill_row(+Size, -Row).
%initializes the empty lists with 0's
fill_row(Size, Row) :-
  length(Row, Size),
  maplist(=(0), Row).

%game_over(Winner).
%detects finished game
game_over(Winner):-
  player_state(Winner,6).

% player_turn(+Player, +PlayerType).
% gets the options available to the players and gets input on the kind of play to make.(human)
% chooses a random move to make (pc1).
% chooses the best move to make (pc2).
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

player_turn(Player, pc1):-
  init_random_state,
  board(Board),
  valid_plays(Player, Board, Plays),
  random_select(Play, Plays, _),
  sleep(3),
  retract(turnNum(Num)),
  Num1 is Num +1,
  assert(turnNum(Num1)),
  bot_play(Player, Board, Play).

player_turn(Player, pc2):-
  board(Board),
  valid_plays(Player, Board, Plays),
  best_play(Player, Plays, Board, BestPlay, _),
  sleep(3),
  retract(turnNum(Num)),
  Num1 is Num +1,
  assert(turnNum(Num1)),
  bot_play(Player, Board, BestPlay).

%check_board(+Player, +Board).
%checks if the current player has any pieces on the board to move.
check_board(Player, Board):-
  member(Row, Board),
  member(Player, Row).

%get_coord(-X, -Y).
% prompts the player for coordinate input and processes it.
get_coord(X, Y):-
  format('input row~n', []),
  read_line(XCode),
  catch(number_codes(X, XCode), _, fail),
  format('input col~n', []),
  read_line(YCode),
  catch(number_codes(Y, YCode), _, fail).


%choose_play(+Play,+Player)
%gets coordinates needed to make the play and advances to next turn.
choose_play(1,Player):- % move
  format('piece coordinates:~n', []),
  get_coord(Row1, Col1),
  format('destination coordinates:~n', []),
  get_coord(Row2, Col2),
  NCol1 is Col1 - 1,
  NCol2 is Col2 - 1,
  NRow1 is Row1 - 1,
  NRow2 is Row2 - 1,
  board(Board),
  move_piece(Player,NRow1,NCol1,NRow2,NCol2, Board),
  retract(turnNum(N)),
  N1 is N+1,
  assert(turnNum(N1)).

choose_play(2,Player):- % place
  format('Where do you want to place the piece? ~n', []),
  get_coord(Row, Col),
  NCol is Col - 1,
  NRow is Row - 1,
  board(Board),
  place_piece(Player,NRow,NCol, Board),
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
  board(Board),
  eat_piece(Player,NRow1,NCol1,NRow2,NCol2, Row3, Col3, Board),
  board(Board1),
  multiple_eat(Player, Row3, Col3, Board1),
  retract(turnNum(N)),
  N1 is N+1,
  assert(turnNum(N1)).

choose_play(_,_):-
  format('invalid input~n', []).

%add_point(Player)
%adds points to a player whenever they capture a piece.
add_point(Player):-
  retract(player_state(Player, Points)),
  Points1 is Points +1,
  assert(player_state(Player, Points1)).


%remove_piece(Player)
%subtracts one from the number of available pieces when a player places one on the board.
remove_piece(Player):-
  retract(pieces(Player, Pieces)),
  Pieces1 is Pieces - 1,
  assert(pieces(Player, Pieces1)).

%game_state(-turnNum,-P1State,-P2State,-Board)
%gets game state.
game_state(T,P1,P2,B):-
  turnNum(T),
  player_state(1, P1),
  player_state(2, P2),
  board(B).

:- dynamic turnNum/1, player_state/2.
turnNum(1).
player_state(1, 0).
player_state(2, 0).

%board predicates

%get_position(+Board, +Row,+Col,?N)
%gets content of one of the board's cells based on coordinates.
get_position(Board, Row, Col, N):-
  size(Size),
  Row >= 0,
  Row < Size,
  Col >= 0, 
  Col < Size,
  nth0(Row, Board, Row1),
  nth0(Col, Row1, N).

%is_empty(+Row,+Col).
%checks if a cell has no pieces from any of the players.
is_empty(Row,Col, Board):-
  get_position(Board,Row,Col,0).

%line_of_three(+Board, +Player).
%checks the board for a line of three from the current player.
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
% if a line of three is detected the current player gets the 6 points needed to win the game.
line_win(Player):-
  board(B),
  line_of_three(B, Player),
  retract(player_state(Player, _)),
  assert(player_state(Player, 6)).

line_win(_).

%move predicates

%place_piece(+Player,+Row,+Col,+Board).
% updates the board with the new piece.
place_piece(Player,Row,Col, Board):-
  valid_place(Player, Row, Col, Board),
  retract(board(_)),
  place(Player, Row, Col, Board, NewBoard),
  assert(board(NewBoard)).

%place(+Player, +Row, +Col, +Board, -NewBoard).
%creates a new board with the piece in the given coordinates
place(Player, Row, Col, Board, NewBoard):-
  replace(Board,Player,Row,Col,NewBoard).

%replace(+Board, +Value, +Row, +Col, -NewBoard).
%creates a new board with the contents of the cell replaced by value.
replace(Board, Value, Row, Col, NewBoard):-
  nth0(Row, Board, Row1),
  nth0(Col, Row1, Element),
  append(P1, [Element|T], Row1),
  length(P1, Col),
  append(P1, [Value|T], Row2),
  append(P2, [Row1|Tail], Board),
  length(P2, Row),
  append(P2, [Row2|Tail], NewBoard).
  
%move_piece(+Player,+Row1,+Col1,+Row2,+Col2, +Board).
%updates the board after the moved piece.
move_piece(Player,Row1,Col1,Row2,Col2, Board):-
  valid_move(Player, Row1, Col1, Row2, Col2, Board),
  retract(board(_)),
  move(Player, Row1, Col1, Row2, Col2, Board, NewBoard),
  assert(board(NewBoard)).

%move(+Player, +Row1, +Col1, +Row2,+Col2, +Board, -NewBoard).
% creates a new board with a moved piece.
move(Player, Row1, Col1, Row2, Col2, Board, NewBoard):-
  replace(Board,0,Row1,Col1,B2),
  replace(B2,Player,Row2,Col2,NewBoard).

%eat_piece(+Player,+Row,+Col,+Row2,+Col2, -Row3, -Col3, +Bpard).
%replaces the board after a successful capture and updates the score.
eat_piece(Player, Row1,Col1,Row2,Col2, Row3, Col3, Board):-
  valid_eat(Player, Row1, Col1, Row2, Col2, Board),
  retract(board(_)),
  eat(Player, Row1, Col1, Row2, Col2, Row3, Col3, Board, NewBoard),
  assert(board(NewBoard)),
  add_point(Player).

%eat(+Player, +Row1, +Col1, +Row2, +Col2, -Row3, -Col3 , +Board, -NewBoard).
%captures a piece.
eat(Player, Row1, Col1, Row2, Col2, Row3, Col3 , Board, NewBoard):-
  get_landing(Row1, Col1, Row2, Col2, Row3, Col3),
  replace(Board,0,Row1,Col1,B2),
  replace(B2,0,Row2,Col2,B3),
  replace(B3,Player,Row3,Col3,NewBoard).

%check_eat(+Player).
%checks if the current player can capture any pieces.
check_eat(Player):-
  board(Board),
  size(Size),
  Size1 is Size-1,
  findall(X1-Y1/X2-Y2, (between(0, Size1, X1), between(0, Size1, Y1), valid_eat(Player, X1, Y1, X2, Y2, Board)), [_|_]).

%check_seconde_eat(+Player, +Row, +Col, +Board).
%after a captures, checks if the piece used to capture can capture another one.
check_second_eat(Player, X1, Y1, Board):-
  findall(X1-Y1/X2-Y2, valid_eat(Player, X1, Y1, X2, Y2, Board), [_|_]).

%multiple_eat(+Player, +Row, +Col, +Board).
%displays the board after each successful capture, until there are no possible captures available
multiple_eat(Player, Row, Col, Board):-
  check_second_eat(Player, Row, Col, Board),
  display_game,
  another_eat(Player, Row, Col, Board).
multiple_eat(_,_,_).

%another_eat(+Player, +Row, +Col, +Board).
%gets position of the target piece and captures it.
another_eat(Player, Row1, Col1, Board):-
  format('Piece to eat:~n', []),
  get_coord(Row2, Col2),
  NCol2 is Col2 - 1,
  NRow2 is Row2 - 1,
  eat_piece(Player,Row1,Col1,NRow2,NCol2, Row3, Col3, Board),
  board(Board1),
  multiple_eat(Player, Row3, Col3, Board1).
another_eat(Player, Row, Col, Board):-
  multiple_eat(Player, Row, Col, Board).

%valid_place(+Player, +Row, +Col, +Board).
%checks if a place play is valid.
valid_place(Player, Row, Col, Board):-
  pieces(Player, Pieces),
  Pieces > 0,
  is_empty(Row, Col, Board).

%valid_move(+Player, +Row, +Col, ?DestRow, ?DestCol, +Board).
%checks if a move play is valid.
valid_move(Player, Row, Col, DestRow, DestCol, Board):-
  get_position(Board, Row, Col, Player),
  adjacent(Row, Col, DestRow, DestCol),
  is_empty(DestRow, DestCol, Board).

%valid_eat(+Player, +Row, +Col, ?DestRow, ?DestCol, +Board).
%checks if a capture play is valid.
valid_eat(Player, Row, Col, DestRow, DestCol, Board):-
  adjacent(Row, Col, DestRow, DestCol),
  get_position(Board, Row, Col, Player),
  Player2 is Player mod 2 +1,
  get_position(Board, DestRow, DestCol, Player2),
  get_landing(Row, Col, DestRow, DestCol, FinalRow, FinalCol),
  is_empty(FinalRow, FinalCol, Board).


%get_landing(+Row, +Col, +DestRow, +DestCol, -FinalRow, -FinalCol).
%gets the coordinates of the cell where a piece will land after a successful capture
get_landing(Row, Col, DestRow, DestCol, FinalRow, FinalCol):-
  MoveX is DestRow - Row,
  MoveY is DestCol - Col,
  FinalRow is DestRow + MoveX,
  FinalCol is DestCol + MoveY.

  
%adjacent(+Row1, +Col1, ?Row2, ?Col2).
%verifies if two cells are adjacent.
adjacent(Row1, Col1, Row2, Col1) :- 
  Row2 is Row1+1.
adjacent(Row1, Col1, Row2, Col1) :- 
  Row2 is Row1-1.
adjacent(Row1, Col1, Row1, Col2) :- 
  Col2 is Col1+1.
adjacent(Row1, Col1, Row1, Col2) :- 
  Col2 is Col1-1.
adjacent(Row1, Col1, Row2, Col2) :- 
  Row2 is Row1+1, 
  Col2 is Col1+1.
adjacent(Row1, Col1, Row2, Col2) :- 
  Row2 is Row1-1, 
  Col2 is Col1+1.  
adjacent(Row1, Col1, Row2, Col2) :- 
  Row2 is Row1+1, 
  Col2 is Col1-1.  
adjacent(Row1, Col1, Row2, Col2) :- 
  Row2 is Row1-1, 
  Col2 is Col1-1. 

%pieces_on_board(+Player, -N, +Board).
%counts how many pieces a player has on board
pieces_on_board(Player, N, Board):-
  size(Size),
  Size1 is Size -1,
  findall(X-Y, (between(0, Size1, X), between(0, Size1, Y), get_position(Board, X, Y, Player)), L),
  sort(L, L1),
  length(L1, N).

%valid_plays(+Player, +Board, -Plays).
%gets all possible plays a player could make. (if a capture is available, it is mandatory).
valid_plays(Player, Board, Plays):-
  size(Size),
  Size1 is Size -1,
  check_eat(Player),
  findall(eat/X1-Y1/X2-Y2, (between(0, Size1, X1), between(0, Size1, Y1), valid_eat(Player, X1, Y1, X2, Y2, Board)), L),
  sort(L, Plays).

valid_plays(Player, Board, Plays):-
  size(Size),
  Size1 is Size -1,
  findall(place/X1-Y1, (between(0, Size1, X1), between(0, Size1, Y1), valid_place(Player, X1, Y1, Board)), L1),
  findall(move/X1-Y1/X2-Y2, (between(0, Size1, X1), between(0, Size1, Y1), valid_move(Player, X1, Y1, X2, Y2, Board)), L2),
  append(L1, L2, L),
  sort(L, Plays).

%winning_moves(+Player, +Board, +play, -Score).
%checks if a move will result in a win.
winning_moves(Player, Board, place/X-Y, 5000):-
  place(Player, X, Y, Board, B2),
  line_of_three(B2, Player).
winning_moves(Player, Board, move/X1-Y1/X2-Y2, 5000):-
  move(Player, X1, Y1, X2, Y2, Board, B2),
  line_of_three(B2, Player).
winning_moves(Player, Board, eat/X1-Y1/X2-Y2, 5000):-
  eat(Player, X1, Y1, X2, Y2, _, _, Board, B2),
  line_of_three(B2, Player).
winning_moves(Player, Board, eat/X1-Y1/X2-Y2, 5000):-
  player_state(Player, Points),
  potential_captures(Player, 1, Board, X1-Y1/X2-Y2, Num),
  Points1 is Num + Points,
  Points1 >= 6.

winning_moves(_, _, _, 0).


%potential_piece_difference(+Player, +Board, +Play, -Score).
%checks how big of a piece advantage/disavantage the current player will get after a specific play.
potential_piece_difference(Player, Board, eat/X1-Y1/X2-Y2, Num):-
  Player2 is Player mod 2 + 1,
  pieces_on_board(Player, N1, Board),
  pieces_on_board(Player2, N2, Board),
  potential_captures(Player, 1, Board, X1-Y1/X2-Y2, N3),
  N4 is N2 - N3,
  Num is N1 - N4.
potential_piece_difference(Player, Board, move/_-_/_-_, Num):-
  Player2 is Player mod 2 + 1,
  pieces_on_board(Player, N1, Board),
  pieces_on_board(Player2, N2, Board),
  Num is N1 - N2.
potential_piece_difference(Player, Board, place/_-_, Num):-
  Player2 is Player mod 2 + 1,
  pieces_on_board(Player, N1, Board),
  pieces_on_board(Player2, N2, Board),
  N3 is N1 + 1,
  Num is N3 - N2.

%potential_captures(+Player, +Acc, +Board, +Play, -Score).
%checks how many pieces a specific play can capture.
potential_captures(Player, Acc, Board, X1-Y1/X2-Y2, Num):-
  eat(Player, X1, Y1, X2, Y2, X3, Y3, Board, NewBoard),
  check_second_eat(Player,X3, Y3, NewBoard),
  Acc1 is Acc+1,
  valid_eat(Player, X3, Y3, X4, Y4, NewBoard),
  potential_captures(Player, Acc1, NewBoard, X3-Y3/X4-Y4, Num).
potential_captures(_, Num, _, _, Num).
  
%move_value(+Player, +Play, +Board, -Score).
%calculates the value of a movePlay.
move_value(Player, X1-Y1/X2-Y2, Board, Value):-
  Player2 is Player mod 2 +1,
  adjacent(X1, Y1, X3, Y3),
  get_position(Board, X3, Y3, Player2),
  adjacent(X2, Y2, X4, Y4),
  get_position(Board, X4, Y4, Player),
  Value is 2.
move_value(Player, X1-Y1/X2-Y2, Board, Value):-
  Player2 is Player mod 2 +1,
  adjacent(X1, Y1, X3, Y3),
  get_position(Board, X3, Y3, Player2),
  adjacent(X2, Y2, X4, Y4),
  get_position(Board, X4, Y4, Player2),
  Value is -1.
move_value(Player, X1-Y1/_-_, Board, Value):-
  Player2 is Player mod 2 +1,
  adjacent(X1, Y1, X3, Y3),
  get_position(Board, X3, Y3, Player2),
  Value is 1.
move_value(Player, X1-Y1/X2-Y2, Board, Value):-
  Player2 is Player mod 2 +1,
  adjacent(X1, Y1, X3, Y3),
  get_position(Board, X3, Y3, Player),
  adjacent(X2, Y2, X4, Y4),
  get_position(Board, X4, Y4, Player2),
  Value is -2.
move_value(Player, X1-Y1/X2-Y2, Board, Value):-
  adjacent(X1, Y1, X3, Y3),
  get_position(Board, X3, Y3, Player),
  adjacent(X2, Y2, X4, Y4),
  get_position(Board, X4, Y4, Player),
  Value is 1.
move_value(Player, X1-Y1/_-_, Board, Value):-
  adjacent(X1, Y1, X3, Y3),
  get_position(Board, X3, Y3, Player),
  Value is 0.
move_value(Player, _-_/X2-Y2, Board, Value):-
  adjacent(X2, Y2, X3, Y3),
  get_position(Board, X3, Y3, Player),
  Value is 2.
move_value(Player, _-_/X2-Y2, Board, Value):-
  Player2 is Player mod 2 +1,
  adjacent(X2, Y2, X3, Y3),
  get_position(Board, X3, Y3, Player2),
  Value is -2.
move_value(_, _, _, 0).

%place_value(+Player, +Play, +Board, -Score).
%calculates the value of a place play.
place_value(Player, X-Y, Board, Value):-
  Player2 is Player mod 2 +1,
  adjacent(X, Y, X1, Y1),
  get_position(Board, X1, Y1, Player2),
  Value is -2.
place_value(Player, X-Y, Board, Value):-
  adjacent(X, Y, X1, Y1),
  get_position(Board, X1, Y1, Player),
  Value is 2.
place_value(_, _, _, 0).

%play_value(+Player, +Play, +Board, -Score).
%calculates the score of a specific play.
play_value(Player, place/X-Y, Board, Value):-
  place_value(Player, X-Y, Board, Value1),
  winning_moves(Player, Board, place/X-Y, Value2),
  potential_piece_difference(Player, Board, place/X-Y, Value3),
  Value is Value1 + Value2 + Value3.

play_value(Player, move/X1-Y1/X2-Y2, Board, Value):-
  move_value(Player, X1-Y1/X2-Y2, Board, Value1),
  winning_moves(Player, Board, move/X1-Y1/X2-Y2, Value2),
  potential_piece_difference(Player, Board, move/X1-Y1/X2-Y2, Value3),
  Value is Value1 + Value2 + Value3.

play_value(Player, eat/X1-Y1/X2-Y2, Board, Value):-
  potential_captures(Player, 1, Board, X1-Y1/X2-Y2, Value1),
  winning_moves(Player, Board, eat/X1-Y1/X2-Y2, Value2),
  potential_piece_difference(Player, Board, eat/X1-Y1/X2-Y2, Value3),
  Value is Value1 + Value2 + Value3.


%best_play(+Player, +Plays, +Board, -BestPlay, -BestValue).
%chooses the best play based on score.
best_play(Player, [CurrPlay], Board, CurrPlay, BestValue) :- play_value(Player, CurrPlay, Board, BestValue).
best_play(Player, [CurrPlay|Tail], Board, BestPlay, BestValue):-
  play_value(Player, CurrPlay, Board, Value1),
  best_play(Player, Tail, Board, BestPlay, BestValue),
  BestValue >= Value1.
best_play(Player, [CurrPlay|_], Board, CurrPlay, BestValue) :- play_value(Player, CurrPlay, Board, BestValue).

%bot_play(+Player, +Board, +Play).
%processes plays of bot players.
bot_play(Player, Board, place/X-Y):-
  place_piece(Player, X, Y, Board).

bot_play(Player, Board, eat/X1-Y1/X2-Y2):-
  eat_piece(Player, X1, Y1, X2, Y2, X3, Y3, Board),
  board(NewBoard),
  check_second_eat(Player, X3, Y3, NewBoard),
  valid_eat(Player, X3, Y3, X4, Y4, NewBoard),
  clear,
  display_game,
  sleep(3),
  bot_play(Player, NewBoard, eat/X3-Y3/X4-Y4).
bot_play(_, _, eat/_-_/_-_).

bot_play(Player, Board, move/X1-Y1/X2-Y2):-
  move_piece(Player, X1, Y1, X2, Y2, Board).
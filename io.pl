encoding(utf8).

%% menu I/O

%welcome_message.

welcome_message:-
    format('Welcome to Moxie!~nLet\'s play!~n', []).

:- dynamic gamemode/1, size/1, board/1, pieces/2, pos/1.

gamemode(h/h).    %default gamemode is human vs human
size(4).          %default size is 4x4
board([
  [0,1,0,0],
  [0,1,0,0],
  [0,2,0,0],
  [0,0,0,0]]).     % default board.

%pieces(+Player, ?Pieces_left).
pieces(1, 8).
pieces(2, 8).


pos(0).
pos(1).
pos(2).
pos(3).

%clear.
clear :- 
    write('\e[2J').

%valid_gamemode(+Gamemode)

valid_gamemode(P1/P2):-
    ground(P1), 
    ground(P2),
    valid_player_type(P1), 
    valid_player_type(P2).


% valid_player_type(+Type)

valid_player_type(h).
valid_player_type(pc-1).
valid_player_type(pc-2).

%get_gamemode.

get_gamemode:-
    format('What game mode would you like to play?~nWrite it in the form "P1/P2", where either can be h or "pc-[1/2]": ', []),
    read_line(Gamemode),
    char_code('/', Bar),
    append(Codes1, [Bar|Codes2], Gamemode),
    atom_codes(P1, Codes1),
    atom_codes(P2, Codes2),
    valid_gamemode(P1/P2),
    retract(gamemode(_)),
    assert(gamemode(P1/P2)),
    !.
get_gamemode:-
    error_message.


%get_boardsize.

get_boardsize:-
    format('Board size? Recommended: 4 / 5~n', []),
    read_line(Codes),
    catch(number_codes(Size, Codes), _, fail),
    Size > 0,
    retract(size(_)),
    assert(size(Size)),
    !.
get_boardsize :- error_message.

%error_message.

error_message:-
    format('It seems an error occurred. Returning to main menu...~n', []).

congratulate(Winner):-
    format('Player ~d Won!~nCongratulations!!!~n~n Press Enter to return to menu.', [Winner]),
    read_line(_).



%% display board

% side_chars(?Line, ?[LeftCorner, Mid, Rightcorner]).

side_chars(first, ['\x250F\', '\x2533\', '\x2513\']).
side_chars(last, ['\x2517\', '\x253B\', '\x2518\']).

%display_char(?piece, ?char).
% 0 -> empty, 1 -> player1(cross), 2-> player2(circle).

display_char(0, ' ').
display_char(1, 'X').
display_char(2, 'O').

%display_game(+Gamestate).

display_game:-
    size(Size),
    board(Board),
    display_edge_line(Size, first),
    display_lines(Board),
    display_edge_line(Size, last).

%display_edge_line(+Size, +Side).

display_edge_line(Size, Side):-
    side_chars(Side, [LeftCorner, Mid, RightCorner]),
    display_intermediate_line(Size, LeftCorner, Mid, RightCorner), nl.

%display_lines(+Board).

display_lines([]).
display_lines([BoardLine]):- 
    write('\x2503\'), 
    display_checker_line(BoardLine), nl, !.
display_lines([BoardLine|T]):- 
    write('\x2503\'), 
    display_checker_line(BoardLine), nl,
    size(Size),
    display_intermediate_line(Size, '\x2503\', '\x2503\', '\x2503\'), nl,
    display_lines(T).

%display_checker_line(+BoardRow).
display_checker_line([]).
display_checker_line([Checker|T]):-
    display_char(Checker, Char),
    format(' ~a ', [Char]),
    write('\x2503\'), 
    display_checker_line(T).

%display_intermediate_line(+Size, +LeftEdge, +Mid, +RightEdge).

display_intermediate_line(Size, LeftEdge, Mid, RightEdge):-
    write(LeftEdge),
    write('\x2501\\x2501\\x2501\'),
    Size1 is Size -1, 
    display_box_edges(Size1, Mid, '\x2501\\x2501\\x2501\'),
    write(RightEdge).

%display_box_edges(+N, +Mid, +Edge).

display_box_edges(0,_,_).
display_box_edges(N, Mid, Edge):-
    N > 0, 
    N1 is N-1,
    write(Mid),
    write(Edge),
    display_box_edges(N1, Mid, Edge).







%%%-------------------------------------------------------------------
%%% @author hunto
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2025 4:30 PM
%%%-------------------------------------------------------------------
-module(connect4).
-author("hunto").

%% API
-export([
  all_same/1,
  check_winner/4,
  check_winner_diagonal/2,
  check_winner_diagonal_left/4,
  check_winner_diagonal_right/4,
  check_winner_helper/3,
  check_winner_horizontal/4,
  check_winner_vertical/4,
  create_matrix/3,
  diagonal_edge_reference/0,
  element_to_string/1,
  get/3,
  getColumn/3,
  getColumn_helper/2,
  get_Diagonal_left/4,
  get_Diagonal_right/4,
  game_config/0,
  move/5,
  move_helper/5,
  play/1,
  print_matrix/1,
  reverse_row_order_in_matrix/1,
  skip_and_take/3,
  start/0
]).

game_config() -> {7, 6, 4}. % Cols, Rows, Connect
diagonal_edge_reference() ->
  {Cols, Rows, Connect} = game_config(),
  {Cols - Connect + 1, Rows - Connect + 1}
.

start() ->

  {Cols, Rows, _} = game_config(),
  Matrix = create_matrix(Cols, Rows, empty),
  play(Matrix)
.

play(Matrix) ->
  {Cols, Rows, Connect} = game_config(),
  case check_winner(Cols, Rows, Connect, Matrix) of
    none ->
      io:format("Current Matrix:~n"),
      print_matrix(Matrix),
      io:format("Enter column (1-7) to move as 'x' (or type 'exit'): "),
      Input = string:trim(io:get_line("> ")),
      case Input of
        "exit" ->
          io:format("Exiting game.~n"),
          ok;
        _ ->
          case catch list_to_integer(Input) of
            Col when is_integer(Col), Col >= 1, Col =< 7 ->
              case move(Matrix, Col, [], x, notMoved) of
                {notMoved, _} ->
                  io:format("Column full. Try a different one.~n"),
                  play(Matrix);
                {moved, NewMatrix} ->
                  Player_turn_matrix = ai_move(NewMatrix),
                  play(Player_turn_matrix)
              end;
            _ ->
              io:format("Invalid input. Please enter a number from 1 to 7.~n"),
              play(Matrix)
          end
      end;
    Winner ->
      print_matrix(Matrix),
      io:format("Winner is: ~p~n", [Winner]),
      ok
  end.


print_matrix(Matrix) ->
  lists:foreach(fun(Row) ->
    lists:foreach(fun(Elem) ->
      io:format("~s ", [element_to_string(Elem)])
                  end, Row),
    io:format("~n", [])
                end, reverse_row_order_in_matrix(Matrix)).

element_to_string(Elem) when Elem == empty -> ".";
element_to_string(Elem) -> atom_to_list(Elem).

reverse_row_order_in_matrix([]) -> [];
reverse_row_order_in_matrix([H | T]) ->
  reverse_row_order_in_matrix(T) ++ [H].

move([], _, Acc, _, IsMoved) -> {IsMoved, lists:reverse(Acc)};
move([Row | Rest], ColumnIndex, Acc, Player, notMoved) ->
  {IsMov, NewRow} = move_helper(Row, ColumnIndex, [], Player, notMoved),
  move(Rest, ColumnIndex, [NewRow | Acc], Player, IsMov);
move([Row | Rest], ColumnIndex, Acc, Player, moved) ->
  move(Rest, ColumnIndex, [Row | Acc], Player, moved).


move_helper([], _, Acc, _, IsMoved) -> {IsMoved, lists:reverse(Acc)};
move_helper([empty | Rest], 1, Acc, Player, notMoved) ->
  {moved, lists:reverse(Acc) ++ [Player | Rest]};
move_helper([H | T], ColIndex, Acc, Player, IsMoved) ->
  move_helper(T, ColIndex - 1, [H | Acc], Player, IsMoved).


skip_and_take(_,_,[]) -> [];
skip_and_take(Skip, Take, [ _ | T]) when Skip > 0 , Take > 0 ->
  skip_and_take(Skip - 1 , Take, T);
skip_and_take(Skip, Take, [H | T]) when Skip == 0 , Take > 0 ->
  [H | skip_and_take(Skip , Take - 1, T)];
skip_and_take(Skip, Take, _) when Skip == 0 , Take == 0 -> [];
skip_and_take(_,_,_) -> [].

all_same([]) -> false;
all_same([X]) when X =/= empty -> X;
all_same([X]) when X == empty -> false;
all_same([X,X | T]) -> all_same([X | T]);
all_same([_,_ | _]) -> false.

create_matrix(Cols, Rows, Value) ->
  Row = lists:duplicate(Cols, Value),
  lists:duplicate(Rows, Row)
.

get(RowIndex, ColIndex, Matrix) ->
  Row = lists:nth(RowIndex, Matrix),
  lists:nth(ColIndex, Row)
.


getColumn(0, _, _) -> [];
getColumn(Number_of_row, ColIndex, Matrix) ->
  Row = lists:nth(Number_of_row, Matrix),
  [getColumn_helper(ColIndex, Row) | getColumn(Number_of_row - 1, ColIndex, Matrix)].

getColumn_helper(1, [H | _]) -> H;
getColumn_helper(N, [_ | T]) -> getColumn_helper(N - 1, T).



check_winner_helper(Index, _, _) when Index < 0 -> none;
check_winner_helper(Index, Connect, Row) ->
  case all_same(skip_and_take(Index, Connect, Row)) of
    false -> check_winner_helper(Index - 1, Connect, Row);
    W -> W
  end.

check_winner_vertical(0, _, _, _) -> none;
check_winner_vertical(ColIndex, Rows, Connect, Matrix) ->
  Column = getColumn(Rows, ColIndex, Matrix),
  MaxIndex = Rows - Connect,
  case check_winner_helper(MaxIndex, Connect, Column) of
    none ->
      check_winner_vertical(ColIndex - 1, Rows, Connect, Matrix);
    W -> W
  end.

check_winner_horizontal(_, 0, _, _) -> none;
check_winner_horizontal(Cols, RowIndex, Connect, Matrix) ->
  Row = lists:nth(RowIndex, Matrix),
  MaxIndex = Cols - Connect,
  case check_winner_helper(MaxIndex, Connect, Row) of
    none ->
      check_winner_horizontal(Cols, RowIndex - 1, Connect, Matrix);
    W -> W
  end.

check_winner(Cols, Rows, Connect, Matrix) ->

  HResult = check_winner_horizontal(Cols, Rows, Connect, Matrix),
  VResult = check_winner_vertical(Cols, Rows, Connect, Matrix),
  DResult = check_winner_diagonal(Connect, Matrix),

  case {HResult, VResult, DResult} of
    {none, none, none} -> none;
    {W, none, none} -> W;
    {none, W, none} -> W;
    {none, none, W} -> W;
    {W, _, _} when W =/= none -> W;
    {_, W, _} when W =/= none -> W;
    {_, _, W} -> W
  end.


get_Diagonal_left(_, _, 0  ,_) -> []; % from top left to bottom right (in matrix)
get_Diagonal_left( ColIndex, RowIndex, ConnectIndex ,Matrix) ->
  [get(  RowIndex, ColIndex,Matrix) | get_Diagonal_left( ColIndex + 1, RowIndex + 1,ConnectIndex - 1,Matrix)  ]
.

get_Diagonal_right(_, _, 0 , _) -> []; % from top right to bottom left (in matrix)
get_Diagonal_right( ColIndex, RowIndex, ConnectIndex ,Matrix) ->
  [get(  RowIndex,ColIndex,Matrix) | get_Diagonal_right( ColIndex - 1, RowIndex + 1, ConnectIndex - 1,Matrix)  ]
.

check_winner_diagonal_left(0, 1 , _, _) -> none;

check_winner_diagonal_left(0, RowIndex , Connect, Matrix) when RowIndex > 1->
  {C, _} = diagonal_edge_reference(),
  check_winner_diagonal_left( C, RowIndex - 1, Connect, Matrix)
;
check_winner_diagonal_left( ColIndex, RowIndex, Connect, Matrix) ->
  Diagonal = get_Diagonal_left( ColIndex,RowIndex, Connect, Matrix),
  case check_winner_helper(0, Connect, Diagonal) of
    none ->
      check_winner_diagonal_left(ColIndex - 1,RowIndex ,  Connect, Matrix);
    W -> W
  end.

check_winner_diagonal_right(8, 1 , _, _) -> none;

check_winner_diagonal_right(8, RowIndex , Connect, Matrix) when RowIndex > 1->
  {C, _} = diagonal_edge_reference(),
  check_winner_diagonal_right( C, RowIndex - 1, Connect, Matrix)
;
check_winner_diagonal_right( ColIndex, RowIndex, Connect, Matrix) ->
  Diagonal = get_Diagonal_right( ColIndex,RowIndex, Connect, Matrix),
  case check_winner_helper(0, Connect, Diagonal) of
    none ->
      check_winner_diagonal_right(ColIndex + 1 ,RowIndex ,  Connect, Matrix);
    W -> W
  end.

check_winner_diagonal(Connect, Matrix) ->
  {ColIndex, RowIndex} = diagonal_edge_reference(),
  Right_result = check_winner_diagonal_right(ColIndex, RowIndex, Connect, Matrix),
  Left_result = check_winner_diagonal_left(ColIndex, RowIndex, Connect, Matrix),
  case {Right_result, Left_result} of
    {none, none} -> none;
    {Winner, none} -> Winner;
    {none, Winner} -> Winner;
    {W1, W2} when W1 == W2 -> W1;
    {W1, _} -> W1  % Inconsistent results — return first
  end.


move_score(Window, Player) ->
  Opponent = case Player of
               x -> o;
               o -> x
             end,
  CountPlayer = count_occurrences(Player, Window),
  CountOpponent = count_occurrences(Opponent, Window),
  CountEmpty = count_occurrences(empty, Window),
  case {CountPlayer, CountOpponent, CountEmpty} of
    {4, _, _} -> 6500;
    {3, 0, 1} -> 55;
    {2, 0, 2} -> 10;
    {1, 0, 3} -> 1;
    {1, 3, 0} -> 1000;
    {0, 3, 1} -> -1500;
    {0, 4, 0} -> -7000;
    {1, 2, 1} -> 25;
    _ -> 0
  end.

count_occurrences(_, []) -> 0;
count_occurrences(X, [X | T]) -> 1 + count_occurrences(X, T);
count_occurrences(X, [_ | T]) -> count_occurrences(X, T).


calc_diagonal_right(8, 1 , _, _, _) -> 0;
calc_diagonal_right(8, RowIndex , Connect, Matrix , Player) when RowIndex > 1->
  {C, _} = diagonal_edge_reference(),
  calc_diagonal_right( C, RowIndex - 1, Connect, Matrix, Player)
;
calc_diagonal_right( ColIndex, RowIndex, Connect, Matrix , Player) ->
  Diagonal = get_Diagonal_right( ColIndex,RowIndex, Connect, Matrix),
  Points = move_score(Diagonal, Player) ,
  Points + calc_diagonal_right(ColIndex + 1 ,RowIndex ,  Connect, Matrix, Player)
.


calc_diagonal_left(0, 1 , _, _, _) -> 0;
calc_diagonal_left(0, RowIndex , Connect, Matrix , Player) when RowIndex > 1->
  {C, _} = diagonal_edge_reference(),
  calc_diagonal_left( C, RowIndex - 1, Connect, Matrix, Player)
;
calc_diagonal_left( ColIndex, RowIndex, Connect, Matrix , Player) ->
  Diagonal = get_Diagonal_left( ColIndex,RowIndex, Connect, Matrix),
  Points = move_score(Diagonal, Player) ,
  Points + calc_diagonal_left(ColIndex - 1,RowIndex ,  Connect, Matrix, Player)
.

calc_vertical(0, _, _, _, _) -> 0;
calc_vertical(ColIndex, Rows, Connect, Matrix , Player) ->
  Column = getColumn(Rows, ColIndex, Matrix),
  MaxIndex = Rows - Connect,
  calc_helper(MaxIndex, Connect, Column, Player) + calc_vertical(ColIndex - 1, Rows, Connect, Matrix , Player)
.

calc_horizontal(_, 0, _, _, _) -> 0;
calc_horizontal(Cols, RowIndex, Connect, Matrix , Player) ->
  Row = lists:nth(RowIndex, Matrix),
  MaxIndex = Cols - Connect,
  calc_helper(MaxIndex, Connect, Row, Player) + calc_horizontal(Cols, RowIndex - 1, Connect, Matrix , Player)
.

calc_helper(Index, _, _, _) when Index < 0 -> 0;
calc_helper(Index, Connect, Row, Player) ->
   move_score(skip_and_take(Index, Connect, Row), Player) + calc_helper(Index - 1, Connect, Row, Player)
.

ai_move(Matrix) ->
  Player = o,
  {ColIndex, RowIndex, Connect} = game_config(),
  {ColDiagonalEdge, RowDiagonalEdge} = diagonal_edge_reference(),

  All_move_score_list = lists:reverse(all_move_score(ColIndex, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player)),
  %Horizontal_extra_score = [0,3,5,10,5,3,0],
  Horizontal_extra_score = [0,-3,-5,-10,-5,-3,0],
  Heuristics_score_list = sum_lists(All_move_score_list,Horizontal_extra_score),
  Min_value = min_list(Heuristics_score_list),

  Ai_move_col_index = find_index_by_value(Min_value, Heuristics_score_list),
  io:format("List: ~p~n", [Heuristics_score_list]),
  {_, NewMatrix} = move(Matrix, Ai_move_col_index, [], Player, notMoved),
  NewMatrix.

all_move_score(0,_, _, _, _, _ , _) -> [];
all_move_score(ColIndex, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player) ->
  Opponent = case Player of
               x -> o;
               o -> x
             end,

  [
    case move(Matrix, ColIndex, [], Player, notMoved) of
      {notMoved, _} ->
        100000;
      {moved, NewMatrix } ->
        {C, _, _} = game_config(),
        max_list(all_move_score_enemy(C, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, NewMatrix , Opponent))
    end
| all_move_score(ColIndex - 1, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player) ].


all_move_score_enemy(0,_, _, _, _, _ , _) -> [];
all_move_score_enemy(ColIndex, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , EnemyPlayer) ->
  [
    case move(Matrix, ColIndex, [], EnemyPlayer, notMoved) of
      {notMoved, _} ->
        -100000;
      {moved, NewMatrix } ->
        {C, _, _} = game_config(),
        calc_move_score(C, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, NewMatrix , EnemyPlayer)
    end
    | all_move_score_enemy(ColIndex - 1, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , EnemyPlayer) ].




calc_move_score(ColIndex, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player) ->
  Sum_diagonal = calc_diagonal_left( ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player) + calc_diagonal_right( ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player),
  Sum_vertical_horizontal = calc_vertical(ColIndex, RowIndex, Connect, Matrix , Player) + calc_horizontal(ColIndex, RowIndex, Connect, Matrix , Player),
  Sum_score = Sum_diagonal + Sum_vertical_horizontal,
  Sum_score
.


max_list([F]) -> F;
max_list([F, S | R]) when F > S -> max_list([F | R]);
max_list([F, S | R]) when F =< S -> max_list([S | R])
.

min_list([F]) -> F;
min_list([F, S | R]) when F > S -> min_list([S | R]);
min_list([F, S | R]) when F =< S -> min_list([F | R])
.

find_index_by_value(X, L) ->
  find_index_by_value(X, L, 1)
.

find_index_by_value(X, [F | R], Index) when X =/= F-> find_index_by_value(X,  R, Index + 1);
find_index_by_value(X, [X | _], Index) -> Index;
find_index_by_value(_, _, _) -> -1
.



add_lists([], [], Acc) -> lists:reverse(Acc);
add_lists([H1|T1], [H2|T2], Acc) ->
  add_lists(T1, T2, [H1 + H2 | Acc]).

sum_lists(L1, L2) -> add_lists(L1, L2, []).

-module(ai).
-author("hunto").

%% API functions exported
-export([
  ai_move/1,
  all_move_score/7,
  all_move_score_enemy/7,
  calc_diagonal_right/5,
  calc_diagonal_left/5,
  calc_vertical/5,
  calc_horizontal/5,
  calc_helper/4,
  calc_move_score/7,
  move_score/2
]).

%%------------------------------------------------------------------------------
%% ai_move/1
%% Entry point for AI to decide and perform a move given the current game board.
%% Medium difficulty
%% Params:
%%  - Matrix: current game board state (list of lists)
%% Returns:
%%  - NewMatrix: updated game board state after AI move
%%------------------------------------------------------------------------------
ai_move(Matrix) ->
  Player = o, %% AI player symbol
  {ColIndex, RowIndex, Connect} = board:game_config(), %% Get board config
  {ColDiagonalEdge, RowDiagonalEdge} = board:diagonal_matrix(), %% Diagonal limits
  %% Calculate move scores for all columns (reversed for indexing convenience)
  All_move_score_list = lists:reverse(all_move_score(ColIndex, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player)),
  %% Additional heuristic scores (penalizing middle/edges for example)
  Horizontal_extra_score = [0,-3,-5,-10,-5,-3,0],
  %% Sum base scores and heuristic adjustments
  Heuristics_score_list = logic:sum_lists(All_move_score_list,Horizontal_extra_score),
  %% Find the minimum score (best move)
  Min_value = logic:min_list(Heuristics_score_list),
  Ai_move_col_index = logic:find_index_by_value(Min_value, Heuristics_score_list),
  %% Apply the best move and return the new game board state
  {_, NewMatrix} = logic:move(Matrix, Ai_move_col_index, [], Player, notMoved),
  NewMatrix
.

%%------------------------------------------------------------------------------
%% all_move_score/7
%% Recursively calculates scores for all possible moves for the AI player.
%% Scores moves by simulating and evaluating opponent's best response.
%% Params:
%%  - ColIndex: current column index (integer)
%%  - RowIndex: number of rows (integer)
%%  - ColDiagonalEdge: column limit for diagonal checks (integer)
%%  - RowDiagonalEdge: row limit for diagonal checks (integer)
%%  - Connect: number of pieces needed to win (integer)
%%  - Matrix: current game board state (list of lists)
%%  - Player: AI player symbol (atom)
%% Returns:
%%  - List of move scores for each column (list of integers)
%%------------------------------------------------------------------------------
all_move_score(0,_, _, _, _, _ , _) -> [];
all_move_score(ColIndex, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player) ->
  Opponent = case Player of
               x -> o;
               o -> x
             end,
  [
    case logic:move(Matrix, ColIndex, [], Player, notMoved) of
      {notMoved, _} ->
        100000; %% Very high cost if no move possible in this column
      {moved, NewMatrix } ->
        {C, _, _} = board:game_config(),
        %% Score this move from opponent perspective
        logic:max_list(all_move_score_enemy(C, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, NewMatrix , Opponent))
    end
    | all_move_score(ColIndex - 1, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player) ]
.

%%------------------------------------------------------------------------------
%% all_move_score_enemy/7
%% Recursively calculates scores for all possible opponent moves to evaluate threats.
%% Used to estimate opponent's best move after AI move.
%% Params:
%%  - ColIndex: current column index (integer)
%%  - RowIndex: number of rows (integer)
%%  - ColDiagonalEdge: column limit for diagonal checks (integer)
%%  - RowDiagonalEdge: row limit for diagonal checks (integer)
%%  - Connect: number of pieces needed to win (integer)
%%  - Matrix: current game board state after AI move (list of lists)
%%  - EnemyPlayer: opponent player symbol (atom)
%% Returns:
%%  - List of threat scores for each column (list of integers)
%%------------------------------------------------------------------------------
all_move_score_enemy(0,_, _, _, _, _ , _) -> [];
all_move_score_enemy(ColIndex, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , EnemyPlayer) ->
  [
    case logic:move(Matrix, ColIndex, [], EnemyPlayer, notMoved) of
      {notMoved, _} ->
        -100000; %% Very low (bad) score if enemy cannot move here
      {moved, NewMatrix } ->
        {C, _, _} = board:game_config(),
        calc_move_score(C, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, NewMatrix , EnemyPlayer)
    end
    | all_move_score_enemy(ColIndex - 1, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , EnemyPlayer) ]
.

%%------------------------------------------------------------------------------
%% calc_diagonal_right/5
%% Recursively sums heuristic scores of all diagonals running top-left to bottom-right.
%% Params:
%%  - ColIndex: starting column index (integer)
%%  - RowIndex: starting row index (integer)
%%  - Connect: number of pieces to connect for win (integer)
%%  - Matrix: current game board state (list of lists)
%%  - Player: player symbol to score for (atom)
%% Returns:
%%  - Sum of scores for all such diagonals (integer)
%%------------------------------------------------------------------------------
calc_diagonal_right(8, 1 , _, _, _) -> 0; %% Stop at 8 (out of index)
calc_diagonal_right(8, RowIndex , Connect, Matrix , Player) when RowIndex > 1 ->
  {C, _} = board:diagonal_matrix(),
  calc_diagonal_right(C, RowIndex - 1, Connect, Matrix, Player)
;
calc_diagonal_right(ColIndex, RowIndex, Connect, Matrix , Player) ->
  Diagonal = board:get_Diagonal_right(ColIndex,RowIndex, Connect, Matrix),
  Points = move_score(Diagonal, Player),
  Points + calc_diagonal_right(ColIndex + 1, RowIndex, Connect, Matrix, Player)
.

%%------------------------------------------------------------------------------
%% calc_diagonal_left/5
%% Recursively sums heuristic scores of all diagonals running top-right to bottom-left.
%% Params:
%%  - ColIndex: starting column index (integer)
%%  - RowIndex: starting row index (integer)
%%  - Connect: number of pieces to connect for win (integer)
%%  - Matrix: current game board state (list of lists)
%%  - Player: player symbol to score for (atom)
%% Returns:
%%  - Sum of scores for all such diagonals (integer)
%%------------------------------------------------------------------------------
calc_diagonal_left(0, 1 , _, _, _) -> 0; %% Stop at 0 (out of index)
calc_diagonal_left(0, RowIndex , Connect, Matrix , Player) when RowIndex > 1 ->
  {C, _} = board:diagonal_matrix(),
  calc_diagonal_left(C, RowIndex - 1, Connect, Matrix, Player);
calc_diagonal_left(ColIndex, RowIndex, Connect, Matrix , Player) ->
  Diagonal = board:get_Diagonal_left(ColIndex,RowIndex, Connect, Matrix),
  Points = move_score(Diagonal, Player),
  Points + calc_diagonal_left(ColIndex - 1, RowIndex, Connect, Matrix, Player)
.

%%------------------------------------------------------------------------------
%% calc_vertical/5
%% Calculates vertical scores by summing heuristic scores on all columns.
%% Params:
%%  - ColIndex: current column index (integer)
%%  - Rows: number of rows (integer)
%%  - Connect: number of pieces to connect for win (integer)
%%  - Matrix: current game board state (list of lists)
%%  - Player: player symbol to score for (atom)
%% Returns:
%%  - Total vertical score (integer)
%%------------------------------------------------------------------------------
calc_vertical(0, _, _, _, _) -> 0; %% Base case: stop recursion
calc_vertical(ColIndex, Rows, Connect, Matrix , Player) ->
  Column = board:getColumn(Rows, ColIndex, Matrix),
  MaxIndex = Rows - Connect,
  calc_helper(MaxIndex, Connect, Column, Player) + calc_vertical(ColIndex - 1, Rows, Connect, Matrix, Player)
.

%%------------------------------------------------------------------------------
%% calc_horizontal/5
%% Calculates horizontal scores by summing heuristic scores on all rows.
%% Params:
%%  - Cols: number of columns (integer)
%%  - RowIndex: current row index (integer)
%%  - Connect: number of pieces to connect for win (integer)
%%  - Matrix: current game board state (list of lists)
%%  - Player: player symbol to score for (atom)
%% Returns:
%%  - Total horizontal score (integer)
%%------------------------------------------------------------------------------
calc_horizontal(_, 0, _, _, _) -> 0; %% Base case: stop recursion
calc_horizontal(Cols, RowIndex, Connect, Matrix , Player) ->
  Row = lists:nth(RowIndex, Matrix),
  MaxIndex = Cols - Connect,
  calc_helper(MaxIndex, Connect, Row, Player) + calc_horizontal(Cols, RowIndex - 1, Connect, Matrix, Player)
.

%%------------------------------------------------------------------------------
%% calc_helper/4
%% Helper to calculate score over all contiguous segments of length Connect
%% within a row or column.
%% Params:
%%  - Index: current starting index in the sequence (integer)
%%  - Connect: number of pieces to connect for win (integer)
%%  - Row: list representing a row or column segment
%%  - Player: player symbol to score for (atom)
%% Returns:
%%  - Sum of scores for all segments starting from Index down to 0 (integer)

%%------------------------------------------------------------------------------
calc_helper(Index, _, _, _) when Index < 0 -> 0; %% Base case: stop recursion
calc_helper(Index, Connect, Row, Player) ->
  move_score(logic:skip_and_take(Index, Connect, Row), Player) + calc_helper(Index - 1, Connect, Row, Player)
.

%%------------------------------------------------------------------------------
%% calc_move_score/7
%% Aggregates scores for all directions (diagonal left, diagonal right,
%% vertical, horizontal) to evaluate board state for a player.
%% Params:
%%  - ColIndex: number of columns (integer)
%%  - RowIndex: number of rows (integer)
%%  - ColDiagonalEdge: column limit for diagonal checks (integer)
%%  - RowDiagonalEdge: row limit for diagonal checks (integer)
%%  - Connect: number of pieces to connect for win (integer)
%%  - Matrix: current game board state (list of lists)
%%  - Player: player symbol to score for (atom)
%% Returns:
%%  - Aggregate heuristic score (integer)
%%------------------------------------------------------------------------------
calc_move_score(ColIndex, RowIndex, ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix , Player) ->
  Sum_diagonal = calc_diagonal_left(ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix, Player) +
    calc_diagonal_right(ColDiagonalEdge, RowDiagonalEdge, Connect, Matrix, Player),
  Sum_vertical_horizontal = calc_vertical(ColIndex, RowIndex, Connect, Matrix, Player) +
    calc_horizontal(ColIndex, RowIndex, Connect, Matrix, Player),
  Sum_diagonal + Sum_vertical_horizontal
.

%%------------------------------------------------------------------------------
%% move_score/2
%% Assigns heuristic score to a given window of pieces based on counts of
%% player pieces, opponent pieces, and empty slots.
%% Params:
%%  - Window: list of elements representing a segment (length Connect)
%%  - Player: player symbol to score for (atom)
%% Returns:
%%  - Integer score reflecting favorability of the window for Player
%%------------------------------------------------------------------------------
move_score(Window, Player) ->
  Opponent = case Player of
               x -> o;
               o -> x
             end,
  CountPlayer = logic:count_occurrences(Player, Window),
  CountOpponent = logic:count_occurrences(Opponent, Window),
  CountEmpty = logic:count_occurrences(empty, Window),
  case {CountPlayer, CountOpponent, CountEmpty} of
    {4, _, _} -> 6500;     %% Winning move
    {3, 0, 1} -> 55;       %% Three in a row + one empty
    {2, 0, 2} -> 10;
    {1, 0, 3} -> 1;
    {1, 2, 1} -> 25;
    {1, 3, 0} -> 1000;     %% Block opponent's three-in-a-row
    {0, 3, 1} -> -1500;    %% Opponent threat
    {0, 4, 0} -> -7000;    %% Opponent wins
    _ -> 0                 %% Default neutral score
  end
.

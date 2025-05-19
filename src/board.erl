-module(board).
-author("hunto").

%% API
-export([
  game_config/0,
  diagonal_matrix/0,
  create_matrix/3,
  print_matrix/1,
  get/3,
  getColumn/3,
  getColumn_helper/2,
  get_Diagonal_left/4,
  get_Diagonal_right/4,
  check_winner_diagonal_left/4,
  check_winner_diagonal_right/4,
  check_winner_diagonal/2,
  check_winner_helper/3,
  check_winner_vertical/4,
  check_winner_horizontal/4,
  check_winner/4
]).
%%------------------------------------------------------------------------------
%% game_config/0
%% Returns the game configuration tuple.
%% Params:
%%  - none
%% Returns:
%%  - {Cols, Rows, Connect}: tuple where
%%     Cols = number of columns
%%     Rows = number of rows
%%     Connect = number of consecutive pieces needed to win
%%------------------------------------------------------------------------------
game_config() -> {7, 6, 4}.

%%------------------------------------------------------------------------------
%% diagonal_matrix/0
%% Calculates the starting reference points for diagonal winner checks.
%% Params:
%%  - none
%% Returns:
%%  - {StartCol, StartRow}: tuple where
%%      StartCol = Cols - Connect + 1
%%      StartRow = Rows - Connect + 1
%%------------------------------------------------------------------------------
diagonal_matrix() ->
  {Cols, Rows, Connect} = game_config(),
  {Cols - Connect + 1, Rows - Connect + 1}.

%%------------------------------------------------------------------------------
%% create_matrix/3
%% Creates a matrix filled with a specified initial value.
%% Params:
%%  - Cols: number of columns (integer)
%%  - Rows: number of rows (integer)
%%  - Value: initial value to fill each cell
%% Returns:
%%  - Matrix: list of lists representing the matrix
%%------------------------------------------------------------------------------
create_matrix(Cols, Rows, Value) ->
  Row = lists:duplicate(Cols, Value),
  lists:duplicate(Rows, Row).

%%------------------------------------------------------------------------------
%% print_matrix/1
%% Prints the matrix row by row, converting elements to string with logic:element_to_string/1.
%% Params:
%%  - Matrix: list of lists representing the matrix
%% Returns:
%%  - none (prints output to console)
%%------------------------------------------------------------------------------
print_matrix(Matrix) ->
  lists:foreach(fun(Row) ->
    lists:foreach(fun(Elem) ->
      io:format("~s ", [logic:element_to_string(Elem)])
                  end, Row),
    io:format("~n", [])
                end, logic:reverse_row_order_in_matrix(Matrix)).

%%------------------------------------------------------------------------------
%% get/3
%% Retrieves the element at given row and column indices (1-based) in the matrix.
%% Params:
%%  - RowIndex: integer (1-based row index)
%%  - ColIndex: integer (1-based column index)
%%  - Matrix: list of lists representing the matrix
%% Returns:
%%  - Element at (RowIndex, ColIndex)
%%------------------------------------------------------------------------------
get(RowIndex, ColIndex, Matrix) ->
  Row = lists:nth(RowIndex, Matrix),
  lists:nth(ColIndex, Row).

%%------------------------------------------------------------------------------
%% getColumn/3
%% Extracts the entire column at ColIndex from bottom row to top row as a list.
%% Params:
%%  - Number_of_row: integer, current row number to process (starts at Rows and decrements)
%%  - ColIndex: integer (1-based column index)
%%  - Matrix: list of lists representing the matrix
%% Returns:
%%  - List of elements in the column from bottom to top
%%------------------------------------------------------------------------------
getColumn(0, _, _) -> [];
getColumn(Number_of_row, ColIndex, Matrix) ->
  Row = lists:nth(Number_of_row, Matrix),
  [getColumn_helper(ColIndex, Row) | getColumn(Number_of_row - 1, ColIndex, Matrix)].

%%------------------------------------------------------------------------------
%% getColumn_helper/2
%% Retrieves the Nth element (1-based) from a list.
%% Params:
%%  - N: integer (1-based index)
%%  - List: input list
%% Returns:
%%  - Element at position N in List
%%------------------------------------------------------------------------------
getColumn_helper(1, [H | _]) -> H;
getColumn_helper(N, [_ | T]) -> getColumn_helper(N - 1, T).

%%------------------------------------------------------------------------------
%% get_Diagonal_left/4
%% Extracts a diagonal list of length from the diagonal matrix
%% moving down-right (top-left to bottom-right).
%% Params:
%%  - Col_diagonal_matrix: integer (1-based column index)
%%  - Row_diagonal_matrix: integer (1-based row index)
%%  - ConnectIndex: integer (remaining length of diagonal to extract)
%%  - Matrix: list of lists representing the matrix
%% Returns:
%%  - List of elements forming the diagonal
%%------------------------------------------------------------------------------
get_Diagonal_left(_, _, 0, _) -> [];
get_Diagonal_left(Col_diagonal_matrix, Row_diagonal_matrix, ConnectIndex, Matrix) ->
  [get(Row_diagonal_matrix, Col_diagonal_matrix, Matrix) | get_Diagonal_left(Col_diagonal_matrix + 1, Row_diagonal_matrix + 1, ConnectIndex - 1, Matrix)].

%%------------------------------------------------------------------------------
%% get_Diagonal_right/4
%% Extracts a diagonal list of length from the diagonal matrix
%% moving down-left (top-right to bottom-left).
%% Params:
%%  - Col_diagonal_matrix: integer (1-based column index)
%%  - Row_diagonal_matrix: integer (1-based row index)
%%  - ConnectIndex: integer (remaining length of diagonal to extract)
%%  - Matrix: list of lists representing the matrix
%% Returns:
%%  - List of elements forming the diagonal
%%------------------------------------------------------------------------------
get_Diagonal_right(_, _, 0, _) -> [];
get_Diagonal_right(Col_diagonal_matrix, Row_diagonal_matrix, ConnectIndex, Matrix) ->
  [get(Row_diagonal_matrix, Col_diagonal_matrix, Matrix) | get_Diagonal_right(Col_diagonal_matrix - 1, Row_diagonal_matrix + 1, ConnectIndex - 1, Matrix)].

%%------------------------------------------------------------------------------
%% check_winner_diagonal_left/4
%% Checks for a winner on diagonals going left (top-left to bottom-right).
%% Starts from the bottom edge and moves leftwards.
%% Params:
%%  - ColIndex: integer, current column index for checking (1-based) , 0 value is outranged
%%  - RowIndex: integer, current row index for checking (1-based)
%%  - Connect: integer, number of consecutive pieces needed to win
%%  - Matrix: list of lists representing the game board
%% Returns:
%%  - Winner atom if found (e.g., 'x' or 'o'), otherwise 'none'
%%------------------------------------------------------------------------------
check_winner_diagonal_left(0, 1, _, _) -> none;
check_winner_diagonal_left(0, RowIndex, Connect, Matrix) when RowIndex > 1 ->
  {C, _} = diagonal_matrix(),
  check_winner_diagonal_left(C, RowIndex - 1, Connect, Matrix);
check_winner_diagonal_left(ColIndex, RowIndex, Connect, Matrix) ->
  Diagonal = get_Diagonal_left(ColIndex, RowIndex, Connect, Matrix),
  case check_winner_helper(0, Connect, Diagonal) of
    none -> check_winner_diagonal_left(ColIndex - 1, RowIndex, Connect, Matrix);
    W -> W
  end.

%%------------------------------------------------------------------------------
%% check_winner_diagonal_right/4
%% Checks for a winner on diagonals going right (top-right to bottom-left).
%% Starts from the bottom edge and moves rightwards.
%% Params:
%%  - ColIndex: integer, current column index for checking (1-based), 8 value is outranged
%%  - RowIndex: integer, current row index for checking (1-based)
%%  - Connect: integer, number of consecutive pieces needed to win
%%  - Matrix: list of lists representing the game board
%% Returns:
%%  - Winner atom if found (e.g., 'x' or 'o'), otherwise 'none'
%%------------------------------------------------------------------------------
check_winner_diagonal_right(8, 1, _, _) -> none;
check_winner_diagonal_right(8, RowIndex, Connect, Matrix) when RowIndex > 1 ->
  {C, _} = diagonal_matrix(),
  check_winner_diagonal_right(C, RowIndex - 1, Connect, Matrix);
check_winner_diagonal_right(ColIndex, RowIndex, Connect, Matrix) ->
  Diagonal = get_Diagonal_right(ColIndex, RowIndex, Connect, Matrix),
  case check_winner_helper(0, Connect, Diagonal) of
    none -> check_winner_diagonal_right(ColIndex + 1, RowIndex, Connect, Matrix);
    W -> W
  end.

%%------------------------------------------------------------------------------
%% check_winner_diagonal/2
%% Aggregates diagonal winner checks from both diagonal directions.
%% Params:
%%  - Connect: integer, number of consecutive pieces needed to win
%%  - Matrix: list of lists representing the game board
%% Returns:
%%  - Winner atom if found, otherwise 'none'
%%------------------------------------------------------------------------------
check_winner_diagonal(Connect, Matrix) ->
  {ColIndex, RowIndex} = diagonal_matrix(),
  Right_result = check_winner_diagonal_right(ColIndex, RowIndex, Connect, Matrix),
  Left_result = check_winner_diagonal_left(ColIndex, RowIndex, Connect, Matrix),
  case {Right_result, Left_result} of
    {none, none} -> none;
    {Winner, none} -> Winner;
    {none, Winner} -> Winner;
    {W1, W2} when W1 == W2 -> W1;
    {W1, _} -> W1  % If inconsistent results, return first
  end.

%%------------------------------------------------------------------------------
%% check_winner_helper/3
%% Checks a sequence (list) for a winning streak of length Connect,
%% starting from Index backwards.
%% Params:
%%  - Index: integer, starting index in the sequence to check backwards (0-based)
%%  - Connect: integer, required consecutive pieces to win
%%  - Row: list of elements to check
%% Returns:
%%  - Winner atom if found (e.g., 'x' or 'o'), otherwise 'none'
%%------------------------------------------------------------------------------
check_winner_helper(Index, _, _) when Index < 0 -> none;
check_winner_helper(Index, Connect, Row) ->
  case logic:all_same(logic:skip_and_take(Index, Connect, Row)) of
    false -> check_winner_helper(Index - 1, Connect, Row);
    W -> W
  end.

%%------------------------------------------------------------------------------
%% check_winner_vertical/4
%% Checks for a winner vertically in each column from right to left.
%% Params:
%%  - ColIndex: integer, current column index to check (1-based)
%%  - Rows: integer, number of rows in the board
%%  - Connect: integer, required consecutive pieces to win
%%  - Matrix: list of lists representing the game board
%% Returns:
%%  - Winner atom if found, otherwise 'none'
%%------------------------------------------------------------------------------
check_winner_vertical(0, _, _, _) -> none;
check_winner_vertical(ColIndex, Rows, Connect, Matrix) ->
  Column = getColumn(Rows, ColIndex, Matrix),
  MaxIndex = Rows - Connect,
  case check_winner_helper(MaxIndex, Connect, Column) of
    none -> check_winner_vertical(ColIndex - 1, Rows, Connect, Matrix);
    W -> W
  end.

%%------------------------------------------------------------------------------
%% check_winner_horizontal/4
%% Checks for a winner horizontally in each row from bottom to top.
%% Params:
%%  - Cols: integer, number of columns in the board
%%  - RowIndex: integer, current row index to check (1-based)
%%  - Connect: integer, required consecutive pieces to win
%%  - Matrix: list of lists representing the game board
%% Returns:
%%  - Winner atom if found, otherwise 'none'
%%------------------------------------------------------------------------------
check_winner_horizontal(_, 0, _, _) -> none;
check_winner_horizontal(Cols, RowIndex, Connect, Matrix) ->
  Row = lists:nth(RowIndex, Matrix),
  MaxIndex = Cols - Connect,
  case check_winner_helper(MaxIndex, Connect, Row) of
    none -> check_winner_horizontal(Cols, RowIndex - 1, Connect, Matrix);
    W -> W
  end.

%%------------------------------------------------------------------------------
%% check_winner/4
%% Checks horizontally, vertically, and diagonally for a winner on the board.
%% Params:
%%  - Cols: integer, number of columns in the board
%%  - Rows: integer, number of rows in the board
%%  - Connect: integer, required consecutive pieces to win
%%  - Matrix: list of lists representing the game board
%% Returns:
%%  - Winner atom if found (e.g., 'x' or 'o'), otherwise 'none'
%%------------------------------------------------------------------------------
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

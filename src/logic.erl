-module(logic).
-author("hunto").

%% API
-export([
  move/5,
  skip_and_take/3,
  all_same/1,
  max_list/1,
  min_list/1,
  find_index_by_value/2,
  sum_lists/2,
  count_occurrences/2,
  element_to_string/1,
  reverse_row_order_in_matrix/1,
  is_top_row_full/1
]).

%%------------------------------------------------------------------------------
%% move/5
%% Tries to perform a move by placing Player's mark at ColumnIndex in the first
%% valid row (from top to bottom). Returns {IsMoved, NewBoard}.
%% Params:
%%  - BoardRows: List of rows (each a list of cells)
%%  - ColumnIndex: Column number (1-based) to place the move
%%  - Acc: Accumulator for building new board rows
%%  - Player: Player atom (e.g., 'x' or 'o')
%%  - IsMoved: 'notMoved' or 'moved' indicating if the move was done yet
%% Returns:
%%  - IsMoved: atom (notMoved,moved)
%%  - NewMatrix: the matrix that has the new token piece
%%------------------------------------------------------------------------------
move([], _, Acc, _, IsMoved) -> {IsMoved, lists:reverse(Acc)};
move([Row | Rest], ColumnIndex, Acc, Player, notMoved) ->
  {IsMov, NewRow} = move_helper(Row, ColumnIndex, [], Player, notMoved),
  move(Rest, ColumnIndex, [NewRow | Acc], Player, IsMov);
move([Row | Rest], ColumnIndex, Acc, Player, moved) ->
  move(Rest, ColumnIndex, [Row | Acc], Player, moved)
.

%%------------------------------------------------------------------------------
%% move_helper/5
%% Helper for move/5 to attempt placing Player's mark at ColumnIndex in a single Row.
%% Params:
%%  - Row: List of cells
%%  - ColIndex: Column number (1-based)
%%  - Acc: Accumulator for building new row
%%  - Player: Player atom
%%  - IsMoved: 'notMoved' or 'moved' indicating if the move was done yet
%% Returns:
%%  - IsMoved: atom (notMoved,moved)
%%  - NewRow: the row that has the new token piece
%%------------------------------------------------------------------------------
move_helper([], _, Acc, _, IsMoved) -> {IsMoved, lists:reverse(Acc)};
move_helper([empty | Rest], 1, Acc, Player, notMoved) ->
  {moved, lists:reverse(Acc) ++ [Player | Rest]};
move_helper([H | T], ColIndex, Acc, Player, IsMoved) ->
  move_helper(T, ColIndex - 1, [H | Acc], Player, IsMoved)
.

%%------------------------------------------------------------------------------
%% skip_and_take/3
%% Skips the first Skip elements and then takes the next Take elements from a list.
%% Params:
%%  - Skip: number of elements to skip
%%  - Take: number of elements to take
%%  - List: the input list
%% Returns:
%%  - sublist with Take elements starting after Skip elements
%%------------------------------------------------------------------------------
skip_and_take(_,_,[]) -> [];
skip_and_take(Skip, Take, [ _ | T]) when Skip > 0 , Take > 0 ->
  skip_and_take(Skip - 1 , Take, T);
skip_and_take(Skip, Take, [H | T]) when Skip == 0 , Take > 0 ->
  [H | skip_and_take(Skip , Take - 1, T)];
skip_and_take(Skip, Take, _) when Skip == 0 , Take == 0 -> [];
skip_and_take(_,_,_) -> []
.

%%------------------------------------------------------------------------------
%% all_same/1
%% Checks if all elements in a list are the same and not 'empty'.
%% Params:
%%  - List of elements
%% Returns:
%%  - The element if all same and non-empty, otherwise false
%%------------------------------------------------------------------------------
all_same([]) -> false;
all_same([X]) when X =/= empty -> X;
all_same([X]) when X == empty -> false;
all_same([X,X | T]) -> all_same([X | T]);
all_same([_,_ | _]) -> false
.

%%------------------------------------------------------------------------------
%% max_list/1
%% Finds the maximum element in a list of comparable elements.
%% Params:
%%  - List of numbers or comparable items
%% Returns:
%%  - Maximum element
%%------------------------------------------------------------------------------
max_list([F]) -> F;
max_list([F, S | R]) when F > S -> max_list([F | R]);
max_list([F, S | R]) when F =< S -> max_list([S | R])
.

%%------------------------------------------------------------------------------
%% min_list/1
%% Finds the minimum element in a list of comparable elements.
%% Params:
%%  - List of numbers or comparable items
%% Returns:
%%  - Minimum element
%%------------------------------------------------------------------------------
min_list([F]) -> F;
min_list([F, S | R]) when F > S -> min_list([S | R]);
min_list([F, S | R]) when F =< S -> min_list([F | R])
.

%%------------------------------------------------------------------------------
%% find_index_by_value/2
%% Finds 1-based index of first occurrence of X in list L.
%% Params:
%%  - X: Element to find
%%  - L: List to search in
%% Returns:
%%  - Index if found, -1 otherwise
%%------------------------------------------------------------------------------
find_index_by_value(X, L) ->
  find_index_by_value(X, L, 1)
.

find_index_by_value(X, [F | R], Index) when X =/= F-> find_index_by_value(X,  R, Index + 1);
find_index_by_value(X, [X | _], Index) -> Index;
find_index_by_value(_, _, _) -> -1
.

%%------------------------------------------------------------------------------
%% add_lists/3 (private helper)
%% Adds two lists element-wise, using an accumulator.
%% Params:
%%  - Two lists of numbers
%%  - Acc: accumulator for recursion
%% Returns:
%%  - Element-wise sum list
%%------------------------------------------------------------------------------
add_lists([], [], Acc) -> lists:reverse(Acc);
add_lists([H1|T1], [H2|T2], Acc) ->
  add_lists(T1, T2, [H1 + H2 | Acc])
.

%%------------------------------------------------------------------------------
%% sum_lists/2
%% Adds two lists element-wise.
%% Params:
%%  - Two lists of numbers
%% Returns:
%%  - List where each element is the sum of corresponding elements
%%------------------------------------------------------------------------------
sum_lists(L1, L2) -> add_lists(L1, L2, [])
.

%%------------------------------------------------------------------------------
%% count_occurrences/2
%% Counts how many times X occurs in a list.
%% Params:
%%  - X: element to count
%%  - List: list to search
%% Returns:
%%  - Number of occurrences of X
%%------------------------------------------------------------------------------
count_occurrences(_, []) -> 0;
count_occurrences(X, [X | T]) -> 1 + count_occurrences(X, T);
count_occurrences(X, [_ | T]) -> count_occurrences(X, T)
.

%%------------------------------------------------------------------------------
%% element_to_string/1
%% Converts an element to a string for display.
%% Params:
%%  - Elem: element to convert
%% Returns:
%%  - "." for empty; atom as string otherwise
%%------------------------------------------------------------------------------
element_to_string(Elem) when Elem == empty -> ".";
element_to_string(Elem) -> atom_to_list(Elem)
.

%%------------------------------------------------------------------------------
%% reverse_row_order_in_matrix/1
%% Reverses the order of rows in a matrix (list of lists).
%% Params:
%%  - Matrix: list of rows
%% Returns:
%%  - Matrix with row order reversed
%%------------------------------------------------------------------------------
reverse_row_order_in_matrix([]) -> [];
reverse_row_order_in_matrix([H | T]) ->
  reverse_row_order_in_matrix(T) ++ [H]
.

%%------------------------------------------------------------------------------
%% is_top_row_full/1
%% Checks whether the top row of the Connect4 board is full.
%%
%% In Connect4, the "top row" is the last list in the matrix (since tokens
%% drop from the top, and the bottom of the board is the first row).
%%
%% Params:
%%  - Matrix: list of rows (each row is a list of cells)
%%
%% Returns:
%%  - true if no cells in the top row are empty (i.e., the board is full at the top)
%%  - false if at least one cell in the top row is still empty
%%------------------------------------------------------------------------------
is_top_row_full(Matrix) ->
  LastRow = lists:last(Matrix),
  not lists:any(fun(Cell) -> Cell == empty end, LastRow)
.

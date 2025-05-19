-module(connect4).
-author("hunto").

%% API
-export([
  start/0,
  play/1
]).

%%------------------------------------------------------------------------------
%% start/0
%% Initializes a new Connect4 game by creating an empty game board matrix
%% and starts the game loop by calling play/1.
%%------------------------------------------------------------------------------
start() ->
  {Cols, Rows, _} = board:game_config(),
  Matrix = board:create_matrix(Cols, Rows, empty),
  play(Matrix)
.

%%------------------------------------------------------------------------------
%% play/1
%% Main game loop function that handles player input, updates the board,
%% checks for winners, and invokes AI moves.
%%
%% Params:
%%  - Matrix: current game board state (list of lists)
%%
%% Behavior:
%%  - Prints the current board state.
%%  - Prompts human player (symbol 'x') to enter a column number or 'exit'.
%%  - Validates input and attempts to place a move for 'x' player.
%%  - If move invalid (e.g., column full or out of range), re-prompts.
%%  - After human move, calls AI move function to update the board.
%%  - Checks for a winner after each move and ends the game if found.
%%
%% Returns:
%%  - ok when the game ends or player exits.
%%-----
play(Matrix) ->
  {Cols, Rows, Connect} = board:game_config(),
  case board:check_winner(Cols, Rows, Connect, Matrix) of
    none ->
      %% Check if top row full
      case logic:is_top_row_full(Matrix) of
        true ->
          board:print_matrix(Matrix),
          io:format("Game over: board is full, no more moves possible.~n"),
          ok;
        false ->
          io:format("Current Matrix:~n"),
          board:print_matrix(Matrix),
          io:format("Enter column (1-7) to move as 'x' (or type 'exit'): "),
          Input = string:trim(io:get_line("> ")),
          case Input of
            "exit" ->
              io:format("Exiting game.~n"),
              ok;
            _ ->
              case catch list_to_integer(Input) of
                Col when is_integer(Col), Col >= 1, Col =< 7 ->
                  case logic:move(Matrix, Col, [], x, notMoved) of
                    {notMoved, _} ->
                      io:format("Column full. Try a different one.~n"),
                      play(Matrix);
                    {moved, NewMatrix} ->
                      Player_turn_matrix = ai:ai_move(NewMatrix),
                      play(Player_turn_matrix)
                  end;
                _ ->
                  io:format("Invalid input. Please enter a number from 1 to 7.~n"),
                  play(Matrix)
              end
          end
      end;
    Winner ->
      board:print_matrix(Matrix),
      io:format("Winner is: ~p~n", [Winner]),
      ok
  end
.




























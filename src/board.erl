-module(board).
-compile(export_all).

create_empty_board() ->
	array:new([{size,8},
			   {fixed, true},
			   {default,array:new([{size, 8},
								   {fixed, true},
								   {default, 0}])}]).

set({X, Y}, Value, Board) ->
	array:set(X, array:set(Y, Value, array:get(X, Board)), Board).

get({X, Y}, Board) ->
	array:get(Y, array:get(X, Board)).

create_default_board() ->
	set({3, 3}, w,
		set({3, 4}, b,
			set({4, 3}, b,
				set({4, 4} , w,
					create_empty_board())))).


other_color(w) ->
	b;
other_color(b) ->
	w.

%% When color at board(x,y) = Color, Do Something
when_color({X, Y}, Color, Board, DoSomething) ->
	case board:get({X, Y}, Board) of
		Color ->
			DoSomething(X, Y, Color, Board);
		_ ->
			ok
	end.

traverse(Color, Board, DoSomething) ->
	traverse(0 ,0, Color, Board, DoSomething, []).

traverse(X, Y, Color, Board, DoSomething, Results) ->
	case when_color({X, Y}, Color, Board, DoSomething) of
		Current_Result when (Current_Result =/= ok) ->
			New_Result = Results ++ [Current_Result];
		_ ->
			New_Result = Results
	end,
	case {X, Y} of
		{7, 7} -> New_Result;
		{X, 7} -> board:traverse(X+1,
								 0,
								 Color,
								 Board,
								 DoSomething,
								 New_Result);
		{X, Y} -> board:traverse(X,
								 Y+1,
								 Color,
								 Board,
								 DoSomething,
								 New_Result);
		_ -> this_wont_happen
	end.

get_move_directions() ->
	[{-1, -1}, {-1, 0}, {-1, 1},
	 {0, 1}, {0, -1},
	 {1, -1}, {1, 0}, {1, 1}].

%% Find a valid move that can be initiated by coin at From{X,Y}
%% in Direction{D1,D2}. If a valid move exist, return the coordinates,
%% else return false
find_direction_move(From, Direction, Board) ->
	Color = board:get(From, Board),
	find_direction_move(From, Direction, Board, {{start_color, Color},
												 {count,0},
												 {start, none},
												 {path, []}
												}).

find_direction_move(From, Direction, Board, Results) ->
	{{start_color, Color},
	 {count, Flip_Count},
	 {start, Start},
	 {path, Path}} = Results,
	{X, Y} = From,
	{Dx, Dy} = Direction,
	Other_Color = other_color(Color),

	case {X + Dx, Y + Dy} of
		{X1, Y1} when (X1 =< 8) and (X1 >= 0) and (Y1 =< 8) and (Y1 >= 0) ->
			case board:get({X1, Y1}, Board) of
				0 when Flip_Count > 0 ->
					{{move, {X1, Y1}},
					 Path};
				0 when (Flip_Count == 0) ->
					no_move_here;
				Other_Color when (Flip_Count == 0) ->
					find_direction_move({X1, Y1},
										Direction,
										Board,
										{{start_color, Color},
										 {count, Flip_Count+1},
										 {start, {X, Y}},
										 {path, [{X1, Y1}]}
										});
				Other_Color when (Flip_Count > 0) ->
					find_direction_move({X1, Y1},
										Direction,
										Board,
										{{start_color, Color},
										 {count, Flip_Count+1},
										 {start, Start},
										 {path, Path ++ [{X1, Y1}]}
										});
				Color ->
					no_move_here;
				_ ->
					this_wont_happen
			end;
		_ ->
			no_move_here
	end.

%% For a given position, find all valid moves.
find_valid_line_moves_from(From, Board) ->
	lists:filter(fun(X) -> X =/= no_move_here end,
				 lists:map(fun(X) -> find_direction_move(From, X, Board) end,
						   get_move_directions())).

%% For a given color(Multiple positions on board), find all valid line moves.
find_valid_line_moves_for(Color, Board) ->
	lists:merge(
	  board:traverse(Color,
					 Board,
					 fun(X, Y, _C, B) ->
							 board:find_valid_line_moves_from({X, Y},
															  B)
					 end)).

merge_line_moves([], D) ->
	D;
merge_line_moves([H|T], D) ->
	{Move, Path} = H,
	case dict:is_key(Move, D) of
		true ->
			merge_line_moves(T, dict:append_list(Move,Path, D));
		false ->
			merge_line_moves(T, dict:store(Move, Path, D))
	end.

find_valid_moves_for(Color, Board) ->
	dict:to_list(
	  merge_line_moves(
		find_valid_line_moves_for(Color, Board),
		dict:new())).


apply_moves([], _Color, Board) ->
	Board;
apply_moves([H|T], Color, Board) ->
	{X, Y} = H,
	apply_moves(T, Color, board:set({X, Y}, Color, Board)).

play_move({{move, Move}, Flips}, Color, Board) ->
	All = Flips ++ [Move],
	apply_moves(All, Color, Board).

is_valid({X, Y}, Color, Board) ->
	Valid_Moves = find_valid_moves_for(Color, Board),
	F = fun(_Fun, {_X,_Y}, []) ->
			false;
	   (Fun, {X1,Y1}, [H|T]) ->
			case H of
				{{move,{X1,Y1}}, _} ->
					H;
				_ ->
					Fun(Fun, {X1,Y1}, T)
			end
		end,
	F(F, {X,Y}, Valid_Moves).

play({X, Y}, Color, Board) ->
	Move = is_valid({X, Y}, Color, Board),
	case Move of
		{{move, {X, Y}}, _} ->
			play_move(Move, Color, Board);
		false ->
			false
	end.

display_board(Board) ->
	B = array:to_list(Board),
	lists:map(fun (X) ->
					  io:format("~p~n",[array:to_list(X)]),
					  ok
			  end, B),
	ok.


really_dumb_ai_play(Color, Board) ->
   play_move(lists:nth(1,find_valid_moves_for(Color,Board)), Color, Board).

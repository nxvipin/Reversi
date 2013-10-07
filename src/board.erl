-module(board).
-compile(export_all).

create_empty_board() ->
	array:new([{size,8},
			   {fixed, true},
			   {default,array:new([{size, 8},
								   {fixed, true},
								   {default, 0}])}]).

set(X, Y, Value, Board) ->
	array:set(X, array:set(Y, Value, array:get(X, Board)), Board).

get(X, Y, Board) ->
	array:get(Y, array:get(X, Board)).

create_default_board() ->
	Board = create_empty_board(),
	B1 = set(3, 3, w, Board),
	B2 = set(3, 4, b, B1),
	B3 = set(4, 3, b, B2),
	B4 = set(4, 4, w, B3),
	B4.

other_color(w) ->
	b;
other_color(b) ->
	w.

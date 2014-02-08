-module(board_server).

-behaviour(gen_server).


-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {play_type, next_player, board}).


start_link() ->
	start_link(ai).
start_link(Type) ->
	%% Type can be ai or ri
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Type], []).


init([Type]) ->
	{ok, #state{play_type=Type,
				next_player=w,
				board=board:create_default_board()}}.


handle_call(get_state, _From, State) ->
	{reply, State, State};
handle_call(get_valid_moves, _From, State) ->
	{reply, board:find_valid_moves_for(State#state.next_player,
								  State#state.board), State};
handle_call({play, {X, Y}} , _From, State) ->
	Board = State#state.board,
	Next_Player = State#state.next_player,
	Move = {X, Y},
	case board:play(Move, Next_Player, Board) of
		false ->
			{reply, invalid_move, State};
		B ->
			board:display_board(B),
			{reply, ok, State#state{next_player = board:other_color(Next_Player),
									board = B}}
	end;

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

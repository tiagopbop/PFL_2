:- use_module(library(lists)).
:- use_module(library(random)).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Starting main predicate
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Initial predicate
play :-
    show_menu,
    repeat,
    catch(read(Choice), _, (write('\nInvalid input! Please enter a number between 1 and 6.\n'), show_menu, fail)),
    handle_choice_menu(Choice),
    !.

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Visual menus
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Main menu
show_menu :-
    nl,
    write('========================================='), nl,
    write('           Welcome to Replica            '), nl,
    write('========================================='), nl,
    write('1. Play Human vs Human'), nl,
    write('2. Play Human vs Computer'), nl,
    write('3. Play Computer vs Human'), nl,
    write('4. Play Computer vs Computer'), nl,
    write('5. Rules & How to play'), nl,
    write('6. Exit'), nl,
    write('10. [DEMO] Midgame scenario'), nl,
    write('20. [DEMO] Endgame scenario'), nl,
    write('========================================='), nl,
    write('Choose an option (1-6): ').

% Rules page
show_rules :-
    nl, nl, nl, nl,
    write('======================================================================================================================================================================================='), nl,
    write('                                                                                         Rules                                                                                         '), nl,
    write('======================================================================================================================================================================================='), nl,
    write('The goal of the game is to either capture an enemy king piece ("*" or "o") by moving onto it, or to reach the opposite corner from which your king starts with one allied king.'), nl, nl,
    write('You can only move 1 space at a time, exclusively in the direct direction of your enemy\'s territory, so for example, white can only move up, to the right or diagonally up-right.'), nl, nl,
    write('You can also jump over pieces, moving 1 space onto an ally piece, which will make you jump over all allied pieces placed together in that line of direction you chose, until you reach'), nl,
    write('an empty space or a space occupied by an enemy piece in which case you will capture it. You can\'t jump over enemy pieces.'), nl, nl,
    write('The king pieces, however, can not only jump but move backwards and all 8 directions but also transform normal pieces into king pieces. You can turn on the optional rule to make it so'), nl,
    write('there are only 2 transforms per player per game.'), nl, nl,
    write('Every single one of the commands needed to make all this actions mentioned here and more are available to check midgame if you enter the command "[commands].", as also shown in the'), nl,
    write('turn display also midgame.'), nl, nl,
    write('We hope you have fun!'), nl,
    write('======================================================================================================================================================================================='), nl, nl, nl, nl.

% Single bot difficulty page
show_difficulties_single :-
    nl, nl,
    write('========================================='), nl,
    write('          Choose Bot difficulty          '), nl,
    write('========================================='), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    write('3. Go back'), nl,
    write('========================================='), nl,
    write('Choose an option (1-3): ').

% First of multi bots difficulty page
show_difficulties_double_first :-
    nl, nl,
    write('========================================='), nl,
    write('         Choose Bot 1 difficulty         '), nl,
    write('========================================='), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    write('3. Go back'), nl,
    write('========================================='), nl,
    write('Choose an option (1-3): ').

% Second of multi bots difficulty page
show_difficulties_double_second :-
    nl, nl,
    write('========================================='), nl,
    write('         Choose Bot 2 difficulty         '), nl,
    write('========================================='), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    write('3. Go back'), nl,
    write('========================================='), nl,
    write('Choose an option (1-3): ').

% Optional rule page
show_optional_rule :-
    nl, nl,
    write('=================================================='), nl,
    write('              Limited transformations             '), nl,
    write('=================================================='), nl,
    write(''), nl,
    write('Do you want to turn on limited transformations?'), nl,
    write(''), nl,
    write('=================================================='), nl,
    write('Choose an option [ yes | no ]: ').

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Handling of choices from the menus
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Handling of input received on main menu
handle_choice_menu(1) :-
    initial_board(1, Board),
    initial_state(human, human, Board).
handle_choice_menu(2) :-
    show_difficulties_single,
    repeat,
    catch(read(Choice), _, (write('\nInvalid input! Please enter a number between 1 and 3.\n'), show_difficulties_single, fail)),
    handle_choice_difficulty_hc(Choice),
    !.
handle_choice_menu(3) :-
    show_difficulties_single,
    repeat,
    catch(read(Choice), _, (write('\nInvalid input! Please enter a number between 1 and 3.\n'), show_difficulties_single, fail)),
    handle_choice_difficulty_ch(Choice),
    !.
handle_choice_menu(4) :-
    show_difficulties_double_first,
    repeat,
    catch(read(Choice), _, (write('\nInvalid input! Please enter a number between 1 and 3.\n'), show_difficulties_double_first, fail)),
    handle_choice_difficulty_cc_1(Choice),
    !.
handle_choice_menu(5) :-
    show_rules,
    play.
handle_choice_menu(6) :-
    write('\nExiting the game!\n'), nl.
handle_choice_menu(10) :-
    initial_board(2, Board),
    initial_state(human, human, Board).
handle_choice_menu(20) :-
    initial_board(3, Board),
    initial_state(human, human, Board).
handle_choice_menu(_) :-
    write('\nInvalid option. Please try again.'), nl,
    play.

% Handling of diffculty input received when choosing to play human vs bot
handle_choice_difficulty_hc(1) :-
    initial_board(1, Board),
    initial_state(human, bot_random, Board).
handle_choice_difficulty_hc(2) :-
    initial_board(1, Board),
    initial_state(human, bot_greedy, Board).
handle_choice_difficulty_hc(3) :-
    play.

% Handling of diffculty input received when choosing to play bot vs human
handle_choice_difficulty_ch(1) :-
    initial_board(1, Board),
    initial_state(bot_random, human, Board).
handle_choice_difficulty_ch(2) :-
    initial_board(1, Board),
    initial_state(bot_greedy, human, Board).
handle_choice_difficulty_ch(3) :-
    play.

% First handling of difficulty input received when choosing to play bot vs bot
handle_choice_difficulty_cc_1(1) :-
    show_difficulties_double_second,
    repeat,
    catch(read(Choice), _, (write('\nInvalid input! Please enter a number between 1 and 3.\n'), show_difficulties_double_second, fail)),
    handle_choice_difficulty_cc_2(1, Choice),
    !.
handle_choice_difficulty_cc_1(2) :-
    show_difficulties_double_second,
    repeat,
    catch(read(Choice), _, (write('\nInvalid input! Please enter a number between 1 and 3.\n'), show_difficulties_double_second, fail)),
    handle_choice_difficulty_cc_2(2, Choice),
    !.
handle_choice_difficulty_cc_1(3) :-
    play.

% Second handling of difficulty input received when choosing to play bot vs bot
handle_choice_difficulty_cc_2(1, 1) :-
    initial_board(1, Board),
    initial_state(bot_random, bot_random, Board).
handle_choice_difficulty_cc_2(1, 2) :-
    initial_board(1, Board),
    initial_state(bot_random, bot_greedy, Board).
handle_choice_difficulty_cc_2(2, 1) :-
    initial_board(1, Board),
    initial_state(bot_greedy, bot_random, Board).
handle_choice_difficulty_cc_2(2, 2) :-
    initial_board(1, Board),
    initial_state(bot_greedy, bot_greedy, Board).
handle_choice_difficulty_cc_2(_, 3) :-
    play.


% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Creation of initial board and game state
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Receives information from the first menus and pages, asks about the optional rule and then creates the first gamestate, sending it to the gameloop
initial_state(Player1Type, Player2Type, Board) :-
    show_optional_rule,
    repeat,
    catch(read(RuleChoice), _, (write('\n\nInvalid input! Please write yes or no.\n'), show_optional_rule, fail)),
    (   RuleChoice = yes -> 
        WhiteTransformCounter = 0,
        BlackTransformCounter = 0
    ;   RuleChoice = no ->
        WhiteTransformCounter = -100,
        BlackTransformCounter = -100
    ;   RuleChoice = yesdemo ->
        WhiteTransformCounter = 1,
        BlackTransformCounter = 2
    ;   write('\n\nInvalid input! Please write yes or no.\n'),
        show_optional_rule,
        fail
    ),
    !, nl,
    GameState = game(Board, white, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter),
    game_loop(GameState).

% Loads the initial or chosen board
initial_board(BoardNum, Board) :-
    (   BoardNum = 1 ->
        Board = [[empty, empty, empty, empty, black, black, black, kingb],
                [empty, empty, empty, empty, black, black, black, black],
                [empty, empty, empty, empty, empty, empty, black, black],
                [empty, empty, empty, empty, empty, empty, black, black],
                [white, white, empty, empty, empty, empty, empty, empty],
                [white, white, empty, empty, empty, empty, empty, empty],
                [white, white, white, white, empty, empty, empty, empty],
                [kingw, white, white, white, empty, empty, empty, empty]]
    ;   BoardNum = 2 ->
        Board = [[empty, empty, empty, black, empty, black, black, empty],
                [empty, empty, empty, black, empty, empty, black, black],
                [empty, empty, empty, empty, empty, empty, black, black],
                [empty, empty, kingw, empty, black, empty, empty, black],
                [white, empty, empty, white, empty, black, empty, kingb],
                [white, white, empty, empty, white, empty, empty, empty],
                [white, white, empty, empty, white, empty, empty, empty],
                [empty, white, white, white, empty, empty, empty, empty]]
    ;   BoardNum = 3 ->
        Board = [[empty, empty, white, kingb, empty, white, empty, empty],
                [empty, white, kingw, black, empty, empty, white, empty],
                [empty, black, empty, empty, white, empty, empty, empty],
                [empty, empty, empty, empty, kingw, empty, white, empty],
                [empty, empty, kingb, white, empty, white, empty, empty],
                [empty, empty, empty, empty, empty, empty, empty, empty],
                [empty, empty, empty, white, black, kingb, empty, empty],
                [empty, empty, empty, black, white, empty, empty, empty]]
    ).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Game loop
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Handles the recurrent gamestates in a loop until a player forfeits or loses, returning to the menu
game_loop(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter)) :-
    display_game(Board, CurrentPlayer),

    (   game_over(Board, Winner) -> 
        format('\nGame Over! Winner: ~w\n', [Winner]),
        play
    ;
        format('\n~w player\'s turn.\n', [CurrentPlayer]),
        nl,
        write('====================================='), nl,
        write('[commands]. -> Available commands'), nl,
        write('=====================================\n'), nl,
        choose_move(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter), Move),
        (   move(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter, Move, NewGameState, GameEnded),
            (   GameEnded = true -> 
                play    
            ;   
                game_loop(NewGameState)
            )
        ;   
            game_loop(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter))
        )
    ).


% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Board dinamic display
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Draws the board in the terminal dinamically with each new gamestate
display_game(Board, CurrentPlayer) :-
    nl,
    write('  +------------------------+'), nl,
    display_rows(Board, 8),
    write('  +------------------------+'), nl,
    write('    1  2  3  4  5  6  7  8'), nl.

display_rows([], _).
display_rows([Row|Rest], RowNum) :-
    write(RowNum), write(' |'),
    display_row(Row),
    write('|'), nl,
    NextRowNum is RowNum - 1,
    display_rows(Rest, NextRowNum).

display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    display_row(Rest).

display_cell(empty) :- write(' . ').
display_cell(white) :- write(' W ').
display_cell(black) :- write(' B ').
display_cell(kingw) :- write(' o ').
display_cell(kingb) :- write(' * ').

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Move choice logic
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Chooses the move to be evaluated as valid, either by asking by input if human or choosing from a list, scored or random, if bot
choose_move(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter), Action) :-
    (

        (CurrentPlayer = white, Player1Type = bot_random ; CurrentPlayer = black, Player2Type = bot_random) ->
        bot_move_random(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter), Action)
    ;
        (CurrentPlayer = white, Player1Type = bot_greedy ; CurrentPlayer = black, Player2Type = bot_greedy) ->
        bot_move_greedy(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter), Action)
    ;

        human_move(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter), Action)
    ).

% Handles the human move input
human_move(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter), Action) :-
    repeat,
    format('Enter move or command for ~w: ', [CurrentPlayer]),
    catch(read(Action), _, 
        (write('\nInvalid input! Please try again.\n'), 
        display_game(Board, CurrentPlayer), 
        format('\n~w player\'s turn.\n', [CurrentPlayer]),
        nl,
        write('====================================='), nl,
        write('[commands]. -> Available commands'), nl,
        write('=====================================\n'), nl,
        fail)), 
        nl,
    !.

% Handles the bot random difficulty move choice
bot_move_random(game(Board, CurrentPlayer, _, _, WhiteTransformCounter, BlackTransformCounter), Move) :-
    valid_moves(Board, CurrentPlayer, WhiteTransformCounter, BlackTransformCounter, Moves),
    random_member(Move, Moves),
    format('Bot (Random) chooses action: ~w\n', [Move]).

% Handles the bot greedy difficulty move choice
bot_move_greedy(game(Board, CurrentPlayer, _, _, WhiteTransformCounter, BlackTransformCounter), Move) :-
    value(Board, CurrentPlayer, WhiteTransformCounter, BlackTransformCounter, SortedScoredMoves, TotalValue),
    reverse(SortedScoredMoves, [BestValue-_ | _]),
    findall(M, member(BestValue-M, SortedScoredMoves), BestMoves),
    random_member(Move, BestMoves),
    format('Bot (Greedy) chooses action: ~w (Score: ~w)\n', [Move, BestValue]).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Move evaluation for bots
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluates a given step or jump move with a value of efficiency and usefulness
evaluate_move(Board, CurrentPlayer, [X1, Y1, X2, Y2], Value) :-
    get_piece(Board, X2, Y2, TargetPiece),
    get_piece(Board, X1, Y1, OwnPiece),
    (   OwnPiece = kingw, CurrentPlayer = white, (is_enemy(CurrentPlayer, TargetPiece); TargetPiece = empty),  X2 = 8, Y2 = 8 ->
        TargetValue = 100
    ;   OwnPiece = kingb, CurrentPlayer = black, (is_enemy(CurrentPlayer, TargetPiece); TargetPiece = empty),  X2 = 1, Y2 = 1 ->
        TargetValue = 100
    ;   is_enemy(CurrentPlayer, TargetPiece) ->
        piece_type_value(TargetPiece, TargetValue)
    ;   is_friendly(CurrentPlayer, TargetPiece) ->
        TargetValue = 0
    ;   TargetPiece = empty ->
        TargetValue = 0
    ),
    (   is_move_exposed(Board, CurrentPlayer, [X2, Y2]) ->
        Value is TargetValue - 1
    ;   Value = TargetValue
    ).

% Evaluates a given transform move with a value of efficiency and usefulness
evaluate_move(Board, CurrentPlayer, [transform, X1, Y1, X2, Y2], Value) :-
    (CurrentPlayer = white -> Opponent = black; Opponent = white),
    findall([OX1, OY1, OX2, OY2], valid_move(Board, Opponent, [OX1, OY1, OX2, OY2]), OpponentMoves),
    (   member([_, _, X1, Y1], OpponentMoves) ->
        Value = 0
    ;   CurrentPlayer = black, X2 = 1, Y2 = 1 ->
        Value = 100
    ;   CurrentPlayer = white, X2 = 8, Y2 = 8 ->
        Value = 100
    ;   CurrentPlayer = black, X2 =< 3, Y2 =< 3 ->
        Value = 1
    ;   CurrentPlayer = white, X2 >= 6, Y2 >= 6 ->
        Value = 1
    ;   Value = 0
    ).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Move evaluation utility predicates
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Helps accurately determine move value by examining future spot neighboor enemy pieces
is_move_exposed(Board, CurrentPlayer, [X2, Y2]) :-
    (CurrentPlayer = white -> Opponent = black; Opponent = white),
    findall([OX1, OY1, OX2, OY2], valid_move(Board, Opponent, [OX1, OY1, OX2, OY2]), OpponentMoves),
    member([_, _, X2, Y2], OpponentMoves).    

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Move logic
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Handles the input given by the player midgame. Can be either a word for one of the various available functional commands or just a move. In case of a move, it validates it and then applies it,
% creating a new board and game state. In case of a command, displays the information or executes the action of the command and then returns to the same turn interface waiting for a new input
move(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter, Action, NewGameState, GameEnded) :-
    (   Action = [X1, Y1, X2, Y2],
        valid_move(Board, CurrentPlayer, [X1, Y1, X2, Y2]) ->
        get_piece(Board, X1, Y1, Piece),
        determine_target(Board, CurrentPlayer, Piece, [X1, Y1, X2, Y2], FinalX, FinalY),
        get_piece(Board, X2, Y2, CapturedPiece),
        make_move(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, BlackTransformCounter), [X1, Y1, FinalX, FinalY], NewGameState),
        NewGameState = game(NewBoard, _, _, _, WhiteTransformCounter, BlackTransformCounter),
        (   game_over(CapturedPiece, CurrentPlayer, Winner) -> 
            display_game(NewBoard, CurrentPlayer),
            format('\nGame Over! Winner: ~w\n\n', [Winner]),
            GameEnded = true
        ;   GameEnded = false
        )
    ;   
        Action = [transform, KingX, KingY, TargetX, TargetY],
        valid_transform(Board, CurrentPlayer, [KingX, KingY, TargetX, TargetY]) ->
        (   CurrentPlayer = white, WhiteTransformCounter < 2 ->
            NewWhiteTransformCounter is WhiteTransformCounter + 1,
            make_transform(game(Board, CurrentPlayer, Player1Type, Player2Type, NewWhiteTransformCounter, BlackTransformCounter), [KingX, KingY, TargetX, TargetY], NewGameState),
            GameEnded = false
        ;   CurrentPlayer = black, BlackTransformCounter < 2 ->
            NewBlackTransformCounter is BlackTransformCounter + 1,
            make_transform(game(Board, CurrentPlayer, Player1Type, Player2Type, WhiteTransformCounter, NewBlackTransformCounter), [KingX, KingY, TargetX, TargetY], NewGameState),
            GameEnded = false   
        ;   write('Transform limit reached. Please try another move.\n'),
            fail
        )
    ;
        Action = [commands] ->
        nl,
        write('====================================================================================================================='), nl,
        write('[OriginX, OriginY, TargetSpotX, TargetSpotY]. -> Step (Into an empty spot or enemy)'), nl,
        write('[OriginX, OriginY, TargetSpotX, TargetSpotY]. -> Jump (Into an ally)'), nl,
        write('[transform, kingX, kingY, allyX, allyY]. -> Transform (No enemy piece can be in between the king and ally piece)'), nl,
        write('[forfeit]. -> Forfeit (Give up and return to the menu)'), nl,
        write('[validmoves]. -> List of current possible moves'), nl,
        write('[scoredmoves]. -> List of current possible moves scored by effectiveness or utility (Score-[Move])'), nl,
        write('[value]. -> Current number of pieces of each side and winning chances evaluation'), nl,
        write('====================================================================================================================='), nl,
        GameEnded = false,
        fail

    ;
        Action = [forfeit] ->
        (   CurrentPlayer = white -> 
            format('Game Over! Winner: ~w\n', [black]),
            GameEnded = true
        ;   CurrentPlayer = black -> 
            format('Game Over! Winner: ~w\n', [white]),
            GameEnded = true
        )

    ;
        Action = [validmoves] ->
        valid_moves(Board, CurrentPlayer, WhiteTransformCounter, BlackTransformCounter, Moves),
        format('\nValid moves for ~w: ~w\n', [CurrentPlayer, Moves]),
        fail

    ;
        Action = [scoredmoves] ->
        value(Board, CurrentPlayer, WhiteTransformCounter, BlackTransformCounter, SortedScoredMoves, TotalValue),
        format('\nValid moves and their scores for ~w: ~w\n', [CurrentPlayer, SortedScoredMoves]),
        fail

    ;
        Action = [value] ->
        calculate_values(Board, CurrentPlayer, WhiteTransformCounter, BlackTransformCounter, TotalValue, OpTotalValue),
        format('Your current game score is ~w agains\'t his of ~w\n', [TotalValue, OpTotalValue]),
        fail

    ;   
        write('Invalid move. Please try again.\n'),
        fail
    ).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Move utility predicates
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% After validated, executes the move and creates a new gamestate
make_move(game(Board, CurrentPlayer, P1, P2, WhiteTransformCounter, BlackTransformCounter), [X1, Y1, FinalX, FinalY], game(NewBoard, NextPlayer, P1, P2, WhiteTransformCounter, BlackTransformCounter)) :-
    apply_move(Board, [X1, Y1, FinalX, FinalY], TempBoard),
    next_player(CurrentPlayer, NextPlayer),
    NewBoard = TempBoard.

% Applies the move to the board
apply_move(Board, [X1, Y1, X2, Y2], NewBoard) :-
    set_piece(Board, X1, Y1, empty, TempBoard),
    get_piece(Board, X1, Y1, Piece),
    set_piece(TempBoard, X2, Y2, Piece, NewBoard).

% After validated, executes the move
make_transform(game(Board, CurrentPlayer, P1, P2, WhiteTransformCounter, BlackTransformCounter), [KingX, KingY, TargetX, TargetY], game(NewBoard, NextPlayer, P1, P2, WhiteTransformCounter, BlackTransformCounter)) :-
    transform_piece(Board, CurrentPlayer, [KingX, KingY, TargetX, TargetY], TempBoard),
    next_player(CurrentPlayer, NextPlayer),
    NewBoard = TempBoard.

% Applies the move to the board
transform_piece(Board, Player, [KingX, KingY, TargetX, TargetY], NewBoard) :-
    get_piece(Board, KingX, KingY, King),
    (Player = white -> King = kingw ; King = kingb),
    get_piece(Board, TargetX, TargetY, Friendly),
    (Player = white -> Friendly = white ; Friendly = black),
    line_of_sight_clear(Board, KingX, KingY, TargetX, TargetY, Player),
    (Player = white -> NewKing = kingw ; NewKing = kingb),
    set_piece(Board, TargetX, TargetY, NewKing, NewBoard).

% Helps calculate the overall values of both the current player as well as the enemy player, for comparison
calculate_values(Board, CurrentPlayer, WhiteTransformCounter, BlackTransformCounter, TotalValue, OpTotalValue) :-
    value(Board, CurrentPlayer, WhiteTransformCounter, BlackTransformCounter, _, TotalValue),

    (CurrentPlayer = white -> Opponent = black; Opponent = white),
    value(Board, Opponent, WhiteTransformCounter, BlackTransformCounter, _, OpTotalValue).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Move validation logic
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Receives and evaluates a normal move for its validity, through different conditions
valid_move(Board, Player, [X1, Y1, X2, Y2]) :-
    within_bounds(X1, Y1),
    within_bounds(X2, Y2),
    get_piece(Board, X1, Y1, Piece),
    Piece \= empty,
    (Player = white -> (Piece = white ; Piece = kingw) ; (Piece = black ; Piece = kingb)),
    determine_target(Board, Player, Piece, [X1, Y1, X2, Y2], FinalX, FinalY).

% Receives and evaluates a transform move for its validity, through different conditions
valid_transform(Board, Player, [KingX, KingY, TargetX, TargetY]) :-
    within_bounds(KingX, KingY),
    within_bounds(TargetX, TargetY),
    get_piece(Board, KingX, KingY, King),
    (Player = white -> King = kingw ; King = kingb),
    get_piece(Board, TargetX, TargetY, Friendly),
    (Player = white -> Friendly = white ; Friendly = black),
    line_of_sight_clear(Board, KingX, KingY, TargetX, TargetY, Player).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Move validation utility predicates
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Helps check if a jump ends up on a wall or valid space, among other things
determine_target(Board, Player, Piece, [X1, Y1, X2, Y2], FinalX, FinalY) :-
    is_valid_direction(Player, Piece, X1, Y1, X2, Y2),
    (
        get_piece(Board, X2, Y2, empty) -> FinalX = X2, FinalY = Y2
    ;   get_piece(Board, X2, Y2, TargetPiece),
        is_enemy(Player, TargetPiece) ->
        FinalX = X2, FinalY = Y2
    ;   get_piece(Board, X2, Y2, TargetPiece),
        (   is_friendly(Player, TargetPiece) -> 
            extend_move(Board, Player, Piece, [X1, Y1, X2, Y2], FinalX, FinalY)
        )
    ;   fail
    ).

% Helps check the movement validity on its allowed directions
is_valid_direction(white, Piece, X1, Y1, X2, Y2) :-
    (   Piece = kingw ->
        (X2 =:= X1 + 1, Y2 =:= Y1 ; X2 =:= X1 + 1, Y2 =:= Y1 + 1 ; X2 =:= X1, Y2 =:= Y1 + 1 ; X2 =:= X1 - 1, Y2 =:= Y1 ; X2 =:= X1, Y2 =:= Y1 - 1 ; X2 =:= X1 - 1, Y2 =:= Y1 + 1 ; X2 =:= X1 - 1, Y2 =:= Y1 - 1 ; X2 =:= X1 + 1, Y2 =:= Y1 - 1)
    ;   (X2 =:= X1 + 1, Y2 =:= Y1 ; X2 =:= X1 + 1, Y2 =:= Y1 + 1 ; X2 =:= X1, Y2 =:= Y1 + 1)
    ).
is_valid_direction(black, Piece, X1, Y1, X2, Y2) :-
    (   Piece = kingb ->
        (X2 =:= X1 + 1, Y2 =:= Y1 ; X2 =:= X1 + 1, Y2 =:= Y1 + 1 ; X2 =:= X1, Y2 =:= Y1 + 1 ; X2 =:= X1 - 1, Y2 =:= Y1 ; X2 =:= X1, Y2 =:= Y1 - 1 ; X2 =:= X1 - 1, Y2 =:= Y1 + 1 ; X2 =:= X1 - 1, Y2 =:= Y1 - 1 ; X2 =:= X1 + 1, Y2 =:= Y1 - 1)
    ;   (X2 =:= X1 - 1, Y2 =:= Y1 ; X2 =:= X1 - 1, Y2 =:= Y1 - 1 ; X2 =:= X1, Y2 =:= Y1 - 1)
    ).

% Main helping predicate for checking if a jump ends up on a wall or valid space
extend_move(Board, Player, Piece, [X1, Y1, X2, Y2], FinalX, FinalY) :-
    DirectionX is X2 - X1,
    DirectionY is Y2 - Y1,
    NewX is X2 + DirectionX,
    NewY is Y2 + DirectionY,
    within_bounds(NewX, NewY),
    is_valid_direction(Player, Piece, X2, Y2, NewX, NewY),
    get_piece(Board, NewX, NewY, TargetPiece),
    (
        TargetPiece = empty ->
        FinalX = NewX, FinalY = NewY
    ;
        is_enemy(Player, TargetPiece) ->
        FinalX = NewX, FinalY = NewY
    ;
        is_friendly(Player, TargetPiece) ->
        extend_move(Board, Player, Piece, [X2, Y2, NewX, NewY], FinalX, FinalY)
    ;
        fail
    ).

% Makes sure the line of sight of a king is clear for a transform
line_of_sight_clear(Board, KingX, KingY, TargetX, TargetY, Player) :-
    direction_vector(KingX, KingY, TargetX, TargetY, DX, DY),
    line_of_sight_path(Board, KingX, KingY, DX, DY, Player, TargetX, TargetY).

% Determines the direction vector between the king and its allied target
direction_vector(KingX, KingY, TargetX, TargetY, DX, DY) :-
    DX is sign(TargetX - KingX),
    DY is sign(TargetY - KingY).

% Makes and checks the line of sight path between the king and its allied piece when performing a transform
line_of_sight_path(Board, X, Y, DX, DY, Player, TargetX, TargetY) :-
    NextX is X + DX,
    NextY is Y + DY,
    (NextX = TargetX, NextY = TargetY -> true ; 
    within_bounds(NextX, NextY),
    get_piece(Board, NextX, NextY, Piece),
    (Piece = empty ; is_friendly(Player, Piece)) -> 
    line_of_sight_path(Board, NextX, NextY, DX, DY, Player, TargetX, TargetY)
    ; fail). 

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Valid and scored moves lists
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Constructs a list of valid moves that can be made by a given player at any point of a game 
valid_moves(Board, Player, WhiteTransformCounter, BlackTransformCounter, Moves) :-
    findall([X1, Y1, X2, Y2], valid_move(Board, Player, [X1, Y1, X2, Y2]), RegularMoves),
    (   Player = white, WhiteTransformCounter < 2 ->
        findall([transform, X1, Y1, X2, Y2], valid_transform(Board, Player, [X1, Y1, X2, Y2]), TransformMoves)
    ;   Player = black, BlackTransformCounter < 2 ->
        findall([transform, X1, Y1, X2, Y2], valid_transform(Board, Player, [X1, Y1, X2, Y2]), TransformMoves)
    ;   TransformMoves = []
    ),
    append(RegularMoves, TransformMoves, Moves).

% Constructs a sorted list of valid moves of a given player with a score attributed to each one measuring their effectiveness and usefulness, as well as return the overall score of all the moves
value(Board, CurrentPlayer, WhiteTransformCounter, BlackTransformCounter, SortedScoredMoves, TotalValue) :-
    valid_moves(Board, CurrentPlayer, WhiteTransformCounter, BlackTransformCounter, Moves),
    findall(Value-M, (member(M, Moves), evaluate_move(Board, CurrentPlayer, M, Value)), ScoredMoves),
    sort(ScoredMoves, SortedScoredMoves),
    findall(Value, member(Value-_, ScoredMoves), Values),
    sumlist(Values, TotalValue).  

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Game over verifications
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Checks for a game over by king capture
game_over(CapturedPiece, CurrentPlayer, Winner) :-
    (   CapturedPiece = kingw, CurrentPlayer = black ->
        Winner = black
    ;   CapturedPiece = kingb, CurrentPlayer = white ->
        Winner = white
    ).

% Checks for a game over by a king reaching the opposite corner
game_over(Board, Winner) :-
    (   get_piece(Board, 8, 8, kingw) ->
        Winner = white
    ;   get_piece(Board, 1, 1, kingb) ->
        Winner = black
    ).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% General utility predicates
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Gets a piece type from the board from a given location
get_piece(Board, X, Y, Piece) :-
    reverse_index(Y, ActualRowIndex),
    nth1(ActualRowIndex, Board, Row),
    nth1(X, Row, Piece).

% Sets a piece type on the board on a given location
set_piece(Board, X, Y, Piece, NewBoard) :-
    reverse_index(Y, ActualRowIndex),
    nth1(ActualRowIndex, Board, Row, RestRows),
    nth1(X, Row, _, RestCells),
    nth1(X, NewRow, Piece, RestCells),
    nth1(ActualRowIndex, NewBoard, NewRow, RestRows).

% Used for checking if a piece is within bounds of the board
within_bounds(X, Y) :-
    between(1, 8, X),
    between(1, 8, Y).

% Used for inversing the Y axis so the origin of both axis can be in the bottom left corner instead of the top left corner
reverse_index(Y, ActualRowIndex) :-
    ActualRowIndex is 9 - Y.

% Used by the within_bounds predicate to accurately check if some value is between two set ones
between(Low, High, Value) :-
    Low =< High,
    (Value = Low;
     Low1 is Low + 1,
     between(Low1, High, Value)).  

% Mini predicates used to get either enemy or allied pieces from a given player, a pieces value or the next player, all dinamically
next_player(white, black).
next_player(black, white).    

is_enemy(white, black).
is_enemy(white, kingb).
is_enemy(black, white).
is_enemy(black, kingw).

is_friendly(white, white).
is_friendly(white, kingw).
is_friendly(black, black).
is_friendly(black, kingb).

piece_type_value(black, 1).
piece_type_value(white, 1).
piece_type_value(kingb, 100).
piece_type_value(kingw, 100).
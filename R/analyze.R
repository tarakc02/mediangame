#' @export
analyze_game <- function(game) {
    analyze_round <- function(round, initial_score) {
        players <- names(attr(round, "old_score"))
        player_data <- function(player) {
            old_score <- attr(round, "old_score")
            my_score <- old_score[[player]]
            p1_score <- max(old_score[setdiff(players, player)])
            p2_score <- min(old_score[setdiff(players, player)])
            win <-
                if (game$game_winner == player) {
                    1L } else if (game$game_winner == "NO WINNER") {
                        0L } else -1L

            tibble::data_frame(player, my_score, p1_score, p2_score,
                       my_play = round[[player]],
                       game_win = win,
                       round_win = round$winner == player,
                       round_winner = round$winner,
                       round_winning_play = round$winning_play)
        }
        map_df(players, player_data)
    }
    map_df(game$results, analyze_round)
}


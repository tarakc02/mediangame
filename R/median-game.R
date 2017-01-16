#' @import tibble
#' @import purrr
NULL

random_weights <- function(n = 8) {
    weights <- sample(n)
    weights / sum(weights)
}

# each play allows for some randomness
play <- function(weights) {
    n <- length(weights)
    sample(n, size = 1, prob = weights)
}

#' @export
random_player <- function(myscore, others) {
    random_weights()
}

determine_winner <- function(plays) {
    winning_play <- median(as.numeric(plays))
    winner <- which(plays == winning_play)
    if (length(winner) != 1L) {
        winner <- c("NO WINNER" = "NO WINNER")
        winning_play <- NA_integer_
    }
    structure(names(winner), winning_play = winning_play)
}

simulate_round <- function(players, scores) {
    plays <- map2(
        players,
        seq_along(players),
        ~play(.x(scores[.y], scores[-.y]))
    )
    winner <- determine_winner(plays)
    new_score <- scores
    if (winner != "NO WINNER")
        new_score[winner] <- new_score[winner] + 1L
    structure(
        c(plays,
          winner = winner,
          winning_play = attr(winner, "winning_play")),
        old_score = scores,
        new_score = new_score)
}

#' @export
simulate_game <- function(players) {
    NUM_ROUNDS <- 8

    player_names <- names(players)

    # initalize scores to zero
    scores <- vector("list", NUM_ROUNDS + 1)
    scores[[1]] <- structure(
        integer(length(players)),
        names = player_names
    )

    # initialize the results list
    results <- vector("list", NUM_ROUNDS)

    for (round in seq_len(NUM_ROUNDS)) {
        round_results <- simulate_round(players, scores[[round]])
        scores[[round + 1]] <- attr(round_results, "new_score")
        results[[round]] <- round_results
    }

    game_winner <- determine_winner(scores[[NUM_ROUNDS + 1]])

    #results <- map_df(results, as_data_frame)
    structure(
        list(results = results, scores = scores, game_winner = game_winner),
        class = c("median_game_results", "list")
    )
}

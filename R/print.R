#' @export
print.median_game_results <- function(x, ...) {
    cat("winner:", x$game_winner, "\n")
    final_score <- x$scores[[length(x$scores)]]
    cat("final scores:\n")
    tmp <- map2(names(final_score), final_score,
                ~cat(.x, ": ", .y, "\n", sep = ""))
}

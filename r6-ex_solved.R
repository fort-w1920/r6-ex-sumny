library(R6)

bavaria_cards <- R6Class("bavaria_cards ", public = list(
  cards = NULL,
  initialize = function() {
    cards <- paste(rep(c("Gras", "Herz", "Eichel", "Schell"), each = 9L),
      c(6:10, "Unter", "Ober", "Koenig", "Ass"), sep = "_")
    self$cards <- cards[sample(36L, size = 36L, replace = FALSE)]
  },
  print = function(...) {
    cat("Cards: \n", self$cards, "\n")
    invisible(self)
  },
  draw = function(number) {
    checkmate::assert_count(number)
    if (length(self$cards) >= number) {
      card_sequence <- seq_len(number)
      drawn <- self$cards[card_sequence]
      self$cards <- self$cards[- card_sequence]
      drawn
    } else {
      stop("You need to remix and reshuffle the cards first.")
    }
  },
  reshuffle = function() {
    self$initialize()
  },
  rebase = function(index) {
    if (length(self$cards) >= index) {
      card_sequence <- seq_len(index)
      self$cards <- c(self$cards[setdiff(seq_along(self$cards), card_sequence)],
        self$cards[card_sequence])
    } else {
      stop("You need to remix and reshuffle the cards first.")
    }
  }
))

# R6 makes this easy because I can change the cards inplace via self reference

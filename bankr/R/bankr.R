#' R6 class providing an account
#'
#' An account has a value.
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_number
Account <- R6Class("Account", public = list(
  #' @field value Account balance.
  value = NULL,
  #' @description
  #' Create a new bank account.
  #' @return A new "Account" object.
  initialize = function() {
    self$value <- 0
  },
  #' @description
  #' Print the value.
  #' @return Value.
  print = function() {
    cat("Value: \n", self$value, "\n")
  },
  #' @description
  #' Deposit money.
  #' @param value How much money.
  deposit = function(value) {
    checkmate::assert_number(value, lower = 0, finite = TRUE)
    self$value <- self$value + value
  },
  #' @description
  #' Withdraw money.
  #' @param value how much money.
  withdraw = function(value) {
    checkmate::assert_number(value, lower = 0, finite = TRUE)
    self$value <- self$value - value
  })
)

#' R6 class providing a giro account
#'
#' A giro account has a value and a limit.
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_number
GiroAccount <- R6Class("GiroAccount", public = list(
  #' @field limit GiroAccount limit.
  limit = NULL,
  #' @description
  #' Create a new giro account.
  #' @param limit overdraw limit.
  #' @return A new "GiroAccount" object.
  initialize = function(limit) {
    checkmate::assert_number(limit, upper = 0, finite = TRUE)
    self$limit <- limit
    self$value <- 0
  },
  #' @description
  #' Withdraw money.
  #' @param value how much money.
  withdraw = function(value) {
    checkmate::assert_number(value, lower = 0, finite = TRUE)
    if ((self$value - value) < self$limit) {
      stop("Limit reached.")
    }
    self$value <- self$value - value
    if (self$value < 0) {
      self$value <- self$value * 1.05 # 5 percent fees
    }
  }),
  inherit = Account
)

#' R6 class providing a safe account
#'
#' A safe account has a value.
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_number
SafeAccount <- R6Class("SafeAccount", private = list(
  .value = NULL
  ), active = list(
  #' @field deposit active binding to value.
  deposit = function(value) {
    if (missing(value)) {
      private$.value
    } else {
      checkmate::assert_number(value, lower = 0, finite = TRUE)
      private$.value <- private$.value + value
    }
  },
  #' @field withdraw active binding to value.
  withdraw = function(value) {
    if (missing(value)) {
      private$.value
    } else {
      checkmate::assert_number(value, lower = 0, finite = TRUE)
      private$.value <- private$.value - value
    }
  }), public = list(
  #' @description
  #' Create a new bank account.
  #' @return A new "SafeAccount" object.
  initialize = function() {
    private$.value <- 0
  },
  #' @description
  #' Print the value.
  #' @return Value.
  print = function() {
    cat("Value: \n", private$.value, "\n")
  })
)

#' R6 class providing a transaction log
#'
#' A transaction log has a log.
#' @importFrom R6 R6Class
TransactionLog <- R6Class("TransactionLog", private = list(
  .log = NULL
  ), active = list(
  #' @field logging active binding to log.
  logging = function(value) {
    if (missing(value)) {
      private$.log
    } else {
      assert_character(value)
      private$.log <- c(private$.log, value)
    }
  }), public = list(
  #' @description
  #' Print the log.
  #' @return Log.
  print = function() {
    cat("Log: \n", paste(private$.log, collapse = "\n "), "\n")
  })
)

#' R6 class providing an account with a log
#'
#' An account log has a value and a transaction log.
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_number
Account_log <- R6Class("Account_log", public = list(
  #' @field value Account_log balance.
  value = NULL,
  #' @field TransactionLog transaction log.
  TransactionLog = NULL,
  #' @description
  #' Create a new bank account with a log.
  #' @return A new "Account_log" object.
  initialize = function() {
    self$value <- 0
    self$TransactionLog <- TransactionLog$new()
  },
  #' @description
  #' Print the value.
  #' @return Value.
  print = function() {
    cat("Value: \n", self$value, "\n")
    print(self$TransactionLog)
  },
  #' @description
  #' Deposit money.
  #' @param value How much money.
  deposit = function(value) {
    checkmate::assert_number(value, lower = 0, finite = TRUE)
    self$value <- self$value + value
    self$TransactionLog$logging <- paste0(Sys.time(), " Deposit: ", value)
  },
  #' @description
  #' Withdraw money.
  #' @param value how much money.
  withdraw = function(value) {
    checkmate::assert_number(value, lower = 0, finite = TRUE)
    self$value <- self$value - value
    self$TransactionLog$logging <- paste0(Sys.time(), " Withdraw: ", value)
  })
)

# TransactionLog must be deep cloned
formals(Account_log$clone_method)$deep = TRUE # does this have any effect?
formals(Account_log$public_methods$clone)$deep = TRUE

test_that("Account", {
  test <- Account$new()
  expect_equal(test$value, 0)
  expect_error(test$deposit(-1))
  expect_error(test$withdraw(-1))
  test$deposit(100)
  test$withdraw(10)
  expect_equal(test$value, 90)
  print(test)
})

test_that("GiroAccount", {
  test <- GiroAccount$new(limit = -500)
  expect_equal(test$value, 0)
  expect_equal(test$limit, -500)
  expect_error(test$withdraw(-1))
  expect_error(test$withdraw(501))
  test$withdraw(500)
  expect_equal(test$value, -525)
  print(test)
})

test_that("SafeAccount", {
  test <- SafeAccount$new()
  expect_equal(test$deposit, 0)
  expect_equal(test$withdraw, 0)
  expect_error((test$deposit <- -1))
  expect_error((test$withdraw <- -1))
  test$deposit <- 100
  test$withdraw <- 10
  expect_equal(test$deposit, 90)
  expect_equal(test$withdraw, 90)
  print(test)
})

test_that("TransactionLog", {
  test <- TransactionLog$new()
  expect_equal(test$logging, NULL)
  expect_error((test$logging <- 0))
  test$logging <- "Hello World"
  expect_equal(test$logging, "Hello World")
})

test_that("AccountLog", {
  test <- Account_log$new()
  expect_equal(test$value, 0)
  expect_equal(test$TransactionLog$logging, NULL)
  expect_error(test$deposit(-1))
  expect_error(test$withdraw(-1))
  test$deposit(100)
  test$withdraw(10)
  expect_equal(test$value, 90)
  expect_character(test$TransactionLog$logging)
  old_log <- test$TransactionLog$logging
  cloned <- test$clone()
  test$deposit(100)
  expect_equal(old_log, cloned$TransactionLog$logging)
  print(test)
})

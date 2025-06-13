library(testthat)

source(here::here("Rutils", "Rutils.R"))

test_that("subscribes returns 0 when paywall >= end", {
  expect_equal(subscribes(10, 10, 0.5), 0)
  expect_equal(subscribes(10, 11, 0.5), 0)
})

test_that("subscribes returns 0 or 1 when paywall < end", {
  result <- subscribes(10, 5, 0.5)
  expect_true(result %in% c(0, 1))
})

test_that("subscribes respects p_subscribes = 0", {
  expect_equal(subscribes(10, 5, 0), 0)
})

test_that("subscribes respects p_subscribes = 1", {
  expect_equal(subscribes(10, 5, 1), 1)
})


test_that("create_subscription_decision returns a function", {
  decider <- create_subscription_decision(0.5)
  expect_type(decider, "closure")
})

test_that("returned function gives 0 when paywall >= end", {
  decider <- create_subscription_decision(0.5)
  result <- decider(user_paywall = c(10, 5), user_ends = c(5, 5))
  expect_equal(result, c(0, 0))
})

test_that("returned function respects p_subscribes = 1", {
  decider <- create_subscription_decision(1)
  result <- decider(user_paywall = c(1, 2), user_ends = c(5, 5))
  expect_equal(result, c(1, 1))
})

test_that("returned function respects p_subscribes = 0", {
  decider <- create_subscription_decision(0)
  result <- decider(user_paywall = c(1, 2), user_ends = c(5, 5))
  expect_equal(result, c(0, 0))
})

test_that("returned function outputs correct length", {
  decider <- create_subscription_decision(0.5)
  result <- decider(user_paywall = c(1, 2, 3), user_ends = c(5, 5, 5))
  expect_length(result, 3)
  expect_true(all(result %in% c(0,1)))
})

test_that("day_sim returns a data frame with correct columns and n rows", {
  fake_sub <- function(x, y) rep(TRUE, length(x))  # everyone subscribes
  result <- day_sim(5, 10, 20, 0, "A", fake_sub)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_true(all(c("user_starts", "user_leaves", "user_subscribes", "grouping") %in% names(result)))
})

test_that("user_starts equals today", {
  fake_sub <- function(x, y) rep(TRUE, length(x))
  result <- day_sim(3, 10, 20, 100, "B", fake_sub)
  
  expect_true(all(result$user_starts == 100))
})

test_that("user_leaves are >= today", {
  fake_sub <- function(x, y) rep(TRUE, length(x))
  result <- day_sim(10, 10, 20, 0, "C", fake_sub)
  
  expect_true(all(result$user_leaves >= 0))
})

test_that("user_subscribes is NA if sub returns FALSE", {
  fake_sub <- function(x, y) rep(FALSE, length(x))
  result <- day_sim(4, 10, 20, 0, "D", fake_sub)
  
  expect_true(all(is.na(result$user_subscribes)))
})

test_that("grouping column is correctly assigned", {
  fake_sub <- function(x, y) rep(TRUE, length(x))
  result <- day_sim(2, 10, 20, 0, "GroupX", fake_sub)
  
  expect_true(all(result$grouping == "GroupX"))
})

test_that("ramp produces correct length output", {
  result <- ramp(5, 10, 20)
  expect_length(result, 5)
})

test_that("ramp starts at floored n_start and ends at floored n_end", {
  result <- ramp(4, 10, 20)
  expect_equal(result[1], floor(10))
  expect_equal(result[4], floor(20))
})

test_that("ramp values are floored", {
  result <- ramp(3, 0, 5)
  expect_equal(result, floor(seq(0, 5, length.out = 3)))
})

test_that("ramp handles n_days = 1", {
  result <- ramp(1, 7, 10)
  expect_equal(result, floor(7))
})

test_that("ramp handles n_start > n_end", {
  result <- ramp(3, 10, 5)
  expect_equal(result, floor(seq(10, 5, length.out = 3)))
})

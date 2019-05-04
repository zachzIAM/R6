context("basic-ops")

# define classes ----------------------------------------------------------
no_overrides <- R6::R6Class(
  classname = "no_overrides",
  public = list(
    x = NULL,
    initialize = function(x) {
      self$x = x
      private$.y = rnorm(1)
    }
  ),
  private = list(
    .y = NULL
  ),
  active = list(
    y = function() {
      private$.y
    }
  )
)

public_overrides <- R6::R6Class(
  classname = "public_overrides",
  inherit = no_overrides,
  public = list(
    initialize = function(x, y) {
      self$x = x
      private$.y = y
    },
    '+' = function(e2) {
      x = self$x + e2$x
      y = self$y + e2$y
      public_overrides$new(x, y)
    },
    '-' = function(e2) {
      x = self$x - e2$x
      y = self$y - e2$y
      public_overrides$new(x, y)
    },
    '*' = function(e2) {
      x = self$x * e2$x
      y = self$y * e2$y
      public_overrides$new(x, y)
    },
    '/' = function(e2) {
      x = self$x / e2$x
      y = self$y / e2$y
      public_overrides$new(x, y)
    },
    '^' = function(e2) {
      x = self$x ^ e2$x
      y = self$y ^ e2$y
      public_overrides$new(x, y)
    }
  )
)

private_overrides <- R6::R6Class(
  classname = "private_overrides",
  inherit = no_overrides,
  public = list(
    initialize = function(x, y) {
      self$x = x
      private$.y = y
    }
  ),
  private = list(
    '+' = function(e2) {
      x = self$x + e2$x
      y = self$y + e2$y
      private_overrides$new(x, y)
    },
    '-' = function(e2) {
      x = self$x - e2$x
      y = self$y - e2$y
      private_overrides$new(x, y)
    },
    '*' = function(e2) {
      x = self$x * e2$x
      y = self$y * e2$y
      private_overrides$new(x, y)
    },
    '/' = function(e2) {
      x = self$x / e2$x
      y = self$y / e2$y
      private_overrides$new(x, y)
    },
    '^' = function(e2) {
      x = self$x ^ e2$x
      y = self$y ^ e2$y
      private_overrides$new(x, y)
    }
  )
)

# tests -------------------------------------------------------------------
test_that("error when overrides not implemented", {
  test_none <- no_overrides$new(5)
  test_public <- public_overrides$new(10, rnorm(1))
  test_private <- private_overrides$new(15, rnorm(1))

  expect_error(test_none + test_none, regexp = "No.*method defined for R6 class 'no_overrides'")
  expect_error(test_none - test_none, regexp = "No.*method defined for R6 class 'no_overrides'")
  expect_error(test_none * test_none, regexp = "No.*method defined for R6 class 'no_overrides'")
  expect_error(test_none / test_none, regexp = "No.*method defined for R6 class 'no_overrides'")
  expect_error(test_none ^ test_none, regexp = "No.*method defined for R6 class 'no_overrides'")

  expect_error(test_public + test_public, regexp = "No.*method defined for R6 class 'public_overrides'")
  expect_error(test_public - test_public, regexp = "No.*method defined for R6 class 'public_overrides'")
  expect_error(test_public * test_public, regexp = "No.*method defined for R6 class 'public_overrides'")
  expect_error(test_public / test_public, regexp = "No.*method defined for R6 class 'public_overrides'")
  expect_error(test_public ^ test_public, regexp = "No.*method defined for R6 class 'public_overrides'")

  expect_error(test_private + test_private, regexp = NA)
  expect_error(test_private - test_private, regexp = NA)
  expect_error(test_private * test_private, regexp = NA)
  expect_error(test_private / test_private, regexp = NA)
  expect_error(test_private ^ test_private, regexp = NA)

})

test_that("new class instances are returned by operations", {
  x <- 15
  y <- rnorm(1, 10, 0.5)
  test_private <- private_overrides$new(x, y)
  test_add <- test_private + test_private
  test_sub <- test_private - test_private
  test_mult <- test_private * test_private
  test_div <- test_private / test_private
  test_pow <- test_private ^ test_private

  # a class instance is returned
  expect_is(test_add, class = "private_overrides")
  expect_is(test_sub, class = "private_overrides")
  expect_is(test_mult, class = "private_overrides")
  expect_is(test_div, class = "private_overrides")
  expect_is(test_pow, class = "private_overrides")

  # new class instance variables are correct
  ## test_add
  expect_equal(test_add$x, x + x)
  expect_equal(test_add$y, y + y)

  ## test_sub
  expect_equal(test_sub$x, 0)
  expect_equal(test_sub$y, 0)

  ## test_mult
  expect_equal(test_mult$x, x * x)
  expect_equal(test_mult$y, y * y)

  ## test_div
  expect_equal(test_div$x, 1)
  expect_equal(test_div$y, 1)

  ## test_pow
  expect_equal(test_pow$x, x ^ x)
  expect_equal(test_pow$y, y ^ y)
})
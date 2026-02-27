test_that("round_tidy_half_up rounds half-up (not banker's rounding)", {
  expect_equal(round_tidy_half_up(2.5, 0), "3")
  expect_equal(round_tidy_half_up(0.5, 0), "1")
  expect_equal(round_tidy_half_up(1.5, 0), "2")
  expect_equal(round_tidy_half_up(3.5, 0), "4")
})

test_that("round_tidy_half_up keeps trailing zeros", {
  expect_equal(round_tidy_half_up(1.1, 3),  "1.100")
  expect_equal(round_tidy_half_up(2.0, 2),  "2.00")
  expect_equal(round_tidy_half_up(0.0, 2),  "0.00")
})

test_that("round_tidy_half_up handles negatives correctly", {
  expect_equal(round_tidy_half_up(-2.5, 0), "-3")
  expect_equal(round_tidy_half_up(-1.1, 2), "-1.10")
})

test_that("round_tidy_half_up suppresses -0.00 artifact", {
  expect_equal(round_tidy_half_up(-0.001, 2), "0.00")
  expect_equal(round_tidy_half_up(-0.004, 2), "0.00")
})

test_that("round_tidy_half_up passes through NA as NA_character_", {
  result <- round_tidy_half_up(NA_real_, 2)
  expect_true(is.na(result))
  expect_type(result, "character")
})

test_that("round_tidy_half_up returns character vector", {
  result <- round_tidy_half_up(c(1.1, 2.2, 3.3), 2)
  expect_type(result, "character")
  expect_length(result, 3)
})

test_that("round_tidy_half_up handles a vector with NAs", {
  result <- round_tidy_half_up(c(1.5, NA, 2.5), 0)
  expect_equal(result[1], "2")
  expect_true(is.na(result[2]))
  expect_equal(result[3], "3")
})

test_that("round_tidy_half_up handles NULL by returning empty character()", {
  expect_equal(round_tidy_half_up(NULL, 2), character())
})

test_that("p_coding returns '<.001' for very small p", {
  expect_equal(p_coding(0.0001), "<.001")
  expect_equal(p_coding(0),      "<.001")
})

test_that("p_coding returns '>.999' for very large p", {
  expect_equal(p_coding(0.9999), ">.999")
  expect_equal(p_coding(1),      ">.999")
})

test_that("p_coding formats a normal p-value numerically", {
  result <- p_coding(0.045)
  expect_type(result, "character")
  # Should not be a threshold string
  expect_false(result %in% c("<.001", ">.999"))
})

test_that("p_coding boundary: exactly 0.001 is NOT '<.001'", {
  expect_false(p_coding(0.001) == "<.001")
})

test_that("p_coding boundary: exactly 0.999 is NOT '>.999'", {
  expect_false(p_coding(0.999) == ">.999")
})

test_that("p_coding returns NA_character_ for NA input", {
  result <- p_coding(NA_real_)
  expect_true(is.na(result))
  expect_type(result, "character")
})

test_that("p_coding respects digits argument", {
  # digits = 2: 0.046 rounds to 0.05 (half-up); result should have 2 decimal places
  result2 <- p_coding(0.046, digits = 2)
  expect_equal(result2, "0.05")
  # digits = 3 (default): 0.046 stays 0.046
  result3 <- p_coding(0.046, digits = 3)
  expect_equal(result3, "0.046")
})

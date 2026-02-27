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
  # digits = 2 should round 0.045 to "0.05" (or "0.04" depending on half-up)
  result2 <- p_coding(0.046, digits = 2)
  expect_equal(nchar(gsub("[^0-9]", "", result2)), 2L)
})

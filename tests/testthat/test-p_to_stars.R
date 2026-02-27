# .p_to_stars is not exported, so we call it via :::
test_that(".p_to_stars returns '***' for p < 0.001", {
  expect_equal(as.character(sostieteidentaulukointityylit:::.p_to_stars(0.0005)), "***")
})

test_that(".p_to_stars returns '**' for 0.001 <= p < 0.01", {
  expect_equal(as.character(sostieteidentaulukointityylit:::.p_to_stars(0.005)), "**")
})

test_that(".p_to_stars returns '*' for 0.01 <= p < 0.05", {
  expect_equal(as.character(sostieteidentaulukointityylit:::.p_to_stars(0.03)), "*")
})

test_that(".p_to_stars returns '' for p > 0.05", {
  expect_equal(as.character(sostieteidentaulukointityylit:::.p_to_stars(0.1)),   "")
  expect_equal(as.character(sostieteidentaulukointityylit:::.p_to_stars(0.051)), "")
  # boundary: cut() uses (0.01, 0.05] so exactly 0.05 maps to "*"
  expect_equal(as.character(sostieteidentaulukointityylit:::.p_to_stars(0.05)), "*")
})

test_that(".p_to_stars is vectorised", {
  result <- as.character(sostieteidentaulukointityylit:::.p_to_stars(c(0.0001, 0.005, 0.03, 0.1)))
  expect_equal(result, c("***", "**", "*", ""))
})

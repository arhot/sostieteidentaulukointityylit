test_that("tee_r2_yhteenveto returns a string", {
  out <- tee_r2_yhteenveto(list(lm_simple))
  expect_type(out, "character")
  expect_length(out, 1L)
})

test_that("tee_r2_yhteenveto single model: contains 'R2 korj.' but no delta", {
  out <- tee_r2_yhteenveto(list(lm_simple))
  expect_true(grepl("R2 korj\\.", out))
  expect_false(grepl("\u0394R2", out))  # no delta R2
})

test_that("tee_r2_yhteenveto two models: contains delta R2 and p value", {
  out <- tee_r2_yhteenveto(list(lm_simple, lm_multi))
  expect_true(grepl("\u0394R2", out))  # ΔR2
  expect_true(grepl("p = ", out))
})

test_that("tee_r2_yhteenveto three models: mentions three steps", {
  out <- tee_r2_yhteenveto(list(lm_simple, lm_multi, lm_full))
  expect_true(grepl("askeleella 1", out))
  expect_true(grepl("askeleella 2", out))
  expect_true(grepl("askeleella 3", out))
})

test_that("tee_r2_yhteenveto R2 values formatted without leading zero", {
  out <- tee_r2_yhteenveto(list(lm_simple))
  # Should be ".xxx" style, not "0.xxx"
  expect_false(grepl("= 0\\.", out))
})

test_that("tee_r2_yhteenveto stops on empty list", {
  expect_error(tee_r2_yhteenveto(list()), "ainakin yksi")
})

test_that("tee_r2_yhteenveto stops on non-lm element", {
  expect_error(tee_r2_yhteenveto(list(lm_simple, glm_logit)),
               "'lm'-objekteja")
})

test_that("tee_r2_yhteenveto digits and p_digits args accepted", {
  expect_no_error(tee_r2_yhteenveto(list(lm_simple, lm_multi), digits = 2, p_digits = 2))
})

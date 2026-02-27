# helper-fixtures.R
# Loaded automatically by testthat before any test file.
# Provides small synthetic objects so no external dataset is needed.

set.seed(42)

# ---- basic synthetic data frame ----
df_synth <- data.frame(
  x1  = rnorm(100),
  x2  = rnorm(100),
  x3  = rnorm(100),
  x4  = rnorm(100),
  y   = rnorm(100),
  grp = sample(0L:1L, 100, replace = TRUE)
)

# Add a factor and a character variable for luokiteltu tests
df_synth$cat <- factor(sample(c("A", "B", "C"), 100, replace = TRUE))

# Introduce a few NAs to test NA handling
df_synth$x1[c(3, 7, 15)] <- NA

# ---- pre-built models ----
lm_simple  <- lm(y ~ x2,       data = df_synth)
lm_multi   <- lm(y ~ x2 + x3,  data = df_synth)
lm_full    <- lm(y ~ x2 * x3,  data = df_synth)
glm_logit  <- glm(grp ~ x2 + x3, data = df_synth, family = binomial)

# ---- correlation objects ----
corr_vars  <- df_synth[, c("x2", "x3", "x4")]
corr_obj   <- psych::corr.test(corr_vars, use = "pairwise", adjust = "none")

# ---- factor analysis ----
# 4 items loaded on 1 latent factor + noise
fa_data <- data.frame(
  q1 = rnorm(120) + 0.8 * rnorm(120),
  q2 = rnorm(120) + 0.8 * rnorm(120),
  q3 = rnorm(120) + 0.8 * rnorm(120),
  q4 = rnorm(120) + 0.8 * rnorm(120)
)
fa_model <- psych::principal(fa_data, nfactors = 1, rotate = "none")

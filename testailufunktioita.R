# ========= Helpers to quickly test each exported function =========

# 1) FACTOR ANALYSIS TABLE ------------------------------------------------------
test_faktorianalyysi <- function(d = ESS2016) {
  # EDIT: choose ~4–8 numeric items (Likert, 0–10 etc.), no missing codes
  vars <- c("h1a", "h1b", "h1c", "h1d", "h1e")
  dd   <- d[vars]
  dd   <- stats::na.omit(dd)

  faktorimalli <- psych::principal(dd, nfactors = 2, rotate = "oblimin")
  tee_faktorianalyysitaulukko(faktorimalli, kieli = "suomi", desimaalierotin = "piste", piilota_lataukset_alle = 0.3)
}

test_faktorianalyysi()

# 2) CORRELATION TABLE (upper triangle) ----------------------------------------
test_korrelaatiotaulukko <- function(d = ESS2016) {
  vars <- c("h1a", "h1b", "h1c", "h1d", "h1e")
  dd   <- d[vars]
  # Pairwise complete for corr.test
  ct <- psych::corr.test(dd, use = "pairwise", adjust = "none")
  tee_korrelaatiotaulukko(ct)
}

test_korrelaatiotaulukko()

test_korrelaatiotaulukko()
# 3) CORR TABLE WITH DESCRIPTIVES (n, ka, kh) ----------------------------------
test_korrelaatiotaulukko_kuvaileva <- function(d = ESS2016) {
  vars <- c("h1a", "h1b", "h1c", "h1d", "h1e")
  dd   <- d[vars]
  ct <- psych::corr.test(dd, use = "pairwise", adjust = "none")
  tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin(ct, d)
}

test_korrelaatiotaulukko_kuvaileva()

# 4) LINEAR REGRESSION TABLE -------------s---------------------------------------
test_regressio <- function(d = ESS2016) {
  # EDIT: set dependent + predictors; ensure numeric where needed
  fit <- lm(c1 ~ as_factor(f2_1) * f3_1, data = d)
  tee_regressiotaulukko(fit)
}

test_regressio()

# 5) LINEAR REGRESSION TABLE (dependent as a row) -------------------------------
test_regressio_selitettava_rivina <- function(d = ESS2016) {
  fit <- lm(c1 ~ as_factor(f2_1) + f3_1, data = d)
  tee_regressiotaulukko_selitettava_rivina(fit)
}

test_regressio_selitettava_rivina()

# 6) DESCRIPTIVES FOR CONTINUOUS VARIABLES -------------------------------------
test_jatkuvan_muuttujan_esittely <- function(d = ESS2016) {
  # EDIT: pick continuous vars; set lv/vinous as needed
  tee_jatkuvan_muuttujan_esittely(d, c1, f3_1, lv = TRUE, vinous = TRUE)
}

test_jatkuvan_muuttujan_esittely()

# 7) LOGISTIC REGRESSION TABLE --------------------------------------------------
test_log_regressio <- function(d = ESS2016) {
  # EDIT: binary outcome must be 0/1 (or a 2-level factor)
  fit <- glm(as_factor(f2_1) ~ c1 * f3_1, family = binomial, data = d)
  tee_log_regressiotaulukko(fit, luottamustaso = 0.95, pyorista_est = 2, pyorista_p = 3)
}

test_log_regressio()

m1 <- lm(c1 ~ f2_1, data = ESS2016)
m2 <- lm(c1 ~ f2_1 + f3_1, data = ESS2016)
m3 <- lm(c1 ~ f2_1 * f3_1, data = ESS2016)

tee_r2_yhteenveto(list(m1, m2, m3))
# "Huom. R2 korj. = .007 askeleella 1., R2 korj. = .018 askeleella 2., R2 korj. = .081 askeleella 3.;
#   R2 = .018 askeleella 2 (p = .112), ΔR2 = .069 askeleella 3 (p = .002)"


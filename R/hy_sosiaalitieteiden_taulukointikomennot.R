#' @import dplyr
#' @import psych
#' @import forcats
#' @import broom
#' @import tidyr
#' @import lm.beta
#' @import purrr
#' @importFrom stringr str_replace_all
#' @importFrom rlang quos enquos ensyms
#' @encoding UTF-8


#' @title Faktorianalyysin tulostaulukko
#'
#' @description Funktio tekee psych-paketin faktorianalyysimallista HY:n sosiaalitieteiden koulutusohjelmien ohjeiden mukaisen tulostaulukon.
#' @param faktorimalli faktorianalyysimalli
#' @param kieli Jos suomi, kääntää käsitteet suomeksi, muuten jättää englanninkieliset.
#' @param desimaalierotin Jos pilkku, muuttaa valmiin taulukon desimaalierottimen pilkuksi, muuten jättää alkuperäisen pisteen.
#' @param piilota_lataukset_alle Poistaa taulukosta argumentin itseisarvoa pienemmät lataukset.
#' @return Valmis faktorianalyysitaulukko.
#' @export

tee_faktorianalyysitaulukko <- function(faktorimalli, kieli="suomi", desimaalierotin="piste", piilota_lataukset_alle = 0.3) {
  if (!inherits(faktorimalli, "fa") && !inherits(faktorimalli, "principal")) {stop("Ensimmäisen argumentin tulee olla faktorianalyysin tulosobjekti.")}

  taulukko1 <-
    faktorimalli$loadings %>%
    psych::fa.sort() %>%
    unclass() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Muuttuja") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ dplyr::if_else(
          abs(.x) < piilota_lataukset_alle,
          "",                                    # hide small loadings
          round_tidy_half_up(.x, 2)              # show with half-up + trailing zeros
        )
      )
    )
  comm_vec <- if (!is.null(faktorimalli$h2)) {
    faktorimalli$h2
  } else if (!is.null(faktorimalli$communality)) {
    faktorimalli$communality
  } else {
    L <- faktorimalli$loadings %>% unclass() %>% as.matrix()
    rowSums(L^2)
  }

  comm_name <- if (kieli == "suomi") "Kommunaliteetti" else "Communality"
  comm_df <- tibble::tibble(
    Muuttuja = rownames(faktorimalli$loadings),
    "{comm_name}" := round_tidy_half_up(comm_vec, 2) %>% format(nsmall = 2)
  )

  # Add communalities as a new column
  taulukko1 <- taulukko1 %>% dplyr::left_join(comm_df, by = "Muuttuja")
  taulukko2 <- faktorimalli$Vaccounted %>% as_tibble(rownames = "Muuttuja") %>%
    mutate(across(tidyselect::vars_select_helpers$where(is.numeric), ~round_tidy_half_up(.x, 2) %>% format(nsmall=2))) %>%
    filter(Muuttuja == "SS loadings" | Muuttuja == "Proportion Var")

  if(kieli=="suomi"){
    taulukko2 <- taulukko2 %>% mutate(Muuttuja = fct_recode(Muuttuja, "Ominaisarvo" = "SS loadings", "Selitysosuus %" = "Proportion Var"))
  }

  valmis_taulukko <- taulukko1 %>% bind_rows(taulukko2)

  if(desimaalierotin=="pilkku"){
    valmis_taulukko <- valmis_taulukko %>% mutate(across(everything(), ~str_replace_all(.x, "\\.", ",")))
  }

  valmis_taulukko
}


#' @title Korrelaatiotaulukko
#'
#' @description Tekee yläkolmion korrelaatiotaulukon, jossa korrelaatiot on pyöristetty ja
#' p-arvot esitetty merkitsevyystähdillä.
#' @param korrelaatiomatriisi psych::corr.test()-objekti tai lista, jossa komponentit r ja p.
#' @return Tibble: Muuttuja + yläkolmion korrelaatiot tähdillä.
#' @export
tee_korrelaatiotaulukko <- function(korrelaatiomatriisi) {
  # validate
  if (is.null(korrelaatiomatriisi) || is.null(korrelaatiomatriisi$r) || is.null(korrelaatiomatriisi$p)) {
    stop("Ensimmäisen argumentin tulee olla psych::corr.test()-tulos (tai lista, jossa on komponentit r ja p).")
  }

  r_mat <- as.matrix(korrelaatiomatriisi$r)
  p_mat <- as.matrix(korrelaatiomatriisi$p)

  # build upper triangle with stars
  out <- .format_corr_upper(r_mat, p_mat, digits = 2)

  return(out)
}


#' @title Korrelaatiotaulukko kuvailevin tunnusluvuin
#'
#' @description Tekee yläkolmion korrelaatiotaulukon ja lisää muuttujakohtaiset kuvailevat tunnusluvut:
#' n, keskiarvo (ka) ja keskihajonta (kh).
#' @param korrelaatiomatriisi psych::corr.test()-objekti tai lista, jossa komponentit r ja p.
#' @param aineisto data.frame, josta lasketaan n, ka ja kh. Sen tulee sisältää samat muuttujat kuin korrelaatiomatriisissa.
#' @return Tibble: Muuttuja, n, ka, kh + yläkolmion korrelaatiot tähdillä.
#' @export
tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin <- function(korrelaatiomatriisi, aineisto) {
  # validate
  if (is.null(korrelaatiomatriisi) || is.null(korrelaatiomatriisi$r) || is.null(korrelaatiomatriisi$p)) {
    stop("Ensimmäisen argumentin tulee olla psych::corr.test()-tulos (tai lista, jossa on komponentit r ja p).")
  }
  if (!is.data.frame(aineisto)) {
    stop("Toisen argumentin tulee olla aineisto (data.frame).")
  }

  r_mat <- as.matrix(korrelaatiomatriisi$r)
  p_mat <- as.matrix(korrelaatiomatriisi$p)
  vars  <- colnames(r_mat)

  # descriptive stats for listed variables, preserving order
  stats_df <- aineisto |>
    dplyr::summarise(dplyr::across(
      .cols = dplyr::all_of(vars),
      .fns = list(
        n  = ~sum(!is.na(.x)),
        ka = ~mean(.x, na.rm = TRUE),
        kh = ~stats::sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}.{.fn}"
    )) |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("Muuttuja", ".value"),
      names_sep = "\\."
    ) |>
    dplyr::mutate(
      ka = janitor::round_half_up(ka, 2),
      kh = janitor::round_half_up(kh, 2),
      Muuttuja = factor(Muuttuja, levels = vars)
    ) |>
    dplyr::arrange(Muuttuja) |>
    dplyr::mutate(Muuttuja = as.character(Muuttuja))

  corr_tbl <- .format_corr_upper(r_mat, p_mat, digits = 2)

  out <- dplyr::left_join(stats_df, corr_tbl, by = "Muuttuja")
  return(out)
}



#' @title Regressioanalyysin tulostaulukko
#'
#' @description Funktio tekee lineaarisesta regressioanalyysimallista HY:n sosiaalitieteiden koulutusohjelmien ohjeiden mukaisen tulostaulukon.
#' @param malli Lineaarinen regressioanalyysimalli.
#' @return Valmis regressioanalyysitaulukko.
#' @export

tee_regressiotaulukko <- function(malli) {
  if (!inherits(malli, "lm")) {
    stop("Ensimmäisen argumentin tulee olla regressioanalyysin tulosobjekti.")
  }

  taulukko <- broom::tidy(malli, conf.int = TRUE) %>%
    tibble::add_column(beta = lm.beta::lm.beta(malli)$standardized.coefficients) %>%
    dplyr::select(Muuttuja = term, B = estimate, LV_alaraja = conf.low, LV_ylaraja = conf.high, beta, p = p.value) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 3))) %>%
    dplyr::mutate(
      p = sapply(p, p_coding),
      Luottamusvali = paste("[", LV_alaraja, ",", LV_ylaraja, "]"),
      B = round_tidy_half_up(B, 3)
    ) %>% dplyr::select(Muuttuja, B, beta, Luottamusvali, p)  %>%
    dplyr::filter(Muuttuja != "(Intercept)")

  selitysosuus <- summary(malli)$adj.r.squared %>%
    round_tidy_half_up(3) %>%                              # character
    tibble::as_tibble(rownames = "Muuttuja") %>%
    dplyr::rename(B = value) %>%
    dplyr::mutate(Muuttuja = "R2, korjattu")

  dplyr::bind_rows(taulukko, selitysosuus)
}


#' @title Regressioanalyysin tulostaulukko, selitettävä rivinä.
#'
#' @description Funktio tekee lineaarisesta regressioanalyysimallista HY:n sosiaalitieteiden koulutusohjelmien ohjeiden mukaisen tulostaulukon, joss
#' selitettävä muuttuja rivinä.
#' @param malli Lineaarinen regressioanalyysimalli.
#' @return Valmis regressioanalyysitaulukko.
#' @export

tee_regressiotaulukko_selitettava_rivina <- function(malli) {
  if (!inherits(malli, "lm")) {
    stop("Ensimmäisen argumentin tulee olla regressioanalyysin tulosobjekti.")
  }

  selitettava <- all.vars(stats::terms(malli))[1]

  taulukko <- broom::tidy(malli, conf.int = TRUE) %>%
    tibble::add_column(beta = lm.beta::lm.beta(malli)$standardized.coefficients) %>%
    dplyr::mutate(
      LV_alaraja = round(conf.low, 3),
      LV_ylaraja = round(conf.high, 3),
      beta       = round(beta, 3),
      p          = sapply(p.value, p_coding),
      B          = round_tidy_half_up(estimate, 3),
      Luottamusvali = paste0("[", LV_alaraja, ", ", LV_ylaraja, "]")
    ) %>%
    dplyr::select(Muuttuja = term, B, Luottamusvali, beta, p) %>%
    dplyr::mutate(Selitettava = selitettava) %>%
    dplyr::filter(Muuttuja != "(Intercept)")

  selitysosuus <- tibble::tibble(
    Selitettava  = selitettava,
    Muuttuja     = "R2, korjattu",
    B            = round_tidy_half_up(summary(malli)$adj.r.squared, 3),
    Luottamusvali = NA_character_,
    beta         = NA_real_,
    p            = NA_character_
  )

  dplyr::bind_rows(taulukko, selitysosuus)
}


#' Esittele jatkuvia muuttujia
#'
#' Laskee valituille muuttujille havaintomäärän \code{n}, keskiarvon (\code{ka})
#' ja keskihajonnan (\code{kh}). Valinnaisesti lisää 95 %:n luottamusvälin
#' keskiarvolle sekä vinouden (\code{vi}) ja huipukkuuden (\code{hu}).
#'
#' @param aineisto Data frame, josta muuttujat lasketaan.
#' @param ... Valittavat muuttujat (tukee tidyselect-apureita kuten \code{c()},
#'   \code{starts_with()}, \code{var1:var5}).
#' @param lv Logical. Jos \code{TRUE}, lisää 95\% luottamusvälit keskiarvolle.
#'   Oletus \code{FALSE}.
#' @param vinous Logical. Jos \code{TRUE}, lisää vinouden (\code{vi}) ja
#'   huipukkuuden (\code{hu}). Oletus \code{FALSE}.
#'
#' @return Tibble, jossa sarakkeet: \code{Muuttuja}, \code{n}, \code{ka},
#'   \code{kh}, sekä valinnaisesti \code{lv_alaraja}, \code{lv_ylaraja},
#'   \code{vi}, \code{hu}.
#' @export


#' @importFrom rlang quos
tee_jatkuvan_muuttujan_esittely <- function(aineisto, ..., lv = FALSE, vinous = FALSE) {

  taulukko <- aineisto %>%
    dplyr::summarise(dplyr::across(
      .cols = c(!!!rlang::quos(...)),
      .fns  = list(
        n  = ~sum(!is.na(.x)),
        ka = ~mean(.x, na.rm = TRUE),
        kh = ~stats::sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}.{.fn}"
    )) %>%
    tidyr::pivot_longer(
      cols = dplyr::contains("."),
      names_sep = "\\.",
      names_to  = c("Muuttuja", ".value")
    )
  if (isTRUE(lv)) {
    taulukko <- taulukko %>%
      dplyr::mutate(
        lv_alaraja = ka - 1.96 * kh / sqrt(n),
        lv_ylaraja = ka + 1.96 * kh / sqrt(n)
      )
  }

  if (isTRUE(vinous)) {
    kurtosis <- function(x) {
      m4 <- mean((x - mean(x, na.rm = TRUE))^4, na.rm = TRUE)
      m4 / (stats::sd(x, na.rm = TRUE)^4) - 3
    }
    skewness <- function(x) {
      m3 <- mean((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE)
      m3 / (stats::sd(x, na.rm = TRUE)^3)
    }

    taulukko_vinous <- aineisto %>%
      dplyr::summarise(dplyr::across(
        .cols = c(!!!rlang::quos(...)),
        .fns  = list(
          vi = ~skewness(.x),
          hu = ~kurtosis(.x)
        ),
        .names = "{.col}.{.fn}"
      )) %>%
      tidyr::pivot_longer(
        cols = dplyr::contains("."),
        names_sep = "\\.",
        names_to  = c("Muuttuja", ".value")
      )

    taulukko <- dplyr::left_join(taulukko, taulukko_vinous, by = "Muuttuja")
  }

  # 4) FORMAT for display (half-up + trailing zeros) — convert selected cols to character
  if (exists("round_tidy_half_up")) {
    fmt_cols <- intersect(c("ka", "kh", "lv_alaraja", "lv_ylaraja"), names(taulukko))
    for (cc in fmt_cols) {
      taulukko[[cc]] <- round_tidy_half_up(taulukko[[cc]], digits = 3)
    }
  }

  taulukko
}

#' @title Tehdään logistisen regression taulukko.
#'
#' @description Funktio tekee logisesta regressiomallista HY:n sosiaalitieteiden ohjeen mukaisen tulostaulukon
#' @param malli Logistinen regressioanalyysimalli.
#' @param luottamustaso Luottamusvälin taso (oletus 0.95).
#' @param pyorista_est Desimaalit estimaateille, SE:lle ja z:lle (oletus 2).
#' @param pyorista_p Desimaalit p-arvoille (oletus 3; käytetään \code{p_coding}).
#' @return Valmis esittelytaulukko.
#' @export

tee_log_regressiotaulukko <- function(malli, luottamustaso = 0.95, pyorista_est = 2, pyorista_p = 3) {
  if (!inherits(malli, "glm") || is.null(malli$family) || malli$family$family != "binomial") {
    stop("Ensimmäisen argumentin tulee olla logistisen regressioanalyysin (glm, family = binomial) tulosobjekti.")
  }

  malli_summary <- broom::tidy(malli, conf.int = TRUE, conf.level = luottamustaso) %>%
    mutate(
      Muuttuja   = as.character(term),
      SE          = round_tidy_half_up(std.error, pyorista_est),
      z           = round_tidy_half_up(statistic, pyorista_est),
      p           = sapply(p.value, p_coding, digits = pyorista_p),
      Log_odds_LL = round_tidy_half_up(conf.low,  pyorista_est),
      Log_odds_UL = round_tidy_half_up(conf.high, pyorista_est),
      OR          = round_tidy_half_up(exp(estimate),   pyorista_est),
      `OR LL`     = round_tidy_half_up(exp(conf.low),   pyorista_est),
      `OR UL`     = round_tidy_half_up(exp(conf.high),  pyorista_est),
      `Log odds CI` = paste0("[", Log_odds_LL, ", ", Log_odds_UL, "]"),
      `OR CI`       = paste0("[", `OR LL`, ", ", `OR UL`, "]")
    ) %>%
    select(Muuttuja, Vetosuhde = OR, `95% Luottamusvali` = `OR CI`, p) %>%
    dplyr::filter(Muuttuja != "(Intercept)")

  return(malli_summary)
}

#' Yhteenvetoteksti: R^2, korjattu R^2 ja ΔR^2 merkitsevyys hierarkkisille lm-malleille
#'
#' @param mallit Lista peräkkäin sisäkkäisiä lm-malleja (askel 1, 2, 3, ...).
#' @param digits Desimaalit R^2-lukuihin (oletus 3).
#' @param p_digits Desimaalit p-arvoihin (oletus 3, käytetään p_coding()).
#' @return Merkkijono, esim:
#'   "Huom. R2 korj. = .007 askeleella 1., R2 korj. = .018 askeleella 2., R2 korj. = .081 askeleella 3.;
#'    R2 = .018 askeleella 2 (p = .112), ΔR2 = .069 askeleella 3 (p = .002)"
#' @export

tee_r2_yhteenveto <- function(mallit, digits = 3, p_digits = 3) {
  # Salli myös syöte c(model1, model2, ...) ilman list()-käärettä
  if (!is.list(mallit) || inherits(mallit, "lm")) {
    mallit <- list(mallit)
  }

  if (length(mallit) < 1L) stop("Anna ainakin yksi lm-malli.")
  if (!all(vapply(mallit, function(m) inherits(m, "lm"), logical(1)))) {
    stop("Kaikkien 'mallit' -listan alkioiden tulee olla 'lm'-objekteja.")
  }

  # Apumuotoilijat -------------------------------------------------------------
  # .018-tyyliin ilman etunollaa, half-up + trailing zeros
  fmt_num <- function(x, d = digits) {
    s <- round_tidy_half_up(x, d)           # "0.018"
    s <- sub("^(-?)0\\.", "\\1.", s)        # ".018" / "-.018"
    s
  }
  # p-arvo: p_coding + sama etunollan poisto (jos tarpeen)
  fmt_p <- function(p) {
    s <- p_coding(p, digits = p_digits)     # "<.001", ">.999" tai "0.112"
    s <- sub("^0\\.", ".", s)               # ".112"
    s
  }

  # Korjatut R^2 jokaiselle askeleelle ----------------------------------------
  adjR2 <- vapply(mallit, function(m) summary(m)$adj.r.squared, numeric(1))
  adj_txt <- vapply(seq_along(adjR2), function(i) {
    paste0("R2 korj. = ", fmt_num(adjR2[i]), " askeleella ", i, ".")
  }, character(1))
  adj_txt <- paste(adj_txt, collapse = ", ")

  # ΔR^2 ja sen p jokaiselle siirtymälle (2..k) --------------------------------
  # (käytetään ANOVA-vertailua peräkkäisten mallien välillä)
  if (length(mallit) >= 2L) {
    R2 <- vapply(mallit, function(m) summary(m)$r.squared, numeric(1))
    deltas <- R2[-1] - R2[-length(R2)]

    pvals <- vapply(seq_len(length(mallit) - 1L), function(i) {
      p <- NA_real_
      # Yritä ANOVA-vertailua; jos ei sisäkkäisiä, jätä NA
      out <- try(stats::anova(mallit[[i]], mallit[[i + 1]]), silent = TRUE)
      if (!inherits(out, "try-error")) {
        # 2. rivi sisältää uuden mallin lisäyksen testin
        p <- suppressWarnings(out[["Pr(>F)"]][2])
        if (is.null(p) || length(p) == 0L) p <- NA_real_
      }
      p
    }, numeric(1))

    # Kirjoitustapa: ensimmäinen muutos "R2 = ... askeleella 2 (p = ...)",
    # seuraavat "ΔR2 = ... askeleella i (p = ...)"
    change_txt <- vapply(seq_along(deltas), function(j) {
      lab <- "ΔR2"
      paste0(lab, " = ", fmt_num(deltas[j]),
             " askeleella ", j + 1,
             " (p = ", fmt_p(pvals[j]), ")")
    }, character(1))
    change_txt <- paste(change_txt, collapse = ", ")
    out <- paste0("Huom. ", adj_txt, "; ", change_txt)
  } else {
    out <- paste0("Huom. ", adj_txt)
  }

  out
}

#' Format p-values consistently
#' @param pval Numeric p-value.
#' @param digits Rounding digits when shown numerically.
#' @return Character.
#' @keywords internal

p_coding <- function(pval, digits = 3) {
  if (is.na(pval)) return(NA_character_)
  if (pval < .001)  return("<.001")
  if (pval > .999)  return(">.999")
  round_tidy_half_up(pval, digits)
}


# ---- small internal helper (not exported) ----
#' @keywords internal
.p_to_stars <- function(p) {
  cut(p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
      labels = c("***", "**", "*", ""))
}

#' @keywords internal
.format_corr_upper <- function(r_mat, p_mat, digits = 2) {
  stopifnot(is.matrix(r_mat), is.matrix(p_mat))
  stopifnot(identical(dim(r_mat), dim(p_mat)))
  stopifnot(identical(colnames(r_mat), colnames(p_mat)))

  r_rounded <- janitor::round_half_up(r_mat, digits)
  r_fmt <- formatC(r_rounded, format = "f", digits = digits)
  disp <- matrix(
    paste0(r_fmt, as.character(.p_to_stars(p_mat))),
    nrow = nrow(r_mat), ncol = ncol(r_mat),
    dimnames = dimnames(r_mat)
  )


  # keep only upper triangle; hide diagonal
  disp[lower.tri(disp, diag = TRUE)] <- NA

  # drop last row and first column to avoid empty/dup cells
  disp_trim <- disp[-nrow(disp), -1, drop = FALSE]

  # to tibble with Muuttuja from rownames
  tibble::as_tibble(disp_trim, rownames = "Muuttuja")
}

#' Half-up rounding with tidy trailing zeros
#' @param x numeric
#' @param digits integer number of decimals
#' @return character vector with fixed decimals
#' @keywords internal
round_tidy_half_up <- function(x, digits = 3) {
  if (is.null(x)) return(character())
  # numeric rounding (half-up)
  v <- janitor::round_half_up(as.numeric(x), digits = digits)
  # avoid "-0.00" artifacts
  tol <- 10^(-digits)
  v[abs(v) < tol] <- 0
  # format with fixed decimals (trailing zeros)
  out <- formatC(v, format = "f", digits = digits)
  # preserve NA as NA_character_
  out[is.na(x)] <- NA_character_
  out
}


#' @title Yhdistelmätaulukko useista regressiotaulukoista
#'
#' @description Yhdistää useita \code{tee_regressiotaulukko()}-funktion tuottamia
#' taulukoita yhdeksi taulukoksi. Jokaisen osataulukon eteen lisätään otsikkorivi,
#' jossa \code{Muuttuja}-sarakkeessa lukee "Malli 1", "Malli 2" jne. ja muut
#' sarakkeet ovat tyhjiä.
#'
#' @param ... Regressiotaulukot (data framet), jotka yhdistetään.
#' @return Yhdistetty taulukko otsikkoriveineen.
#' @export

tee_yhdistelmataulukko <- function(...) {
  taulukot <- list(...)
  if (length(taulukot) == 0) stop("Anna vähintään yksi taulukko.")

  yhdistelty <- purrr::imap(taulukot, function(taulukko, i) {
    otsikkorivi <- taulukko[1, ] %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ NA)) %>%
      dplyr::mutate(Muuttuja = paste0("Malli ", i))
    dplyr::bind_rows(otsikkorivi, taulukko)
  }) %>%
    purrr::reduce(dplyr::bind_rows)

  yhdistelty
}


utils::globalVariables(c(
  # common columns
  "Muuttuja","ka","kh","n","lv_alaraja","lv_ylaraja","vi","hu",
  # broom columns you use
  "term","estimate","conf.low","conf.high","p.value","std.error","statistic",
  # your computed/display columns
  "Luottamusvali","Log_odds_LL","Log_odds_UL","OR","OR CI","OR LL","OR UL",
  "B","Selitettava","value"
))


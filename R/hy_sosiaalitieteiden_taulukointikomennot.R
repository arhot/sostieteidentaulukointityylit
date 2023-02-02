#' @import dplyr
#' @import psych
#' @import forcats
#' @import janitor
#' @import broom
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
  if (class(faktorimalli)[1] != "psych") {stop("Ensimmäisen argumentin tulee olla faktorianalyysin tulosobjekti.")}

  taulukko1 <- faktorimalli$loadings  %>% fa.sort() %>% unclass() %>% as_tibble(rownames ="Muuttuja") %>%
    mutate(across(tidyselect::vars_select_helpers$where(is.numeric), ~case_when(abs(.x) < piilota_lataukset_alle  ~ "", TRUE ~ round_half_up(.x, 2) %>%
                                                  format(nsmall=2)))) %>%
    left_join(faktorimalli$communalities %>% as_tibble(rownames="Muuttuja") %>%
                mutate(Kommunaliteetti = round_half_up(value,2), .keep="unused"))

  taulukko2 <- faktorimalli$Vaccounted %>% as_tibble(rownames = "Muuttuja") %>%
    mutate(across(tidyselect::vars_select_helpers$where(is.numeric), ~round_half_up(.x, 2) %>% format(nsmall=2))) %>%
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
#' @description Funktio tekee korrelaatiomatriisista HY:n sosiaalitieteiden koulutusohjelmien ohjeiden mukaisen tulostaulukon.
#' @param korrelaatiomatriisi Korrelaatiomatriisi
#' @return Valmis korrelaatiotaulukko.
#' @export
#'
tee_korrelaatiotaulukko <- function(korrelaatiomatriisi) {
  if (class(korrelaatiomatriisi)[2] != "corr.test") {stop("Ensimmäisen argumentin tulee olla korrelaatiomatriisi.")}

  korrelaatio_arvot <- korrelaatiot$r %>%  as_tibble() %>% mutate_if(is.numeric, round, 2) %>% as.matrix()
  korrelaatioiden_p_arvot <- korrelaatiot$p %>%  as_tibble() %>%  mutate_if(is.numeric,
                                                                            cut, breaks=c(-0.1,0.001,0.01,0.05,1),
                                                                            labels=c("***", "**", "*", "")) %>% as.matrix()
  korrelaatiotaulukko <- matrix(paste(korrelaatio_arvot,korrelaatioiden_p_arvot, sep=""), dim(korrelaatio_arvot)[1], dim(korrelaatio_arvot)[1])
  korrelaatiotaulukko[lower.tri(korrelaatiotaulukko,diag=TRUE)] <- NA
  colnames(korrelaatiotaulukko) <- colnames(korrelaatiot$r)
  rownames(korrelaatiotaulukko) <- colnames(korrelaatiot$r)
  korrelaatiotaulukko <- korrelaatiotaulukko[-nrow(korrelaatiotaulukko),-1] %>% as_tibble(rownames = "Muuttuja")
  korrelaatiotaulukko
}

tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin <- function(korrelaatiomatriisi, aineisto) {
  if (class(korrelaatiomatriisi)[2] != "corr.test") {stop("Ensimmäisen argumentin tulee olla korrelaatiomatriisi.")}
  if (is(aineisto, "data.frame")) {stop("Toisen argumentin tulee olla aineisto.")}

  korrelaatio_arvot <- korrelaatiot$r %>%  as_tibble() %>% mutate_if(is.numeric, round, 2) %>% as.matrix()
  korrelaatioiden_p_arvot <- korrelaatiot$p %>%  as_tibble() %>%  mutate_if(is.numeric,
                                                                            cut, breaks=c(-0.1,0.001,0.01,0.05,1),
                                                                            labels=c("***", "**", "*", "")) %>% as.matrix()
  korrelaatiotaulukko <- matrix(paste(korrelaatio_arvot,korrelaatioiden_p_arvot, sep=""), dim(korrelaatio_arvot)[1], dim(korrelaatio_arvot)[1])
  korrelaatiotaulukko[lower.tri(korrelaatiotaulukko,diag=TRUE)] <- NA
  colnames(korrelaatiotaulukko) <- colnames(korrelaatiot$r)
  rownames(korrelaatiotaulukko) <- colnames(korrelaatiot$r)
  korrelaatiotaulukko <- korrelaatiotaulukko[-nrow(korrelaatiotaulukko),-1] %>% as_tibble(rownames = "Muuttuja")


}

#' @title Regressioanalyysin tulostaulukko
#'
#' @description Funktio tekee lineaarisesta regressioanalyysimallista HY:n sosiaalitieteiden koulutusohjelmien ohjeiden mukaisen tulostaulukon.
#' @param malli Lineaarinen regressioanalyysimalli.
#' @return Valmis regressioanalyysitaulukko.
#' @export

tee_regressiotaulukko<- function(malli) {
  if (class(malli)[1] != "lm") {stop("Ensimmäisen argumentin tulee olla regressioanalyysin tulosobjekti.")}
  taulukko <- tidy(malli, conf.int=T) %>% tibble::add_column(beta = lm.beta::lm.beta(malli)$standardized.coefficients) %>%
    select(Muuttuja=term, B=estimate, LV_alaraja=conf.low, LV_ylaraja=conf.high, beta, p=p.value)  %>%
    mutate_if(is.numeric, round, 3) %>% mutate(p= cut(p, breaks=c(-0.1,0.001,0.01,0.05,1),
                                                      labels=c("***", "**", "*", ""))) %>%
    mutate(Luottamusvali = paste("[", LV_alaraja, ",", LV_ylaraja, "]"), .after=B) %>% select(-LV_alaraja, -LV_ylaraja) %>% filter(Muuttuja!= "(Intercept)")
  selitysosuus <- summary(malli)$adj.r.squared %>% round_half_up(3) %>% as_tibble(rownames="Muuttuja") %>% rename(B=value) %>%
    mutate(Muuttuja = "R2, korjattu")

  bind_rows(taulukko, selitysosuus)
}


#' @title Regressioanalyysin tulostaulukko, selitettävä rivinä.
#'
#' @description Funktio tekee lineaarisesta regressioanalyysimallista HY:n sosiaalitieteiden koulutusohjelmien ohjeiden mukaisen tulostaulukon, joss
#' selitettävä muuttuja rivinä.
#' @param malli Lineaarinen regressioanalyysimalli.
#' @return Valmis regressioanalyysitaulukko.
#' @export

tee_regressiotaulukko_selitettava_rivina <- function(malli) {
  if (class(malli)[1] != "lm") {stop("Ensimmäisen argumentin tulee olla regressioanalyysin tulosobjekti.")}
  selitettava <- all.vars(stats::terms(malli))[1]
  taulukko <- tidy(malli, conf.int=T) %>% tibble::add_column(beta = lm.beta::lm.beta(malli)$standardized.coefficients) %>%
    tibble::add_column(Selitettava = selitettava) %>%
    select(Selitettava, Muuttuja=term, B=estimate, LV_alaraja=conf.low, LV_ylaraja=conf.high, beta, p=p.value)  %>%
    mutate_if(is.numeric, round, 3) %>% mutate(p= cut(p, breaks=c(-0.1,0.001,0.01,0.05,1),
                                                      labels=c("***", "**", "*", ""))) %>%
    mutate(Luottamusvali = paste("[", LV_alaraja, ",", LV_ylaraja, "]"), .after=B) %>% select(-LV_alaraja, -LV_ylaraja) %>% filter(Muuttuja!= "(Intercept)")
  selitysosuus <- summary(malli)$adj.r.squared %>% round_half_up(3) %>% as_tibble(rownames="Muuttuja") %>% rename(B=value) %>%
    mutate(Muuttuja = "R2, korjattu")

  bind_rows(taulukko, selitysosuus)
}

#' @title Esitellään jatkuva muuttuja.
#'
#' @description Funktio tekee jatkuvasta muuttujasta HY:n sosiaalitieteiden ohjeen mukaisen esittelytaulukon.
#' @param aineisto Aineistotiedosto.
#' @return Valmis esittelytaulukko.
#' @export

tee_jatkuvan_muuttujan_esittely <- function(aineisto, ..., lv=F, vinous=F){
  taulukko <- aineisto %>%
    summarise(across(c(!!!quos(...)), list(
      n = ~sum(!is.na(.x)),
      ka = ~mean(.x, na.rm=T) %>% round_half_up(3),
      kh = ~sd(.x, na.rm=T) %>% round_half_up(3)), .names = "{.col}.{.fn}")) %>%
    tidyr::pivot_longer(cols = contains("."), names_sep = "\\.", names_to  = c("Muuttuja", ".value"))
  if(lv==T) {
    taulukko <- taulukko %>% mutate(lv_alaraja = ka-1.96*kh/sqrt(n),
                                    lv_ylaraja = ka+1.96*kh/sqrt(n))
  }
  if(vinous==T){
    kurtosis <- function(x) {
      m4 <- mean((x - mean(x, na.rm=T))^4, na.rm=T)
      kurtosis <- m4/(sd(x, na.rm=T)^4) - 3
      kurtosis
    }

    skewness <-  function(x) {
      m3 <- mean((x - mean(x, na.rm=T))^3, na.rm=T)
      skewness <- m3/(sd(x, na.rm=T)^3)
      skewness
    }

    taulukko_vinous <- aineisto %>%
      summarise(across(c(!!!quos(...)), list(
        vi = ~skewness(.x),
        hu = ~kurtosis(.x)), .names = "{.col}.{.fn}")) %>%
      tidyr::pivot_longer(cols = contains("."), names_sep = "\\.", names_to  = c("Muuttuja", ".value"))
    taulukko <- left_join(taulukko, taulukko_vinous)
  }

  taulukko
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("value", "Muuttuja", "korrelaatiot", "term", "estimate",
                                                        "conf.low", "conf.high", "p.value", "p", "LV_alaraja", "LV_ylaraja",
                                                        "B", "Selitettava", "term"))


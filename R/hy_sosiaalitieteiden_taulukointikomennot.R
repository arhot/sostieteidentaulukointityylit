taulukoi_faktorianalyysi <- function(faktorimalli, kieli="suomi", desimaalierotin="piste", piilota_lataukset_alle = 0.3) {
  
  taulukko1 <- faktorimalli$loadings  %>% fa.sort() %>% unclass() %>% as_tibble(rownames ="Muuttuja") %>% 
    mutate(across(where(is.numeric), ~case_when(abs(.x) < piilota_lataukset_alle  ~ "", TRUE ~ round_half_up(.x, 2) %>% 
                                                  format(nsmall=2)))) %>% 
    left_join(faktorimalli$communalities %>% as_tibble(rownames="Muuttuja") %>%
                mutate(Kommunaliteetti = round_half_up(value,2), .keep="unused"))
  
  taulukko2 <- faktorimalli$Vaccounted %>% as_tibble(rownames = "Muuttuja") %>% 
    mutate(across(where(is.numeric), ~round_half_up(.x, 2) %>% format(nsmall=2))) %>% 
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

tee_korrelaatiotaulukko <- function(korrelaatiomatriisi) {
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

tee_regressiotaulukko<- function(malli) {
  taulukko <- tidy(malli, conf.int=T) %>% add_column(beta = lm.beta(malli)$standardized.coefficients) %>% 
    select(Muuttuja=term, B=estimate, LV_alaraja=conf.low, LV_ylaraja=conf.high, beta, p=p.value)  %>% 
    mutate_if(is.numeric, round, 3) %>% mutate(p= cut(p, breaks=c(-0.1,0.001,0.01,0.05,1), 
                                                      labels=c("***", "**", "*", ""))) %>% 
    mutate(Luottamusvali = paste("[", LV_alaraja, ",", LV_ylaraja, "]"), .after=B) %>% select(-LV_alaraja, -LV_ylaraja) %>% filter(Muuttuja!= "(Intercept)")
  selitysosuus <- summary(malli)$adj.r.squared %>% round_half_up(3) %>% as_tibble(rownames="Muuttuja") %>% rename(B=value) %>%
    mutate(Muuttuja = "R^2^, korjattu")
  
  bind_rows(taulukko, selitysosuus)
}

tee_regressiotaulukko_selitettava_rivina <- function(malli) {
  selitettava <- all.vars(terms(malli))[1]
  taulukko <- tidy(malli, conf.int=T) %>% add_column(beta = lm.beta(malli)$standardized.coefficients) %>% 
    add_column(Selitettava = selitettava) %>% 
    select(Selitettava, Muuttuja=term, B=estimate, LV_alaraja=conf.low, LV_ylaraja=conf.high, beta, p=p.value)  %>% 
    mutate_if(is.numeric, round, 3) %>% mutate(p= cut(p, breaks=c(-0.1,0.001,0.01,0.05,1), 
                                                      labels=c("***", "**", "*", ""))) %>% 
    mutate(Luottamusvali = paste("[", LV_alaraja, ",", LV_ylaraja, "]"), .after=B) %>% select(-LV_alaraja, -LV_ylaraja) %>% filter(Muuttuja!= "(Intercept)")
  selitysosuus <- summary(malli)$adj.r.squared %>% round_half_up(3) %>% as_tibble(rownames="Muuttuja") %>% rename(B=value) %>%
    mutate(Muuttuja = "R^2^, korjattu")
  
  bind_rows(taulukko, selitysosuus)
}


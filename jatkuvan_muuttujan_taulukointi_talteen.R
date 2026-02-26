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
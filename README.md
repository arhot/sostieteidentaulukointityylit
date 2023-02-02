
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sostieteidentaulukointityylit

<!-- badges: start -->
<!-- badges: end -->

Paketti tekee tavallisimmista tilastoanalyysien tulosobjekteista
Helsingin yliopiston sosiaalitieteiden koulutusohjelmien mukaisia
tulostaulukoita. Tyyli noudattelee pääpiirteissään APA 7th -ohjeita,
muutamia suomen kieleen liittyviä poikkeuksia lukuunottamatta.

Versio 0.1, testaillaan. Tällä hetkellä paketissa on seuraavat funktiot:

- tee_faktorianalyysitaulukko() - psych-paketin faktorianalyyseille

- tee_korrelaatiotaulukko() - korrelaatiomatriisille

- tee_regressiotaulukko() - lm-regressiomalleille

- tee_regressiotaulukko_selitettava_rivina() - lm-regressiomalleille
  (eri selitettävien yhdistämiseen samaan taulukkoon)

## asennus

Asenna suorittamalla seuraavat koodi. Jos et ole asentanut
devtools-pakettia, asenna se ensin.

``` r
# install.packages("devtools")
devtools::install_github("arhot/sostieteidentaulukointityylit")
```

------------------------------------------------------------------------

Ylläpito Arho Toikka <arho.toikka@helsinki.fi>

# Provenance for the Zika application (vignette "applications", paper Sec 3.3).
#
# Source: Amorim, L. (2022). Replication Data for: Zika Epidemic and Birth
# Rates in Brazil. Harvard Dataverse. https://doi.org/10.7910/DVN/ENG0IY
#
# The data are not redistributed with this package. Download the replication
# archive from the DOI above, then adapt the code below to produce a data frame
# with one row per municipality and the columns used in the vignette.
#
# Design, following Taddeo et al. (2022) and Tchetgen et al. (2023):
#   treated group  Pernambuco       (high Zika incidence in 2015)
#   control group  Rio Grande do Sul (zero reported cases)
#   outcome        municipality birth rate per 1,000 people, 2016
#   placebo        the same measure in 2014 (pre-treatment)

## raw <- read.csv("path/to/downloaded/birth_rates.csv")
##
## zika <- data.frame(
##   municipality    = raw$municipality,
##   treated         = as.integer(raw$state == "Pernambuco"),
##   birth_rate_2016 = raw$birth_rate_2016,
##   birth_rate_2014 = raw$birth_rate_2014
## )
## zika <- zika[stats::complete.cases(zika), ]
## saveRDS(zika, "zika.rds")

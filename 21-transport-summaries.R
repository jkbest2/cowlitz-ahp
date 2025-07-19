library(tidyverse)

source(here::here("01-identify-origin.R"))
source(here::here("02-destinations.R"))
source(here::here("03-transport-functions.R"))

ahp <- read_rds(here::here("data", "ahp_df.rds"))

sprchk_tr <- summarize_transport("Spring Chinook", ahp)
plot_ahp_tr(sprchk_tr)

fallchk_tr <- summarize_transport("Fall Chinook", ahp)
plot_ahp_tr(fallchk_tr)

coho_tr <- summarize_transport("Coho", ahp)
plot_ahp_tr(coho_tr)

winsth_tr <- summarize_transport("Stlhd Winter", ahp)
plot_ahp_tr(winsth_tr)

sumsth_tr <- summarize_transport("Stlhd Summer", ahp)
plot_ahp_tr(sumsth_tr)

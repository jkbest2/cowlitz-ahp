library(tidyverse)

source(here::here("01-identify-origin.R"))
source(here::here("02-destinations.R"))
source(here::here("03-transport-functions.R"))

ahp <- read_rds(here::here("data", "ahp_df.rds"))

sprchk_tr <- summarize_transport("Spring Chinook", ahp)
plot_ahp_tr(sprchk_tr) +
  ggtitle("Spring Chinook")
ggsave(here::here("figs/sprchk_transport.png"), width = 12, height = 8)

fallchk_tr <- summarize_transport("Fall Chinook", ahp)
plot_ahp_tr(fallchk_tr) +
  ggtitle("Fall Chinook")
ggsave(here::here("figs/fallchk_transport.png"), width = 12, height = 8)

coho_tr <- summarize_transport("Coho", ahp)
plot_ahp_tr(coho_tr) +
  ggtitle("Coho")
ggsave(here::here("figs/coho_transport.png"), width = 12, height = 8)

winsth_tr <- summarize_transport("Stlhd Winter", ahp)
plot_ahp_tr(winsth_tr) +
  ggtitle("Winter Steelhead")
ggsave(here::here("figs/winsth_transport.png"), width = 12, height = 8)

sumsth_tr <- summarize_transport("Stlhd Summer", ahp)
plot_ahp_tr(sumsth_tr) +
  ggtitle("Summer Steelhead")
ggsave(here::here("figs/sumsth_transport.png"), width = 12, height = 8)

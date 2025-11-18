library(tidyverse)

source(here::here("01-identify-origin.R"))
source(here::here("02-destinations.R"))
source(here::here("03-transport-functions.R"))

ahp <- read_rds(here::here("data", "ahp_df.rds"))

## Spring Chinook --------------------------------------------------------------
sprchk_bh <- summarize_byhaul("Spring Chinook", ahp) |>
  filter_ahp_bh(
    destination %in%
      c(
        "Bremer Bridge",
        "Cispus",
        "Lower River",
        "Packwood",
        "Riffe Lake",
        "Scanewa",
        "Tilton"
      )
  )

plot_ahp_bh(sprchk_bh)
## Adult HORs
sprchk_bh |>
  filter_ahp_bh(
    lifestage == "Adult",
    origin == "Hatchery (HOR)"
  ) |>
  plot_ahp_bh()

## Fall Chinook ----------------------------------------------------------------
fallchk_bh <- summarize_byhaul("Fall Chinook", ahp) |>
  filter_ahp_bh(
    destination %in%
      c(
        "Bremer Bridge",
        "Cispus",
        "Lower River",
        "Packwood",
        "Riffe Lake",
        "Scanewa",
        "Tilton"
      )
  )

plot_ahp_bh(fallchk_bh)
## Adult HORs
fallchk_bh |>
  filter_ahp_bh(
    lifestage == "Adult",
    origin == "Hatchery (HOR)"
  ) |>
  plot_ahp_bh()

## Coho ------------------------------------------------------------------------
coho_bh <- summarize_byhaul("Coho", ahp) |>
  filter_ahp_bh(
    destination %in%
      c(
        "Bremer Bridge",
        "Cispus",
        "Lower River",
        "Packwood",
        "Riffe Lake",
        "Scanewa",
        "Tilton"
      )
  )

plot_ahp_bh(coho_bh)
## Adult HORs
coho_bh |>
  filter_ahp_bh(
    lifestage == "Adult",
    origin == "Hatchery (HOR)"
  ) |>
  plot_ahp_bh()

## Winter Steelhead ------------------------------------------------------------
sthw_bh <- summarize_byhaul("Stlhd Winter", ahp) |>
  filter_ahp_bh(
    destination %in%
      c(
        "Bremer Bridge",
        "Cispus",
        "Lower River",
        "Packwood",
        "Riffe Lake",
        "Scanewa",
        "Tilton"
      )
  )

plot_ahp_bh(sthw_bh)
## Adult HORs
sthw_bh |>
  filter_ahp_bh(
    lifestage == "Adult",
    origin == "Integrated (H/W)"
  ) |>
  plot_ahp_bh()

## Summer Steelhead ------------------------------------------------------------
sths_bh <- summarize_byhaul("Stlhd Summer", ahp) |>
  filter_ahp_bh(
    destination %in%
      c(
        "Bremer Bridge",
        "Cispus",
        "Lower River",
        "Packwood",
        "Riffe Lake",
        "Scanewa",
        "Tilton"
      )
  )

plot_ahp_bh(sths_bh)
## Adult HORs
sths_bh |>
  filter_ahp_bh(
    lifestage == "Adult",
    origin == "Hatchery (HOR)"
  ) |>
  plot_ahp_bh()

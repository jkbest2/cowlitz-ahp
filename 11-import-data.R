library(tidyverse)
library(readxl)

ahp <- read_xlsx(
  here::here("data", "2024-12-06-ahp-record.xlsm"),
  sheet = "TankWeeklyHoldingQuery_FOREXCEL",
  col_types = c(
    "numeric",
    "numeric",
    "text",
    "text",
    "text",
    "date",
    "text",
    "numeric",
    "text",
    "text",
    "numeric",
    "text",
    "date",
    "numeric",
    "text",
    "numeric",
    "date"
  )
) |>
  rename_with(tolower) |>
  mutate(
    fishdescription = factor(fishdescription),
    sex = fct_collapse(factor(sex), Unknown = c("Unknown", "Unk")),
    lifestage = fct_recode(
      factor(lifestage),
      `Jack` = "Jacks",
      `Mini Jack` = "Mini Jacks"
    ),
    destination = factor(destination),
    hold_date = as.Date(hold_date),
    haul_date = as.Date(haul_date)
  ) |>
  select(
    hold_date,
    haul_date,
    species = fishdescription,
    sex,
    lifestage,
    tag,
    destination,
    count
  ) |>
  filter(
    species %in% c(
      "Coho", "Fall Chinook", "Spring Chinook", "Stlhd Summer", "Stlhd Winter"
    )
  ) |>
  mutate(
    dest_orig = destination,
    destination = fct_collapse(
      destination,
      `Bremer Bridge` = "Bremer Bridge",
      Broodstock = "Broodstock",
      Cispus = "Cispus - 2810 Site",
      Tilton = "Tilton Gust Backstrom Park",
      `Lower River` = c("Barrier Dam", "I-5 Bridge"),
      Mortality = c("Holding Pool Mort", "Separator Mort."),
      Packwood = "Franklin Bridge",
      `Riffe Lake` = "Mossyrock Park",
      Scanewa = c("Copper Canyon (Scanewa)", "Scanewa Day Use"),
      Surplus = c("SOMMA - Food Bank", "Surplus", "WDFW Given away"),
    ),
    transport = !destination %in% c("Broodstock", "Mortality", "Surplus")
  )
write_rds(ahp, here::here("data", "ahp_df.rds"))

library(tidyverse)

class_fchk_origin <- function(tag) {
  orgn <- switch(
    tag,
    "Ad only" = "Hatchery (HOR)",
    "AD + CWT" = "Integrated (H/W)",
    "UM" = ,
    "UM + BWT" = "Wild (NOR)"
  )
  factor(orgn, levels = c("Hatchery (HOR)", "Integrated (H/W)", "Wild (NOR)"))
}

fchk_ahp <- read_rds(here::here("data", "ahp_df.rds")) |>
  filter(fishdescription == "Fall Chinook") |>
  mutate(
    across(where(is.factor), fct_drop),
    destination = fct_relevel(
      destination,
      "Tilton Gust Backstrom Park", "Bremer Bridge", "Broodstock"
    ),
    origin = map_vec(fchk_ahp$tag, class_fchk_origin)
  )

fchk_goals <- tribble(
  ~ origin, ~ destination, ~ goal, ~ nobrood,
  "Hatchery (HOR)", "Tilton Gust Backstrom Park", "> 75%", TRUE,
  "Hatchery (HOR)", "Bremer Bridge", "< 25%", TRUE,
  "Hatchery (HOR)", "Broodstock", "Broodstock", FALSE,
  "Integrated (H/W)", "Broodstock", "Broodstock", FALSE,
  "Wild (NOR)", "Bremer Bridge", "100%", TRUE,
  "Wild (NOR)", "Tilton Gust Backstrom Park", "0%", TRUE,
  "Wild (NOR)", "Broodstock", "Collect at 1 in 3", FALSE
) |>
mutate(
  origin = factor(
    origin,
    levels = c("Hatchery (HOR)", "Integrated (H/W)", "Wild (NOR)")
  ),
  destination = factor(
    destination,
    levels = c("Tilton Gust Backstrom Park", "Bremer Bridge", "Broodstock")
  )
)

fallchk_df <- fchk_ahp |>
  summarize(
    n_fish = sum(total_fish_count),
    .by = c(origin, destination)
  ) |>
  arrange(origin) |>
  mutate(
    total_fish = sum(n_fish),
    .by = origin
  ) |>
  mutate(
    isbrood = destination == "Broodstock",
  ) |>
  mutate(
    n_fish_nobrood = sum(n_fish),
    .by = c(origin, isbrood)
  ) |>
  mutate(
    prop_fish = n_fish / total_fish,
    prop_fish_nobrood = n_fish / n_fish_nobrood
  ) |>
  select(origin, destination, prop_fish, prop_fish_nobrood) |>
  left_join(
    fchk_goals,
    by = join_by(origin, destination)
  ) |>
  mutate(
    realized = ifelse(nobrood, prop_fish_nobrood, prop_fish),
    realized = scales::percent(realized)
  ) |>
  select(origin, destination, realized, goal) |>
  arrange(origin, destination)

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
    lifestage = factor(lifestage),
    destination = factor(destination),
    hold_date = as.Date(hold_date),
    haul_date = as.Date(haul_date)
  ) |>
  select(
    hold_date,
    haul_date,
    fishdescription,
    sex,
    lifestage,
    tag,
    destination,
    total_fish_count) |>
  filter(
    fishdescription %in% c(
      "Coho", "Fall Chinook", "Spring Chinook", "Stlhd Summer", "Stlhd Winter"
    )
  ) |>
  mutate(
    brood = destination == "Broodstock",
    mort = grepl("mort", destination, ignore.case = TRUE),
    surplus = destination %in% c("SOMMA - Food Bank", "Surplus", "WDFW Given away"),
    transport = !brood & !surplus & !mort
  )
write_rds(ahp, here::here("data", "ahp_df.rds"))

ahp |>
  summarize(
    n = sum(total_fish_count, na.rm = TRUE),
    .by = c(fishdescription, lifestage, tag)
  ) |>
  ggplot(aes(x = fishdescription, fill = tag, y = n)) +
  geom_col(position = position_dodge())

ahp |>
  filter(grepl("OP RET", tag))


prob_rows <- ahp |>
  mutate(row = seq_along(haul_date) + 1) |>
  filter(
    is.na(haul_date) |
      is.na(hold_date) |
      is.na(fishdescription) |
      is.na(sex) |
      is.na(lifestage) |
      is.na(tag) |
      is.na(total_fish_count)
  )

prob_rows

ahp |>
  filter(
    fishdescription == "Spring Chinook"
  ) |>
  arrange(hold_date) |>
  summarize(
    tot = sum(total_fish_count, na.rm = TRUE),
    .by = c(hold_date, fishdescription, destination)
  ) |>
  mutate(
    ctot = cumsum(tot),
    .by = c(fishdescription, destination)
  ) |>
  ggplot(aes(x = hold_date, y = ctot, color = destination)) +
  geom_line()

ahp |>
  filter(
    !is.na(hold_date),
    !is.na(fishdescription),
    !is.na(destination),
    !is.na(total_fish_count)
  ) |>
  summarize(
    total = sum(total_fish_count),
    .by = c(hold_date, fishdescription, destination)
  ) |>
  arrange(hold_date) |>
  mutate(
    ctot = cumsum(total),
    .by = c(hold_date, fishdescription, destination)
  ) |>
  ggplot(aes(x = hold_date, y = ctot, color = destination)) +
  geom_line() +
  facet_wrap(~fishdescription, ncol = 1)

## Return tibble rows that can then be combined using `bind_rows` or
## `list_rbind` and NAs replaced with zeros.
ahp_goal <- function(fish, marks, lifestage, date) {
  if (fish == "Spring Chinook") {

  }
}

ahp_sprchk <- function(fish, marks, lifestage, date) {
  if (lifestage %in% c("Adult", "Jacks")) {
    if ()
  } else if (lifestage == "Mini Jacks") {
    if ()
  } else {
    stop("lifestage must be Adult, Jacks, or Mini Jacks")
  }
  goal
}

### Spring Chinook
## Transition from Scanewa releases is July 5 this year
ahp |>
  filter(
    fishdescription == "Spring Chinook",
    grepl("Scanewa", destination)
  ) |>
  summarize(
    mhold = max(hold_date),
    mhaul = max(haul_date),
    .by = destination
  )
  
ahp |>
  filter(
    fishdescription == "Spring Chinook"
  ) |>
  mutate(
    early_season = hold_date <= as.Date("2024-07-05")
  ) |>
  arrange(
    haul_date
  ) |>
  summarize(
    tot = sum(total_fish_count),
    .by = c(hold_date, lifestage, tag, destination, early_season)
  )

ahp |>
  filter(fishdescription == "Spring Chinook") |>
  mutate(
    brood = destination == "Broodstock",
    mort = grepl("mort", destination, ignore.case = TRUE)
    surplus = destination %in% c("SOMMA - Food Bank", "Surplus", "WDFW Given away")
  )
  


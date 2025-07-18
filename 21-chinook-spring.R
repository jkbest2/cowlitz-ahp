library(tidyverse)

source(here::here("01-identify-origin.R"))
source(here::here("02-destinations.R"))

ahp <- read_rds(here::here("data", "ahp_df.rds"))

sprchk_tr_df <- ahp |>
  filter(
    species == "Spring Chinook"
  ) |>
  mutate(
    origin = id_origin(species, tag),
    ahp_dest = sprchk_dest(tag, lifestage, hold_date),
    transport = only_transport(ahp_dest)
  ) |>
  select(
    lifestage, tag, origin, destination, ahp_dest, transport,
    count
  ) |>
  unnest_wider(transport) |>
  select(-ahp_dest) |>
  mutate(
    across(`Bremer Bridge`:Tilton, \(x) x * count)
  ) |>
  summarize(
    count = sum(count, na.rm = TRUE),
    across(`Bremer Bridge`:Tilton, \(x) sum(x, na.rm = TRUE)),
    .by = c(lifestage, tag, origin, destination)
  )

sprchk_tr <- sprchk_tr_df |>
  select(lifestage, tag, origin, destination, count) |>
  summarize(
    tr_count = sum(count, na.rm = TRUE),
    .by = c(lifestage, tag, origin, destination)
  )

sprchk_goal <- sprchk_tr_df |>
  select(lifestage, tag, origin, `Bremer Bridge`:Tilton) |>
  pivot_longer(
    `Bremer Bridge`:Tilton,
    names_to = "destination",
    values_to = "count"
  ) |>
  summarize(
    goal_count = sum(count, na.rm = TRUE),
    .by = c(lifestage, tag, origin, destination)
  )

sprchk <- sprchk_goal |>
  left_join(
    sprchk_tr,
    by = join_by(lifestage, tag, origin, destination)
  ) |>
  mutate(
    tr_count = replace_na(tr_count, 0),
    diff = tr_count - goal_count
  )

sprchk |>
  filter(tr_count > 0 | goal_count > 0) |>
  ggplot(
    aes(
      x = destination,
      y = tr_count, yend = goal_count,
      color = origin, shape = lifestage
    )
  ) +
  geom_segment(
    arrow = arrow(90, length = unit(0.1, "inches"), ends = "last"),
    position = position_dodge(width = 0.4)
  ) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_point(aes(y = goal_count), shape = "_", position = position_dodge(width = 0.4)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.025))) +
  facet_wrap(~tag, scales = "free") +
  labs(x = "Destination", y = "Count", color = "Origin", shape = "Life stage")

ggsave(here::here("figs", "sprchk_transport.png"), width = 12, height = 8)

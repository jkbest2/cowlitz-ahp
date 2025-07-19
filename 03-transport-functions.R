summarize_transport <- function(species, ahp) {
  dest_fn <- switch(species,
    "Spring Chinook" = dest_sprchk,
    "Fall Chinook" = dest_fallchk,
    "Coho" = dest_coho,
    "Stlhd Winter" = dest_winsth,
    "Stlhd Summer" = dest_sumsth
  )
  n_missing <- ahp |>
    ## Need to use injection (!!) here to avoid comparing the species column to
    ## itself
    filter(species == !!species) |>
    filter(is.na(species) | is.na(lifestage) | is.na(tag) | is.na(count)) |>
    nrow()
  ## Filter out desired species, get origin and AHP destination distributions
  ahp2 <- ahp |>
    filter(
      species == !!species,
      !is.na(lifestage),
      !is.na(tag),
      !is.na(count)
    ) |>
    mutate(
      origin = id_origin(species, tag),
      ahp_dest = dest_fn(tag, lifestage, hold_date),
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

  ## Summary of fish that were transported
  tr <- ahp2 |>
    select(lifestage, tag, origin, destination, count) |>
    summarize(
      tr_count = sum(count, na.rm = TRUE),
      .by = c(lifestage, tag, origin, destination)
    )

  goal <- ahp2 |>
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

  goal |>
    left_join(
      tr,
      by = join_by(lifestage, tag, origin, destination)
    ) |>
    mutate(
      tr_count = replace_na(tr_count, 0),
      diff = tr_count - goal_count
    )
}

plot_ahp_tr <- function(tr_df) {
  tr_df |>
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
}

ggsave(here::here("figs", "sprchk_transport.png"), width = 12, height = 8)

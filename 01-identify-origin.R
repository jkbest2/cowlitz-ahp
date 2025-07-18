origin_sprchk <- function(tag) {
  orgn <- case_match(
    tag,
    c(
      "AD + CWT", "AD + RV", "AD + RV + OP RET",
      "Ad only", "UM + BWT"
    ) ~ "Hatchery (HOR)",
    c("UM", "UM + PIT Tag", "OP RET") ~ "Wild (NOR)"
  )
  factor(orgn, levels = c("Hatchery (HOR)", "Integrated (H/W)", "Wild (NOR)"))
}

origin_fallchk <- function(tag) {
  orgn <- case_match(
    tag,
    "Ad only" ~ "Hatchery (HOR)",
    "AD + CWT" ~ "Integrated (H/W)",
    ## Assume UM + BWT corresponds with UM + CWT in goals spreadsheet
    c("UM", "UM + BWT") ~ "Wild (NOR)"
  )
  factor(orgn, levels = c("Hatchery (HOR)", "Integrated (H/W)", "Wild (NOR)"))
}

origin_coho <- function(tag) {
  orgn <- case_match(
    tag,
    "Ad only" ~ "Hatchery (HOR)",
    "AD + CWT" ~ "Integrated (H/W)",
    ## Assume UM + BWT corresponds with UM + CWT in goals spreadsheet, also
    ## assume that a PIT tag implies tagged in upper watershed and thus wild
    c("UM + BWT", "UM", "UM + PIT Tag") ~ "Wild (NOR)",
    .default = NA
  )
  factor(orgn, levels = c("Hatchery (HOR)", "Integrated (H/W)", "Wild (NOR)"))
}

origin_wintsth <- function(tag) {
  orgn <- case_match(
    tag,
    "Ad only" ~ "Hatchery (HOR)",
    c("AD + CWT", "AD + LV", "AD+LV+PIT") ~ "Integrated (H/W)",
    c(
      "UM", "UM + BWT", "UM + BWT + Pit",
      "UM + DOR + PIT", "UM + DORSAL CWT",
      "UM + PIT Tag", "UM + RV"
    ) ~ "Wild (NOR)"
  )
  factor(orgn, levels = c("Hatchery (HOR)", "Integrated (H/W)", "Wild (NOR)"))
}

origin_summsth <- function(tag) {
  orgn <- case_match(
    tag,
    c("Ad only", "AD + OP + PIT", "AD + PIT Tag") ~ "Hatchery (HOR)",
    c("UM", "OP RET") ~ "Wild (NOR)"
  )
  factor(orgn, levels = c("Hatchery (HOR)", "Integrated (H/W)", "Wild (NOR)"))
}

id_origin <- function(species, tag) {
  case_match(
    species,
    "Spring Chinook" ~ origin_sprchk(tag),
    "Fall Chinook" ~ origin_fallchk(tag),
    "Coho" ~ origin_coho(tag),
    "Stlhd Winter" ~ origin_wintsth(tag),
    "Stlhd Summer" ~ origin_summsth(tag)
  )
}

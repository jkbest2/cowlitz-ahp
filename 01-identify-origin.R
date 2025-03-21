origin_sprchk <- function(tag, lifestage) {
  if (lifestage == "Mini Jacks") {
    orgn <- "Hatchery (HOR)"
  } else {
    orgn <- ifelse(tag == "UM", "Wild (NOR)", "Hatchery (HOR)")
  }
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
    c("AD + CWT", "")
  )
}

id_origin <- function(species, tag) {

}
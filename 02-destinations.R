### Define AHP Goal object ----------------------------------------------------
ahp_destinations <- c(
  "Bremer Bridge", "Broodstock", "Cispus", "Lower River", "Mortality",
  "Packwood", "Riffe Lake", "Scanewa", "Surplus", "Tilton"
)

ahp_goal <- function(...) {
  new_goals <- list(...)
  stopifnot(all(names(new_goals) %in% ahp_destinations))
  goal <- modifyList(list(
    `Bremer Bridge` = 0,
    `Broodstock` = 0,
    `Cispus` = 0,
    `Lower River` = 0,
    `Mortality` = 0,
    `Packwood` = 0,
    `Riffe Lake` = 0,
    `Scanewa` = 0,
    `Surplus` = 0,
    `Tilton` = 0
  ), new_goals)
  structure(
    goal,
    class = "ahp_goal"
  )
}

## Missing version
ahp_goal_na <- function() {
  structure(
    list(
      `Bremer Bridge` = NA,
      `Broodstock` = NA,
      `Cispus` = NA,
      `Lower River` = NA,
      `Mortality` = NA,
      `Packwood` = NA,
      `Riffe Lake` = NA,
      `Scanewa` = NA,
      `Surplus` = NA,
      `Tilton` = NA
    ),
    class = "ahp_goal"
  )
}

## Extract only the transport goals (exclude broodstock collection,
## mortalities, and surpluses)
only_transport <- function(goal) {
  map(goal, function(goal) {
    if (!is.list(goal) && is.na(goal)) {
      return(NA)
    }
    stopifnot(isa(goal, "ahp_goal"))
    goal[c(
      "Bremer Bridge", "Cispus", "Lower River", "Packwood", "Riffe Lake",
      "Scanewa", "Tilton"
    )]
  })
}

### Spring Chinook ------------------------------------------------------------
## Non-vectorized function to determine AHP goals by tag, lifestage, and date
.dest_sprchk <- function(tag, lifestage, date) {
  if (lifestage %in% c("Jack", "Adult")) {
    ## Assuming here that UM + PIT is a wild fish from FCE trials
    if (tag %in% c("UM", "UM + PIT Tag")) {
      dest <- ahp_goal(Scanewa = 1)
    } else if (tag %in% c("Ad only", "UM + BWT")) {
      if (month(date) < 7) {
        dest <- ahp_goal(
          Packwood = 0.25, Cispus = 0.25,
          Scanewa = 0.5, Broodstock = TRUE
        )
      } else {
        dest <- ahp_goal(Packwood = 0.5, Cispus = 0.5, Broodstock = TRUE)
      }
    } else if (tag %in% c("AD + CWT")) {
      dest <- ahp_goal(Broodstock = 1)
    } else if (tag %in% c("AD + RV", "AD + RV + OP RET")) {
      dest <- ahp_goal(Surplus = 1)
    } else {
      dest <- ahp_goal_na()
    }
  } else if (lifestage == "Mini Jack") {
    if (tag %in% c("Ad only", "UM")) {
      dest <- ahp_goal(`Riffe Lake` = 1)
    } else {
      dest <- ahp_goal(Broodstock = 1)
    }
  } else {
    dest <- ahp_goal_na()
  }
  dest
}

## Vectorize above function
dest_sprchk <- function(tag, lifestage, date) {
  pmap(
    list(tag = tag, lifestage = lifestage, date = date),
    .dest_sprchk
  )
}

### Fall Chinook --------------------------------------------------------------
.dest_fallchk <- function(tag) {
  if (tag == "Ad only") {
    dest <- ahp_goal(Tilton = 0.75, `Bremer Bridge` = 0.25, Broodstock = TRUE)
  } else if (tag == "AD + CWT") {
    dest <- ahp_goal(Broodstock = 1)
  } else if (tag %in% c("UM", "UM + BWT")) {
    dest <- ahp_goal(`Bremer Bridge` = 1, Broodstock = 1 / 3)
  } else {
    dest <- ahp_goal_na()
  }
  dest
}

dest_fallchk <- function(tag, ...) {
  map(tag, .fallchk_dest)
}

### Coho
.dest_coho <- function(tag) {
  if (tag == "Ad only") {
    dest <- ahp_goal(Tilton = 0.75, `Bremer Bridge` = 0.25, Broodstock = TRUE)
  } else if (tag == "AD + CWT") {
    ## Separate goal for "flow/spill", not sure how to account for this
    dest <- ahp_goal(Packwood = 0.25, Cispus = 0.25, Scanewa = 0.5, Broodstock = TRUE)
  } else if (tag == "UM + BWT") {
    dest <- ahp_goal(`Bremer Bridge` = 1)
  } else if (tag == "UM") {
    dest <- ahp_goal(Scanewa = 1, Broodstock = TRUE)
  } else {
    dest <- ahp_goal_na()
  }
  dest
}

dest_coho <- function(tag, ...) {
  map(tag, .dest_coho)
}

### Winter steelhead ----------------------------------------------------------
## FIXME Not sure what to do with RV tagged fish
.dest_winsth <- function(tag) {
  if (tag == "Ad only") {
    ## Hatchery
    dest <- ahp_goal(Broodstock = 1)
  } else if (tag == "AD + CWT") {
    ## Integrated
    dest <- ahp_goal(Packwood = 0.5, Cispus = 0.5, Broodstock = TRUE)
  } else if (tag %in% c("AD + LV", "AD+LV+PIT")) {
    ## Integrated (Tilton)
    dest <- ahp_goal(Tilton = 0.75, `Bremer Bridge` = 0.25, Broodstock = TRUE)
  } else if (tag %in% c("UM + DORSAL CWT", "UM + DOR + PIT")) {
    ## Wild
    dest <- ahp_goal(`Bremer Bridge` = 1)
  } else if (tag %in% c("UM + BWT", "UM + BWT + PIT")) {
    dest <- ahp_goal(Scanewa = 1, Broodstock = TRUE)
  } else if (tag %in% c("UM", "UM + PIT Tag")) {
    dest <- ahp_goal(`Bremer Bridge` = 1, Broodstock = TRUE)
  } else {
    dest <- ahp_goal_na()
  }
  dest
}

dest_winsth <- function(tag, ...) {
  map(tag, .dest_winsth)
}

### Summer Steelhead ----------------------------------------------------------
.dest_sumsth <- function(tag) {
  if (tag %in% c("Ad only", "AD + PIT Tag", "AD + OP + PIT")) {
    dest <- ahp_goal(Broodstock = 1)
  } else if (tag %in% c("UM", "OP RET")) {
    ## Assume that OP RET means no other marks, so also UM
    dest <- ahp_goal(`Lower River` = 1, Broodstock = TRUE)
  } else {
    dest <- ahp_goal_na
  }
  dest
}

dest_sumsth <- function(tag, ...) {
  map(tag, .dest_sumsth)
}

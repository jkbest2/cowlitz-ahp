### Define AHP Goal object
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
.sprchk_dest <- function(tag, lifestage, date) {
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
  }
  dest
}

## Vectorize above function
sprchk_dest <- function(tag, lifestage, date) {
  pmap(
    list(tag = tag, lifestage = lifestage, date = date),
    .sprchk_dest
  )
}

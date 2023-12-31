
# - clergy political agendas ------------------------------------------------- #
# - script 01: cleaning ------------------------------------------------------ #

### note: this script 
###       (a) subsets the sample to primary leaders,
###       (b) subsets the sample to those that engage in politics,
###       (c) clean and save the analysis file.

# ---------------------------------------------------------------------------- #

rm(list = ls())
pacman::p_load(janitor,
               haven,
               tidyverse)

d <- read.csv("./data/data_nsrl.csv") |>
  clean_names() |> filter(position == 1)

# PART 1: POLITICAL ISSUES --------------------------------------------------- #

d.pol <- d |>
  
  ## keep those who engaged in politics (above & beyond voting)
  mutate(
    onlyvoted =
      ifelse(
        polgroup == 1 &
          urgeact == 1 &
          lobbied == 1 &
          protest == 1 &
          prayissue == 1 &
          tookstnd == 1,
        1,
        0
      )) |>
  filter(onlyvoted == 0) |>
  
  ## select the political issue variables
  dplyr::select(
    caseid,
    actabort,
    actcapun,
    actecon,
    acteduc,
    actenvir,
    actforgn,
    actlgbt,
    actgun,
    actheal,
    acthungr,
    actimm,
    actpolic,
    actrace) |>
  
  ## code such that 1 equals to "non-addressed" and 2 equals to "addressed"
  mutate(across(!caseid, ~ recode(.x, "1" = 2, "2" = 1)))

# PART 2: RELIGIOUS TRADITION ------------------------------------------------ #

d.cov <- d |>
  dplyr::select(caseid, cong_trad) |>
  filter(cong_trad != 6) |> # drop non-christian groups
  mutate(
    cong_trad = factor(
      cong_trad,
      levels = c(1, 2, 3, 4),
      labels = c(
        "roman catholic",
        "white conservative",
        "black protestant",
        "white liberal")
    )
  )

# PART 2: MERGE & SAVE ------------------------------------------------------- #

d <- 
  inner_join(d.pol, d.cov, by = "caseid") |> 
  drop_na()

# save for LatentGOLD
write_sav(d, path = "./data/data_nsrl.sav")

# ---------------------------------------------------------------------------- #

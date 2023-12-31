
# - clergy political agendas ------------------------------------------------- #
# - script 02: LCA ----------------------------------------------------------- #

### note: this script 
###       (a) plots the LCA model fit statistics,
###       (b) describes the item-response probabilities for each class,
###       (c) shows the distribution across religious traditions.

# ---------------------------------------------------------------------------- #

rm(list = ls())
pacman::p_load(janitor,
               haven,
               hrbrthemes,
               tidyverse)
theme_set(theme_ipsum_rc())

d <- haven::read_sav("./data/data_nsrl.sav")

# PART 1: ADD LG FILES ------------------------------------------------------- #

d.fits <- read_csv(
  "./LG/LG_modelfits.csv")
d.prob <- read_csv(
  "./LG/LG_probabilities.csv")
d.est1 <- read_csv(
  "./LG/LG_tradition01.csv")
d.est2 <- read_csv(
  "./LG/LG_tradition02.csv")

## add posterior classifications
d <- d |>
  left_join(
    read_sav("./LG/data_nsrl-posteriors.sav") |>
      clean_names() |>
      select(caseid, class = clu_number) |>
      mutate(class = factor(
        class,
        levels = c(4, 2, 3, 1),
        labels = c(
          "Universally Engaged",
          "Social Justice: Global Citizenship",
          "Social Justice: Community Empowerment",
          "Moral Reform"
        )
      )),
    by = "caseid"
  )

# PART 2: LCA MODEL FITS ----------------------------------------------------- #

png("./figures/FIG1.png", 
    width = 8, height = 6, units = "in", res = 500)
d.fits |>
  ggplot(aes(x = factor(cluster), y = bic)) +
  geom_point(aes(color = factor(resid)), size = 2.5) +
  geom_line(aes(group = factor(resid)),
            linewidth = .25,
            linetype = "dashed") +
  geom_line(aes(group = factor(alter)),
            linewidth = .25,
            linetype = "dashed") +
  labs(x = "The Number of Classes", y = "Bayesian Information Criteria",
       color = "Residuals Allowed to Vary") +
  scale_color_manual(values = wesanderson::wes_palette("IsleofDogs1", n = 2)) +
  theme_ipsum_rc() + theme(legend.position = "top") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
dev.off()

# PART 3: LCA PROFILES ------------------------------------------------------- #

# clean-up
d.prob <-
  d.prob |>
  pivot_longer(
    cols = -c(variable, outcome),
    names_to = "class",
    values_to = "estimate"
  ) |>
  mutate(
    class = factor(
      class,
      levels = c("Cluster4",
                 "Cluster1",
                 "Cluster2",
                 "Cluster3"),
      labels = c(
        "Universally Engaged",
        "Moral Reform",
        "Social Justice: Global Citizenship",
        "Social Justice: Community Empowerment"
      )
    ),
    variable = case_when(
      variable == "actabort" ~ "Abortion",
      variable == "actcapun" ~ "Capital Punishment",
      variable == "actecon" ~ "Economics",
      variable == "acteduc" ~ "Education",
      variable == "actenvir" ~ "Environment",
      variable == "actforgn" ~ "Foreign Policy",
      variable == "actlgbt" ~ "LGBT",
      variable == "actgun" ~ "Guns",
      variable == "actheal" ~ "Health",
      variable == "acthungr" ~ "Hunger",
      variable == "actimm" ~ "Immigration",
      variable == "actpolic" ~ "Police",
      variable == "actrace" ~ "Race"
    ),
    variable = factor(variable,
                      levels = c("Race",
                                 "Abortion",
                                 "Hunger",
                                 "LGBT",
                                 "Immigration",
                                 "Economics",
                                 "Education",
                                 "Health",
                                 "Police",
                                 "Guns",
                                 "Environment",
                                 "Capital Punishment",
                                 "Foreign Policy")),
    outcome = factor(
      outcome,
      levels = c(2, 1),
      labels = c("Yes", "No")
    )
  )

# plot
png("./figures/FIG2.png",
    width = 10, height = 7.5, units = "in", res = 500)
d.prob |>
  ggplot(aes(x = variable,
             y = estimate,
             fill = outcome)) +
  geom_bar(stat = "identity",
           position = "stack",
           color = "black") +
  facet_wrap( ~ class, nrow = 5) +
  labs(x = "Variables", y = "Estimated Item Values", fill = "Response") +
  scale_fill_manual(values = c("#5b9877", "gray50")) +
  theme_ipsum() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# PART 4: POSTERIOR CLASSIFICATIONS ------------------------------------------ #

png("./figures/FIG3.png",
    width = 8, height = 5, units = "in", res = 500)
d.est1 |>
  pivot_longer(cols = -trad) |>
  mutate(name = factor(
    name,
    levels = c("Cluster1",
               "Cluster4",
               "Cluster2",
               "Cluster3"),
    labels = c(
      "Universally Engaged",
      "Moral Reform",
      "Social Justice: Global Citizenship",
      "Social Justice: Community Empowerment"
    )
  )) |>
  mutate(trad = factor(
    trad,
    levels = c(
      "roman catholic",
      "white conservative",
      "black protestant",
      "white liberal"
    )
  )) |>
  ggplot(aes(x = trad, y = value, fill = name)) +
  geom_col(position = "fill") +
  theme_ipsum_rc(grid = F) + theme(legend.position = "top") +
  labs(x = "Political Agendas", y = "", fill = "") +
  scale_fill_manual(values = wesanderson::wes_palette("IsleofDogs1", n = 4))
dev.off()

# ---------------------------------------------------------------------------- #




library(tidyverse)
library(exscidata)
library(dplyr)
library (ggplot2)

remotes::install_github("dhammarstrom/exscidata")

dxadata <- data.frame(lean.mass = rnorm(100, 6000, 100))

## Base R forward pipe operator

exscidata::dxadata |> select(participant)

## when they differ

exscidata::dxadata |>
  lm(weight ~ height, data = _)

glimpse(exscidata::dxadata)

## variablen finden in tidy select
exscidata::dxadata |>
  select(participant:weight, starts_with("lean.") & contains("_leg"))


exscidata::dxadata |>
  select(weight, fp = participant)

## Rename
exscidata::dxadata |>
  select(participant:weight) |>
  rename(kg = weight) |>
  relocate(kg, .before = participant)

## Mutate
lean_mass <- exscidata::dxadata %>%
  select(participant:weight, lean.whole) |>
  
  
  mutate(rel_lean = 100 * ((lean.whole/1000)/weight),
         anewvariable = rnorm(80, 0, 1)) |>
  print()


## Create a small data set containing volume/leg information
leg_volume <- exscidata::dxadata |>
  select(participant, single, multiple) |>
  
  pivot_longer(names_to = "volume",
               values_to = "leg",
               cols = single:multiple) |>
  distinct(participant, volume, leg) |> ## distinct: man behält nur unique infos
  
  print()


## Create a small data set containing volume/leg information
lean_mass <- exscidata::dxadata |>
  select(participant, include, time,
         starts_with("lean.") & contains("_leg")) |>
  
  pivot_longer(names_to = "leg",
               values_to = "leanmass",
               cols = starts_with("lean")) |>
  mutate(leg = if_else(leg == "lean.left_leg", "L", "R")) |>
  
  print()


lean_mass <- full_join(leg_volume, lean_mass)

lean_mass |>
  filter(include == "incl") |>
  print()



lean_mass |>
  group_by(time)

lean_mass |>
  filter(include == "incl") |>
  group_by(time, volume) |>
  summarise(Median = median(leanmass))

print()


lean_mass_sum <- lean_mass |>
  filter(include == "incl") |>
  
  summarise(Median = median(leanmass),
            Mean =mean(leanmass),
            SD = sd(leanmass),
            q25 = quantile(leanmass, 0.25),
            q75 = quantile (leanmass, 0.75),
            Min = min(leanmass),
            Max = max(leanmass),
            
            .by = c(time, volume)) |>
  
  print()


p <- ggplot(data = lean_mass_sum,
            aes(time, Median, color = volume)) +
  geom_point() +
  geom_point(aes(y = Mean), shape = 21)




## am einfachsten direkt in der PDF die veränderungen zu sehen

ggsave("figures/lean_mass.pdf",
       p,
       width = 8.9,
       height = 8.9,
       units = "cm")

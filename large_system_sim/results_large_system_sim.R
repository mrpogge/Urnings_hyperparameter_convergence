library(tidyverse)
library(Hmisc)

set.seed(13181913)
#dir.create("figures", showWarnings = FALSE)
################################################################################
#READING THE SIMULATION OUTPUTS
################################################################################
large_system_sim = readRDS("large_systems_sim.rds")

large_system_sim = large_system_sim %>% 
  mutate(across(starts_with("iter"), ~ (. - true_value)^2))

player_urnsizes = large_system_sim %>% 
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

plot(as.vector(unlist(player_urnsizes[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(player_urnsizes[i,-1])), col = i)
}

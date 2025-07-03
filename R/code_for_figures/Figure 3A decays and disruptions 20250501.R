library(tidyverse)
library(patchwork)
library(ggrepel)
library(geosphere)


#Reading in data (corresponding to function call in determining FOI script)
df.foi.i <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/output/foi_for_return_time_plotting.csv")


#Specifying  half lives for illustrating decay in protection
r_fast.p <- 0.010
r_mid.p  <- 0.005
r_slow.p <- 0.001
#Starting at 1
P_0.p <- 1

#Using MNJ.10 as exemplar
df.curves <- tibble(t = rep(1:10000, 3),
                    code.new = c(rep("MNJ.10", 10000), rep("MNJ.10", 10000), rep("MNJ.10", 10000)),
                    age = c(rep("Young children (5y)", 10000), rep("School aged children (13y)", 10000), rep("Adults (27y)", 10000)),
                    foi.obs = c(rep(df.foi.i$prob.inf[df.foi.i$code.new == "MNJ.10" & df.foi.i$day == 1 & df.foi.i$age ==  5], 10000), 
                                rep(df.foi.i$prob.inf[df.foi.i$code.new == "MNJ.10" & df.foi.i$day == 1 & df.foi.i$age == 13], 10000),
                                rep(df.foi.i$prob.inf[df.foi.i$code.new == "MNJ.10" & df.foi.i$day == 1 & df.foi.i$age == 27], 10000))) %>%
  mutate(E = 0.88,
         decay.fast = 1-P_0.p*(1 - r_fast.p)^t,
         decay.mid  = 1-P_0.p*(1 - r_mid.p)^t,
         decay.slow = 1-P_0.p*(1 - r_slow.p)^t) %>%
  mutate(E.t.fast = E*(1-decay.fast),
         E.t.mid  = E*(1-decay.mid),
         E.t.slow = E*(1-decay.slow)) %>%
  mutate(foi.effective.fast = foi.obs*(1-E.t.fast),
         foi.effective.mid  = foi.obs*(1-E.t.mid),
         foi.effective.slow = foi.obs*(1-E.t.slow)) %>%
  mutate(prob.inf = 1-exp(-foi.obs*t),
         prob.inf.fast_decay = 1-exp(-foi.effective.fast*t),
         prob.inf.mid_decay  = 1-exp(-foi.effective.mid*t),
         prob.inf.slow_decay = 1-exp(-foi.effective.slow*t),
         prob.inf.covered = 1-exp(-foi.obs*(1-E)*t))

df.curves.p <- df.curves %>%
  dplyr::select(t:E, prob.inf:prob.inf.covered) %>%
  pivot_longer(!c(t:E), names_to = "scenario.i", values_to = "prob.inf") %>%
  mutate(scenario = case_when(
    scenario.i == "prob.inf"            ~ "No supplemental intervention",
    scenario.i == "prob.inf.fast_decay" ~ "Shorter half-life intervention",
    scenario.i == "prob.inf.mid_decay"  ~ "Moderate half-life intervention",
    scenario.i == "prob.inf.slow_decay" ~ "Longer half-life intervention",
    scenario.i == "prob.inf.covered"    ~ "Continuous intervention coverage")) %>%
  mutate(scenario = factor(scenario, levels = c(
    "No supplemental intervention",
    "Shorter half-life intervention",
    "Moderate half-life intervention",
    "Longer half-life intervention",
    "Continuous intervention coverage"
  )))


df.curves.p2 <- df.curves %>%
  dplyr::select(t:E, prob.inf:prob.inf.covered) %>%
  pivot_longer(!c(t:E), names_to = "scenario.i", values_to = "prob.inf") %>%
  #Dropping some scenarios for simplicity
  filter(scenario.i != "prob.inf.fast_decay") %>%
  filter(age != "Adults (27y)") %>%
  mutate(scenario = case_when(
    scenario.i == "prob.inf"            ~ "(1) Observed: No supplemental intervention",
    scenario.i == "prob.inf.mid_decay"  ~ "(2) Disruption of shorter half-life supplemental intervention",
    scenario.i == "prob.inf.slow_decay" ~ "(3) Disruption of longer half-life supplemental intervention",
    scenario.i == "prob.inf.covered"    ~ "(4) Continuous intervention coverage"
    )) %>%
  mutate(scenario = factor(scenario, levels = c(
    "(1) Observed: No supplemental intervention",
    "(2) Disruption of shorter half-life supplemental intervention",
    "(3) Disruption of longer half-life supplemental intervention",
    "(4) Continuous intervention coverage"
    ))) %>%
  mutate(foi.cat = case_when(
    age == "Young children (5y)" ~ "Low",
    age == "School aged children (13y)" ~ "High"))

#Plotting
p.curve1 <- df.curves.p2 %>% 
  filter(t < 700) %>%
  ggplot(aes(x = t, y = prob.inf, linetype = foi.cat, color = scenario)) +
  geom_hline(yintercept = 0.35, color = "grey50", alpha = 0.5, linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  #facet_wrap(vars(age), nrow = 3) +
  scale_color_viridis_d(option = "inferno", name = "Scenario", direction = -1, begin = 0.1, end = 0.85) +
  scale_linetype_discrete(name = "Force of infection (FOI)") +
  xlab("Time") + ylab("Cumulative probability of a new malaria infection") +
  scale_y_continuous(breaks = c(0, 1), position = "left") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.key.size = unit(30, "pt"),
        axis.title = element_text(size = 26),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.position = "right",
        legend.justification = "top")
p.curve1

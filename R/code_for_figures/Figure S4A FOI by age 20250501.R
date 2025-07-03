library(tidyverse)
library(patchwork)
library(ggrepel)


#v2 #########################
#Reading in data (corresponding to function call in determining FOI script)

df.age.i <- readr::read_csv('/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_foi/foi_age_exemplar 20250220.csv') %>%
  mutate(code.new = case_when(
    site_code == "N5" ~ "MNJ.01",
    site_code == "N4" ~ "MNJ.02",
    site_code == "N3" ~ "MNJ.03",
    site_code == "N2" ~ "MNJ.04",
    site_code == "N1" ~ "MNJ.05",
    site_code == "S1" ~ "MNJ.06",
    site_code == "S2" ~ "MNJ.07",
    site_code == "S3" ~ "MNJ.08",
    site_code == "S5" ~ "MNJ.09",
    site_code == "S6" ~ "MNJ.10")) %>%
  mutate(code.new = factor(code.new, levels = c("MNJ.06",
                                                "MNJ.04",
                                                "MNJ.03",
                                                "MNJ.05",
                                                "MNJ.02",
                                                "MNJ.07",
                                                "MNJ.08",
                                                "MNJ.09",
                                                "MNJ.01",
                                                "MNJ.10"))) %>%
  mutate(bednet.cat = if_else(bednet.c == 1, "Bednet recall = 1", "Bednet recall = 0"))


p.foi.age <- df.age.i %>% 
  #Age range trimmed to less than 70 yos due to paucity of enrollees over age 70
  ggplot(aes(x = age_yrs, y = fit, color = code.new, group = code.new)) +
  geom_point() +
  geom_path() +
  facet_wrap(vars(bednet.cat), ncol = 2) +
  scale_color_viridis_d(option = "turbo", name = "Site Code") +
  xlab("Age (years)") + 
  ylab("Probability of infection (per month)") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text = element_text(family = "Andale Mono", size = 20),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        legend.key.width = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm'))
p.foi.age




#Reading in data (corresponding to function call in determining FOI script)
dfi <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/output/foi_by_age.csv") %>%
  mutate(code.new = factor(code.new, levels = c("MNJ.06", "MNJ.04", "MNJ.03", "MNJ.05", "MNJ.02", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.01", "MNJ.10")))


p.foi.age <- dfi %>% 
  #For visualization, showing the month of January for all sites
  #Age range trimmed to less than 75 yos due to paucity of enrollees over age 75
  filter(month == "JAN") %>% filter(age < 75) %>%
  ggplot(aes(x = age, y = prob.inf, color = code.new, group = code.new)) +
  geom_point() +
  geom_path() +
  scale_color_viridis_d(option = "turbo", name = "Site Code") +
  xlab("Age (years)") + ylab("Force of infection (FOI)\n(per day)") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", size = 12),
        axis.title = element_text(size = 16),
        strip.background = element_rect(fill = "white"))
p.foi.age


library(tidyverse)
library(ggsankey)
library(ggridges)
library(patchwork)


#####################################################################################
## Figure S1A: Sankey flow chart of sampling
#####################################################################################

#Reading in data
dfi <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_malaria_rdt/rdt_data_20231007.csv") %>%
  dplyr::select(unique_ind_id, time_point, rdt.result)

df1 <- dfi %>% 
  #Dropping individuals that were never enrolled
  group_by(unique_ind_id) %>%
  mutate(sum_na = sum(is.na(rdt.result))) %>%
  filter(sum_na < 11) %>% ungroup() 

df2 <- df1 %>%
  #Converting rdt.result into character for plotting
  mutate(rdt.result = as.character(rdt.result)) %>%
  replace_na(list(rdt.result = "NO")) %>%
  mutate(rdt.result = case_when(
    rdt.result == "0"  ~ "0",
    rdt.result == "1"  ~ "1",
    rdt.result == "NO" ~ "NA")) %>% 
  mutate(time_point = case_when(
    time_point == "T01" ~ "T0",
    time_point == "T02" ~ "T01",
    time_point == "T03" ~ "T02",
    time_point == "T04" ~ "T03",
    time_point == "T05" ~ "T04",
    time_point == "T06" ~ "T05",
    time_point == "T07" ~ "T06",
    time_point == "T08" ~ "T07",
    time_point == "T09" ~ "T08",
    time_point == "T10" ~ "T09",
    time_point == "T11" ~ "T10")) %>% 
  pivot_wider(names_from = time_point, values_from = rdt.result)

df3 <- df2 %>% make_long(T0, T01, T02, T03, T04, T05, T06, T07, T08, T09, T10) %>%
  #Adding month and year of time point
  mutate(x = case_when(
    x == "T0"  ~ "T01\n2021\n(JUL-OCT)",
    x == "T01" ~ "T01\n2021\n(NOV-DEC)",
    x == "T02" ~ "T02\n2022\n(JAN-MAR)",
    x == "T03" ~ "T03\n2022\n(APR-MAY)",
    x == "T04" ~ "T04\n2022\n(MAY-JUN)",
    x == "T05" ~ "T05\n2022\n(JUL-AUG)",
    x == "T06" ~ "T06\n2022\n(AUG-SEP)",
    x == "T07" ~ "T07\n2022\n(OCT-NOV)",
    x == "T08" ~ "T08\n2022\n(NOV-DEC)",
    x == "T09" ~ "T09\n2023\n(JAN-MAR)",
    x == "T10" ~ "T10\n2023\n(MAR-APR)"
  ))

p.sankey <- df3 %>% ggplot(aes(x = x, 
                               next_x = next_x, 
                               node = node, 
                               next_node = next_node,
                               fill = factor(node),
                               label = node)) +
  geom_sankey(flow.alpha = 0.6,
              node.color = "gray30") +
  scale_fill_viridis_d(option = "magma",
                       name   = 'Malaria infection status\n(by RDT)',
                       labels = c("Negative", "Positive", "Unsampled"),
                       direction = -1,
                       end = 0.9) +
  theme_sankey(base_size = 16) + 
  xlab("Time Point") + ylab("Proportion of Enrollees") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p.sankey



#####################################################################################
## Figure S1B: Distribution of number of samples per individual
#####################################################################################

df4 <- df1 %>% group_by(unique_ind_id) %>%
  summarize(n = sum(!is.na(rdt.result))) %>% 
  #Dropping individuals with no follow-up infection observation
  #filter(n > 1) %>%
  mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
  mutate(code.new = case_when(
    site_code == "S1" ~ "MNJ.06",
    site_code == "S2" ~ "MNJ.07",
    site_code == "S3" ~ "MNJ.08",
    site_code == "S5" ~ "MNJ.09",
    site_code == "S6" ~ "MNJ.10",
    site_code == "N1" ~ "MNJ.05",
    site_code == "N2" ~ "MNJ.04",
    site_code == "N3" ~ "MNJ.03",
    site_code == "N4" ~ "MNJ.02",
    site_code == "N5" ~ "MNJ.01")) %>%
  mutate(code.new = factor(code.new, levels = c("MNJ.06", "MNJ.04", "MNJ.03", "MNJ.05", "MNJ.02", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.01", "MNJ.10")))

p.num.samples <- df4 %>% 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth=1, fill = "#AE123A", color = "white", alpha = 0.9) +
  scale_x_continuous(breaks = c(1, 5, 10)) +
  labs(subtitle = "All sites") +
  xlab("No. of malaria\ninfection observations") + ylab("Count of individuals") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono"),
        strip.background = element_rect(fill = "white"))
p.num.samples

p.num.samples.by.site <- df4 %>% 
  ggplot(aes(x = n, fill = code.new)) +
  geom_histogram(binwidth=1) +
  facet_wrap(vars(code.new), nrow = 1) +
  scale_x_continuous(breaks = c(1, 5, 10)) +
  scale_fill_viridis_d(option = "turbo", name = "Site Code") +
  labs(subtitle = "By site") +
  xlab("No. of malaria\ninfection observations") + ylab("Count of individuals") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono"),
        strip.background = element_rect(fill = "white"))
p.num.samples.by.site

p.num.samples + p.num.samples.by.site + plot_layout(nrow = 1, widths = c(1, 5))



#####################################################################################
## Figure S1C: Distribution of interval lengths
#####################################################################################

#Reading in data
df.interval <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/mnj_rdt_data_foi_20230821.csv") %>%
  dplyr::select(unique_ind_id, interlude, interlude.days) %>%
  mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
  mutate(code.new = case_when(
    site_code == "S1" ~ "MNJ.06",
    site_code == "S2" ~ "MNJ.07",
    site_code == "S3" ~ "MNJ.08",
    site_code == "S5" ~ "MNJ.09",
    site_code == "S6" ~ "MNJ.10",
    site_code == "N1" ~ "MNJ.05",
    site_code == "N2" ~ "MNJ.04",
    site_code == "N3" ~ "MNJ.03",
    site_code == "N4" ~ "MNJ.02",
    site_code == "N5" ~ "MNJ.01")) %>%
  mutate(code.new = factor(code.new, levels = c("MNJ.06", "MNJ.04", "MNJ.03", "MNJ.05", "MNJ.02", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.01", "MNJ.10")))

#Values
#% of samples with interval less than 30 days:
#30 DAYS
length(df.interval$interlude.days[df.interval$interlude.days < 30])
length(df.interval$interlude.days[df.interval$interlude.days < 30])/length(df.interval$interlude.days)*100

p.int.lens <- df.interval %>% 
  ggplot(aes(x = interlude.days)) +
  geom_histogram(binwidth=10, fill = "#76669B", color = "white", alpha = 0.9) +
  labs(subtitle = "All sites") +
  xlab("Interval between samples\n(days)") + ylab("Count of observations") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono"),
        strip.background = element_rect(fill = "white"))
p.int.lens

p.int.lens.by.site <- df.interval %>% 
  ggplot(aes(x = interlude.days, fill = code.new)) +
  geom_histogram(binwidth=10) +
  facet_wrap(vars(code.new), nrow = 1) +
  scale_fill_viridis_d(option = "turbo", name = "Site Code") +
  labs(subtitle = "By sites") +
  xlab("Interval between samples\n(days)") + ylab("Count of observations") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono"),
        strip.background = element_rect(fill = "white"))
p.int.lens.by.site

###Plotting panels B and C together
p.num.samples + p.num.samples.by.site + p.int.lens + p.int.lens.by.site + plot_layout(nrow = 2, widths = c(1, 5))

###Figure S2C sample intervals zoom in for response to Reviewer 4
p.int.lens.zoom <- df.interval %>% 
  ggplot(aes(x = interlude.days)) +
  geom_histogram(binwidth=10, fill = "#76669B", color = "white", alpha = 0.9) +
  labs(subtitle = "All sites") +
  xlab("Interval between samples\n(days)") + ylab("Count of observations") +
  scale_x_continuous(n.breaks = 30) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono"),
        strip.background = element_rect(fill = "white"))
p.int.lens.zoom

#####################################################################################
## Sample completion per time point
#####################################################################################

# Individuals

#Selecting individuals enrolled in the study (as indicated by sampled in last 3 time points)
#(individuals absent for more than 3 time points were no longer considered active enrollees)
df1.trim.ids <- df1 %>% filter(time_point %in% c("T09", "T10", "T11")) %>% filter(!is.na(rdt.result))

df.comp.by.tp <- df1 %>% filter(unique_ind_id %in% unique(df1.trim.ids$unique_ind_id)) %>%
  group_by(time_point) %>% 
  summarize(sum_na = sum(is.na(rdt.result)),
            n = sum(!is.na(rdt.result)),
            total = sum_na + n,
            perc = n/total * 100)

# Households
df.comp.by.tp.hh <- df1 %>% filter(unique_ind_id %in% unique(df1.trim.ids$unique_ind_id)) %>%
  mutate(hh_id = substr(unique_ind_id, 1, 5)) %>%
  group_by(hh_id, time_point) %>% 
  summarize(sum_na = sum(is.na(rdt.result)),
            n = sum(!is.na(rdt.result)),
            total.inds = sum_na + n,
            perc.sampled = n/total.inds * 100,
            sampled.hh = ifelse(sum_na != total.inds, 1, 0)) %>%
  group_by(time_point) %>%
  summarize(sampled.hh.count = sum(sampled.hh),
            n = n(),
            perc.sampled = sampled.hh.count/n * 100)
  








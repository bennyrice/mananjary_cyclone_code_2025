library(tidyverse)
library(patchwork)
library(mgcv)

#Reading in data (corresponding to function call in determining FOI script)
dfi <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/Lab Projects/2020 Projects/CRS2020/1 DATA/DATA/DATA_BEDNET/bednet_data_20230907.csv")

df.rdt <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_malaria_rdt/rdt_data_20231007.csv")

df1 <- full_join(df.rdt, dfi, by = join_by(unique_ind_id, time_point, site_code)) %>%
  filter(!is.na(rdt.result)) %>% filter(!is.na(bednet.24hrs.coded)) %>%
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
  mutate(code.new = factor(code.new, levels = c("MNJ.06", "MNJ.04", "MNJ.03", "MNJ.05", "MNJ.02", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.01", "MNJ.10"))) %>%
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
  #24hr recall
  mutate(bednet.stat.24hr = case_when(
    bednet.condition.24hrs == "good"        ~ "Yes",
    bednet.condition.24hrs == "bad_other"   ~ "Yes (damaged)",
    bednet.condition.24hrs == "hole_finger" ~ "Yes (damaged)",
    bednet.condition.24hrs == "hole_fist"   ~ "Yes (damaged)",
    bednet.condition.24hrs == "hole_head"   ~ "Yes (damaged)",
    bednet.condition.24hrs == "hole_unsp"   ~ "Yes (damaged)",
    is.na(bednet.condition.24hrs)           ~ "No")) %>%
  mutate(bednet.stat.24hr = factor(bednet.stat.24hr, levels = c("No", "Yes (damaged)", "Yes"))) %>%
  #2wk recall
  mutate(bednet.stat.2wks = case_when(
    bednet.condition.2wks == "good"        ~ "Yes",
    bednet.condition.2wks == "bad_other"   ~ "Yes (damaged)",
    bednet.condition.2wks == "hole_finger" ~ "Yes (damaged)",
    bednet.condition.2wks == "hole_fist"   ~ "Yes (damaged)",
    bednet.condition.2wks == "hole_head"   ~ "Yes (damaged)",
    bednet.condition.2wks == "hole_unsp"   ~ "Yes (damaged)",
    is.na(bednet.condition.2wks)           ~ "No")) %>%
  mutate(bednet.stat.2wks = factor(bednet.stat.2wks, levels = c("No", "Yes (damaged)", "Yes")))
  



#Due to sampling schedule, looking by month is not helpful as biased by some sites not covered during certain months
df2 <- df1 %>% mutate(month = lubridate::floor_date(sample.date, "month")) %>%
  group_by(month, bednet.stat.24hr) %>% 
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>% ungroup()

p.bednet.month <- df2 %>%
  ggplot(aes(x = month, y = prop, fill = bednet.stat.24hr)) +
  geom_bar(position = "fill", stat = "identity")
p.bednet.month


#by time point (all sites combined)
df3 <- df1 %>% 
  group_by(time_point, bednet.stat.24hr) %>% 
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>% ungroup()

p.bednet.tp <- df3 %>%
  ggplot(aes(x = time_point, y = prop, fill = bednet.stat.24hr)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis_d(option = "cividis", direction = -1, name = "Bednet Usage") +
  xlab("Sampling Time Point") + ylab("Proportion of individuals reporting using a bednet in last 24 hours") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", angle = 90, vjust = 0.5, hjust = 0.5))
p.bednet.tp


#by site and time point
df4 <- df1 %>% 
  dplyr::select(all_of(c("code.new", "unique_ind_id", "sample.date", "time_point", "bednet.stat.24hr", "bednet.stat.2wks"))) %>%
  pivot_longer(!code.new:time_point, names_to = "period", values_to = "status") %>%
  group_by(code.new, time_point, period, status) %>% 
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>% ungroup()

p.bednet.tp.site <- df4 %>%
  ggplot(aes(x = time_point, y = prop, fill = status)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(cols = vars(code.new), rows = vars(period)) +
  scale_fill_viridis_d(option = "cividis", direction = -1, name = "Bednet Usage") +
  xlab("Sampling Time Point") + ylab("Proportion of individuals reporting using a bednet") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", angle = 90, vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(fill = "white"))
p.bednet.tp.site

#2wk and 24hr recall are similar
df4 %>%
  ggplot(aes(x = period, y = prop, fill = status)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(vars(time_point), nrow = 1) +
  scale_fill_viridis_d(option = "cividis", direction = -1, name = "Bednet Usage") +
  xlab("period") + ylab("Proportion of individuals reporting using a bednet") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", angle = 90, vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(fill = "white"))

##Only showing 24hr since similar to 2wks
##Marking cyclone period (February 2022 and February 2023)
p.bednet.tp.site.24hr <- df4 %>%
  filter(period == "bednet.stat.24hr") %>%
  ggplot(aes(x = time_point, y = prop, fill = status)) +
  geom_bar(position = "fill", stat = "identity") +
  #Adding a vertical line for storm impact
  #geom_vline(xintercept = c(3, 10), linetype = 2, alpha = 0.9, color = "grey20") +
  facet_wrap(vars(code.new), nrow = 1) +
  scale_fill_viridis_d(option = "cividis", direction = -1, name = "Bednet Usage\n(24hr Recall)") +
  xlab("Sampling Time Point") + ylab("Proportion of individuals reporting using a bednet\n(in previous 24 hours)") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", angle = 90, vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(fill = "white"))
p.bednet.tp.site.24hr

##Prevalence heat strip: By time point
p.prev.heat <- df1 %>%
  group_by(code.new, time_point) %>% summarize(prev = sum(rdt.result)/n()*100) %>%
  ggplot(aes(x = time_point, y = 1, fill = prev)) +
  geom_tile() +
  facet_wrap(vars(code.new), nrow = 1) +
  scale_fill_viridis_c(option = "inferno", name = "Prevalence\n(%)", limits = c(0, 65)) +
  ylab(NULL) +
  xlab("Prevalence by time point") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", angle = 90, vjust = 0.5, hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_rect(fill = "white"))
p.prev.heat

p.prev.heat.avg <- df1 %>%
  group_by(code.new) %>% summarize(prev = sum(rdt.result)/n()*100) %>%
  ggplot(aes(x = code.new, y = 1, fill = prev)) +
  geom_tile() +
  facet_wrap(vars(code.new), nrow = 1, scales = "free_x") +
  scale_fill_viridis_c(option = "inferno", name = "Avg. Prevalence\n(%)", limits = c(0, 65)) +
  ylab(NULL) +
  xlab("Average prevalence") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_rect(fill = "white"))
p.prev.heat.avg

#Adding sample size
p.n <- df1 %>%
  group_by(code.new, time_point) %>% summarize(n = n()) %>%
  ggplot(aes(x = time_point, y = n)) +
  geom_point() +
  facet_wrap(vars(code.new), nrow = 1) +
  ylim(0, 375) +
  ylab("n") +
  xlab("Sample size by time point (number of individuals sampled)") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", angle = 90, vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(fill = "white"))
p.n


#Prevalence bar graph
p.prev.bar <- df1 %>%
  group_by(code.new, time_point) %>% summarize(prev = sum(rdt.result)/n()*100) %>%
  ggplot(aes(x = time_point, y = prev)) +
  geom_bar(stat = "identity", fill = "firebrick3") +
  facet_wrap(vars(code.new), nrow = 1) +
  #scale_fill_viridis_c(option = "inferno", name = "Prevalence\n(%)", limits = c(0, 65)) +
  ylab("Percent positive by RDT") +
  xlab("Prevalence by time point") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", angle = 90, vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(fill = "white"))
p.prev.bar

p.bednet.tp.site.24hr / p.prev.bar / p.n + plot_layout(heights = c(10, 6, 4))


################################################################################################
#pre-post storms with confidence intervals

#Checking dates
df1 %>% group_by(code.new, time_point, sample.date) %>% summarize(n = n()) %>%
  ggplot(aes(x = sample.date, y = n, color = code.new)) +
  geom_point() +
  scale_color_viridis_d(option = "turbo") +
  facet_grid(cols = vars(time_point), rows = vars(code.new), scales = "free")

#Batsirai
#Sites:
# "MNJ.01" Pre: T01 ; Post: T02
# "MNJ.02" Pre: T01 ; Post: T02
# "MNJ.03" Pre: T01 ; Post: T02
# "MNJ.04" Pre: T01 ; Post: T02
# "MNJ.05" Pre: T01 ; Post: T03
# "MNJ.06" Pre: T02 ; Post: T03
# "MNJ.07" Pre: T02 ; Post: T03
# "MNJ.08" Pre: T02 ; Post: T03
# "MNJ.09" Pre: T02 ; Post: T03
# "MNJ.10" Pre: T02 ; Post: T03

#Confidence intervals: using binom.test() and Clopper and Pearson (1934) method

#Writing a function to run binom.test() using number of positives and sample size
#Output of binom.test() is a htest object
#From which we extract the upper and lower confidence interval bounds
#For lower limit first
f.CI_calc_lower <- function(v.n_pos, v.n){
  v.CI_lower <- rep(NA, length(v.n_pos))
  for (i in 1:length(v.n_pos)) {
    x.i <- v.n_pos[i]
    n.i <- v.n[i]
    htest.i <- binom.test(x.i, n.i)
    v.CI_lower[i] <- htest.i$conf.int[1]
  }
  return(v.CI_lower)
}
#And now for upper limit
f.CI_calc_upper <- function(v.n_pos, v.n){
  v.CI_upper <- rep(NA, length(v.n_pos))
  for (i in 1:length(v.n_pos)) {
    x.i <- v.n_pos[i]
    n.i <- v.n[i]
    htest.i <- binom.test(x.i, n.i)
    v.CI_upper[i] <- htest.i$conf.int[2]
  }
  return(v.CI_upper)
}

#Cyclone Batsirai: T02-T01 vs T04-T03
#Cyclone Freddy:   T09-T08 vs T11-T10
p.T01.T03 <- df1 %>% filter(time_point %in% c("T01", "T03")) %>%
  group_by(time_point, bednet.stat.24hr) %>% 
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>% ungroup() %>%
  #Calculating the proportion not using a bednet
  group_by(time_point) %>% mutate(sum_n = sum(n)) %>% filter(bednet.stat.24hr == "No") %>%
  #Adding CIs
  rowwise() %>% 
  mutate(CI.lower = f.CI_calc_lower(n, sum_n)) %>%
  mutate(CI.upper = f.CI_calc_upper(n, sum_n)) %>%
  #Plotting
  ggplot(aes(x = time_point, y = prop)) +
  geom_bar(stat = "identity", fill = "#FCEA66") +
  geom_errorbar(aes(x=time_point, ymin=CI.lower, ymax=CI.upper), 
                width=0.4, 
                alpha=0.9) +
  #Adding a vertical line for storm impact
  geom_vline(xintercept = 1.5, linetype = 2, alpha = 0.9, color = "grey20") +
  scale_y_continuous(limits = c(0, 0.5)) +
  xlab("Sampling Time Point") + ylab("Proportion of individuals not using a bednet\n(in previous 24 hours)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", angle = 90, vjust = 0.5, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
  )
p.T01.T03


p.T08.T10 <- df1 %>% filter(time_point %in% c("T08", "T10")) %>%
  group_by(time_point, bednet.stat.24hr) %>% 
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>% ungroup() %>%
  group_by(time_point) %>% mutate(sum_n = sum(n)) %>% filter(bednet.stat.24hr == "No") %>%
  #Adding CIs
  rowwise() %>% 
  mutate(CI.lower = f.CI_calc_lower(n, sum_n)) %>%
  mutate(CI.upper = f.CI_calc_upper(n, sum_n)) %>%
  ggplot(aes(x = time_point, y = prop)) +
  geom_bar(stat = "identity", fill = "#FCEA66") +
  geom_errorbar(aes(x=time_point, ymin=CI.lower, ymax=CI.upper), 
                width=0.4, 
                alpha=0.9) +
  #Adding a vertical line for storm impact
  geom_vline(xintercept = 1.5, linetype = 2, alpha = 0.9, color = "grey20") +
  scale_y_continuous(limits = c(0, 0.5)) +
  xlab("Sampling Time Point") + ylab("Proportion of individuals not using a bednet\n(in previous 24 hours)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(family = "Andale Mono", angle = 90, vjust = 0.5, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        )
p.T08.T10

#Plotting
p.T01.T03 + p.T08.T10



###Controlling for age, site, sex in pre-post cyclone comparisons
df.gam <- df1 %>% 
  filter(!is.na(bednet.24hrs.coded)) %>%
  filter(!is.na(code.new)) %>%
  filter(!is.na(age.yrs.at.sample)) %>%
  filter(!is.na(sex)) %>%
  filter(!is.na(time_point)) %>%
  mutate(sex = factor(sex)) %>%
  mutate(time_point = factor(time_point)) %>%
  mutate(code.new = factor(code.new)) %>%
  dplyr::select(unique_ind_id, bednet.24hrs.coded, age.yrs.at.sample, sex, code.new, time_point)

#Exporting bednet data for convenience
# write_csv(df.gam, "/Users/blrice/Downloads/bednet_data.csv")


df.bednet <- read_csv('/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_bednet_usage/bednet_data_for_predictor_20241104.csv') %>%
  mutate(sex = factor(sex)) %>%
  mutate(time_point = factor(time_point)) %>%
  mutate(code.new = factor(code.new))

#fitting gam
model1  <- gam(bednet.24hrs.coded ~ s(age.yrs.at.sample) + code.new + sex + time_point, data = df.bednet, family=binomial)

summary(model1)
plot(model1)
AIC(model1)
gam.check(model1)



#using test case for illustration
test.age <- 5
test.code <- as.factor("MNJ.06")
test.sex <- as.factor("M")
test.time_point <- as.factor(c("T01","T03","T08","T10"))

predictions <- predict(model1, 
                       newdata=data.frame(age.yrs.at.sample=test.age, 
                                          code.new=test.code,
                                          sex=test.sex,
                                          time_point=test.time_point),
                       se.fit=TRUE,
                       type="response")

plot(1:4,1-predictions$fit,pch=19,ylim=c(0,0.5), axes=FALSE, xlab="", ylab="Proportion not using a bednet")
axis(1,at=c(1:4),lab=c("pre-Batsirai","post-Batsirai","pre-Freddy","post-Freddy"))
axis(2)
#for (j in 1:4) { points(c(j,j),1-predictions$fit[j]+c(-1.96,1.96)*predictions$se.fit[j], type="l")}
for (j in 1:4) { points(c(j,j),1-predictions$fit[j]+c(-1,1)*predictions$se.fit[j], type="l")}


#Plotting via ggplot
df.predictions <- tibble(test.age = test.age,
                         test.code = test.code,
                         test.sex = test.sex,
                         test.time_point = test.time_point,
                         fit = predictions$fit,
                         se = predictions$se.fit,
                         bednet.prop = fit,
                         no_bednet.prop = 1-bednet.prop,
                         cyclone = c("Tropical Cyclone Batsirai", "Tropical Cyclone Batsirai", "Tropical Cyclone Freddy", "Tropical Cyclone Freddy"),
                         comparison = c("pre", "post", "pre", "post")) %>%
  #Adding confidence intervals
  mutate(lower = no_bednet.prop-se, upper = no_bednet.prop+se) %>%
  mutate(cyclone = factor(cyclone, levels = c("Tropical Cyclone Batsirai", "Tropical Cyclone Freddy"))) %>%
  mutate(comparison = factor(comparison, levels = c("pre", "post")))

#Plot: facet and color by storm; add se for error bars, ylim 0 to max
p.5y <- df.predictions %>%
  ggplot(aes(x = comparison, y = no_bednet.prop, color = comparison)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  scale_y_continuous(limits = c(0, max(df.predictions$upper))) +
  facet_wrap(vars(cyclone), nrow = 1) +
  xlab("Comparison") + 
  ylab("Proportion reporting not using a bednet\n(for 5 year old males)") +
  scale_color_viridis_d(option = "mako", begin = 0.2, end = 0.8, name = "Cyclone Event") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.position = "none")
p.5y


## check the sig BETWEEN parameters by setting the focal one as a contrast - i.e., here pre-batsirai, just using R's proclivity to alphabetize
df.bednet$time_point.contrast <- as.character(df.bednet$time_point)
df.bednet$time_point.contrast[df.bednet$time_point.contrast=="T01"] <- "aT01"
df.bednet$time_point.contrast <- as.factor(df.bednet$time_point.contrast)
model1c  <- gam(bednet.24hrs.coded ~ s(age.yrs.at.sample) + code.new + sex + time_point.contrast, data = df.bednet, family=binomial)
summary (model1c)
## can see that T03 is significantly lower (on a postive scale as opposed to the way plotted)
## and the contrast is now with T01
## time_point.contrastT03 -0.44344    0.08454  -5.246 1.56e-07 ***


## same for Cyclone Freddy 
df.bednet$time_point.contrast <- as.character(df.bednet$time_point)
df.bednet$time_point.contrast[df.bednet$time_point.contrast=="T08"] <- "aT08"
df.bednet$time_point.contrast <- as.factor(df.bednet$time_point.contrast)
model1cc  <- gam(bednet.24hrs.coded ~ s(age.yrs.at.sample) + code.new + sex + time_point.contrast, data = df.bednet, family=binomial)
summary(model1cc)
## can see that T10 is significantly lower (on a postive scale as opposed to the way plotted)
## and the contrast is now with T08
## time_point.contrastT10 -0.18689    0.07375  -2.534   0.0113 *




#Plotting
p.T01.T03 + p.T08.T10 + p.5y + plot_layout(widths = c(1, 1, 2.5))




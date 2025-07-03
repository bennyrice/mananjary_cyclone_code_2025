library(tidyverse)
library(ggrepel)
library(metR) # for contour labels

####################################################################################################
# Defining probabilities and model verification
####################################################################################################

#Defining parameters and using dummy values to valide equations
#Probability of full vaccination (v)
v   <- 0.50
#Probability of partial vaccination (v_p)
v_p   <- 0
#Probability of infection (p_I)
p_I <- 0.45
#Vaccine efficacy against infection (E_i)
E_i <- 0.04
#Vaccine efficacy against symptomatic infection (E_s)
E_s <- 0.80
#Average efficacy for partial vaccinated individuals (E_p)
E_p <- 0.40
#Probability of clinical symptoms (ie fever) upon infection (s)
s   <- 0.53


#Model 1: No partial vaccination

#1 Symptomatic infections among vaccinated individuals (I_V_S)
I_V_S = v * p_I * (1 - E_i) * s * (1 - E_s)
#2 Asymptomatic infections among vaccinated individuals (I_V_nS)
I_V_nS = v * p_I * (1 - E_i) * (1 - s * (1 - E_s))
#3 Uninfected vaccinated individuals (nI_V)
nI_V = v * (1 - (p_I * (1 - E_i)))
#4 Symptomatic infections among unvaccinated individuals (I_nV_S)
I_nV_S = (1 - v) * p_I * s
#5 Asymptomatic infections among unvaccinated individuals (I_nV_nS)
I_nV_nS = (1 - v) * p_I * (1 - s)
#6 Uninfected unvaccinated individuals (nI_nV)
nI_nV = (1 - v) * (1 - p_I)

#Verifying that sum of probabilities for outcomes is 1
I_V_S + I_V_nS + nI_V + I_nV_S + I_nV_nS + nI_nV


#Model 2: Partial vaccination

#1 Symptomatic infections among fully vaccinated individuals (I_V_S)
I_V_S = v * p_I * (1 - E_i) * s * (1 - E_s)
#2 Asymptomatic infections among fully vaccinated individuals (I_V_nS)
I_V_nS = v * p_I * (1 - E_i) * (1 - s * (1 - E_s))
#3 Uninfected fully vaccinated individuals (nI_V)
nI_V = v * (1 - (p_I * (1 - E_i)))

#4 Symptomatic infections among partially vaccinated individuals (I_V_S)
I_VP_S = v_p * p_I * (1 - E_i) * s * (1 - E_p)
I_VP_S
#5 Asymptomatic infections among partially vaccinated individuals (I_V_nS)
I_VP_nS = v_p * p_I * (1 - E_i) * (1 - s * (1 - E_p))
I_VP_nS
#6 Uninfected fully vaccinated individuals (nI_V)
nI_VP = v_p * (1 - (p_I * (1 - E_i)))

#7 Symptomatic infections among unvaccinated individuals (I_nV_S)
I_nV_S = (1 - v - v_p) * p_I * s
#8 Asymptomatic infections among unvaccinated individuals (I_nV_nS)
I_nV_nS = (1 - v - v_p) * p_I * (1 - s)
#9 Uninfected unvaccinated individuals (nI_nV)
nI_nV = (1 - v - v_p) * (1 - p_I)

#Verifying that sum of probabilities for outcomes is 1
I_V_S + I_V_nS + nI_V + I_VP_S + I_VP_nS + nI_VP + I_nV_S + I_nV_nS + nI_nV



####################################################################################################
# Calculating symptomatic infection rates under vaccination scenarios
####################################################################################################

# Symptomatic infections = fully vaccinated symptomatics (I_V_S) + partially vaccinated symptomatics (I_VP_S) + unvaccinated symptomatics (I_nV_S)
# I_V_S + I_VP_S + I_nV_S = v * p_I * (1 - E_i) * s * (1 - E_s) + v_p * p_I * (1 - E_i) * s * (1 - E_p) + (1 - v - v_p) * p_I * s

### Test case

#Params
v <- 0.7
v_p <- 0
p_I <- 0.25
E_i <- 0
E_s <- 0.55
E_p <- 0
s <- 0.4


#Probability for test parameter set
PR.sympt.inf <- v * p_I * (1 - E_i) * s * (1 - E_s) + v_p * p_I * (1 - E_i) * s * (1 - E_p) + (1 - v - v_p) * p_I * s
PR.sympt.inf

#First, building a data frame for a range of vaccination coverages, symptomatic rates, and vaccine efficacies
#Fixing probability of infection to 10% (since looking at proportions only)
#No partial vaccination or efficacy against infection yet

#paramater ranges:
# p_I = 0.1
# v: 0:100
# s: 1:100
# E_s: 0:100
#n = approx. 101^3 = approx. 1,030,301 combinations for a given level of infection

#Making an initial data frame for vaccination simulation
#using expand.grid() to write out all possible combinations
dfv.i <- tibble(expand.grid(v = (0:100)/100,
                            s = (1:100)/100, #no cases expected if symptomatic infection = 0
                            E_s = (0:100)/100))



dfv <- dfv.i %>%
  #fixing the probability of infection 0.1 here
  mutate(p_I = 0.1) %>%
  #setting fixed paramaters
  mutate(E_i = 0, v_p = 0, E_p = 0) %>%
  #Calculate the probability of a symptomatic infection
  mutate(PR.sympt.inf = v * p_I * (1 - E_i) * s * (1 - E_s) + v_p * p_I * (1 - E_i) * s * (1 - E_p) + (1 - v - v_p) * p_I * s) %>%
  #Calculating baseline number of expected cases and declines under vaccination
  mutate(baseline_proportion = p_I*s,
         perc_decline = (baseline_proportion - PR.sympt.inf)/baseline_proportion*100,
         cases_averted_p1000 = baseline_proportion*1000 - PR.sympt.inf*1000,
         baseline_case_count_p1000 = baseline_proportion*1000,
         vaccinated_case_count_p1000 = PR.sympt.inf*1000) %>%
  #showing vaccinated proportion as a percentage for plotting
  mutate(v_perc = v*100)

##################################################################
## Plotting % reduction in cases
##################################################################

p1 <- dfv %>% 
  #Fixing symptomatic rate at an arbitrary level to reduce data frame size (s has no effect here)
  filter(s == 0.28) %>% 
  ggplot(aes(x = v_perc, y = perc_decline, group = E_s, color = E_s)) +
  #Shading portion of the plot to higlight v and E combos with reduction > 50%
  annotate("rect", xmin=50, xmax=100, ymin=50, ymax=100, fill = "black", alpha = 0.07) +
  #Adding the geom_path
  geom_path(linewidth = 0.8, alpha = 0.8) +
  #Overlay R21
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.75), 
            aes(x = v_perc, y = perc_decline, group = E_s),
            color = "black", linewidth = 1.3, alpha = 0.3) +
  geom_label_repel(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.75) %>% filter(v == 1), 
                   aes(label = paste0("E_s = ", E_s, " (R21)")),
                   nudge_x = 6.1,
                   na.rm = TRUE) +
  #Overlay RTS,S
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.55), 
            aes(x = v_perc, y = perc_decline, group = E_s),
            color = "black", linewidth = 1.3, alpha = 0.3) +
  geom_label_repel(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.55) %>% filter(v == 1), 
                   aes(label = paste0("E_s = ", E_s, " (RTS,S)")),
                   nudge_x = 7.2,
                   na.rm = TRUE) +
  #Adding horizontal and vertical guidelines
  #50% reduction threshold
  geom_segment(aes(x=0, xend=100, y=50, yend=50), color="grey50", linetype="dashed", linewidth=0.3) +
  #R21
  geom_segment(aes(x=50/0.75, xend=50/0.75, y=50, yend=100), color="grey50", linetype="dashed", linewidth=0.3) +
  #RTSS
  geom_segment(aes(x=50/0.55, xend=50/0.55, y=50, yend=100), color="grey50", linetype="dashed", linewidth=0.3) +
  #Perfect vaccine
  geom_segment(aes(x=50/1.00, xend=50/1.00, y=50, yend=100), color="grey50", linetype="dashed", linewidth=0.3) +
  #Overlaying intersection points
  #R21
  geom_point(aes(x = 50/0.75, y = 50), color = "black", alpha = 0.6) +
  #RTS,S
  geom_point(aes(x = 50/0.55, y = 50), color = "black", alpha = 0.6) +
  #Perfect vaccine
  geom_point(aes(x = 50/1.00, y = 50), color = "black", alpha = 0.6) +
  #Scales and themes
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_reverse(breaks = seq(0, 100, by = 10)) +
  scale_color_viridis_c(option = "cividis",
                        name = "Vaccine\nEfficacy",
                        end = 1) +
  xlab(expression(paste("Percent fully vaccinated at time of disruptive event (", italic("v"), ")"))) +
  ylab(expression(paste("Percent reduction in symptomatic infections expected (", italic("vE"), ")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16, family = "Arial"),
        axis.text = element_text(size = 13, family = "Arial"))
p1


#######################################################
# Simpler plot with RTSS and R21 only shown
#######################################################

df.v.points <- data.frame(cat    = c("R21","RTSS"),
                          v_perc = c(50/0.68, 50/0.56), 
                          perc_decline = rep(50, 2)) 


p1v2 <- dfv %>% 
  #Fixing symptomatic rate at an arbitrary level to reduce data frame size (s has no effect here)
  filter(s == 0.28) %>% 
  ggplot(aes(x = v_perc, y = perc_decline, group = E_s)) +

  #Shading portion of the plot to higlight v and E combos with reduction > 50%
  #R21
  annotate("rect", xmin=50/0.61, xmax=50/0.74, ymin=0, ymax=100, fill = "#243E6D", alpha = 0.07) +
  #RTS,S
  annotate("rect", xmin=50/0.51, xmax=50/0.60, ymin=0, ymax=100, fill = "#CABA6B", alpha = 0.07) +
  
  #Adding horizontal and vertical guidelines
  #50% reduction threshold
  geom_segment(aes(x=0, xend=100, y=50, yend=50), color="grey70", linetype="dashed", linewidth=0.4) +
  #R21 mid
  geom_segment(aes(x=50/0.68, xend=50/0.68, y=100, yend=0), color="grey70", linetype="dashed", linewidth=0.4) +
  #R21 max
  geom_segment(aes(x=50/0.74, xend=50/0.74, y=100, yend=0), color="grey70", linetype="dotted", linewidth=0.2) +
  #R21 min
  geom_segment(aes(x=50/0.61, xend=50/0.61, y=100, yend=0), color="grey70", linetype="dotted", linewidth=0.2) +
  
  #RTSS mid
  geom_segment(aes(x=50/0.56, xend=50/0.56, y=100, yend=0), color="grey70", linetype="dashed", linewidth=0.4) +
  #RTSS max
  geom_segment(aes(x=50/0.60, xend=50/0.60, y=100, yend=0), color="grey70", linetype="dotted", linewidth=0.2) +
  #RTSS min
  geom_segment(aes(x=50/0.51, xend=50/0.51, y=100, yend=0), color="grey70", linetype="dotted", linewidth=0.2) +
  
  #Overlay R21
  #R21 max efficacy
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.74), 
            aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.5) +
  #R21 mid efficacy
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.68), 
            aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 1) +
  #R21 min efficacy
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.61), 
            aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.5) +
  #R21 label
  geom_label_repel(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.68) %>% filter(v == 1), 
                   aes(label = "R21"), nudge_x = 6.1, na.rm = TRUE, color = "#243E6D") +
  #R21 paths
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.73), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.72), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.71), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.70), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.69), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.67), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.66), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.65), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.64), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.63), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.62), aes(x = v_perc, y = perc_decline, group = E_s), color = "#243E6D", linewidth = 1.3, alpha = 0.2) +

  #Overlay RTSS
  #RTSS max efficacy
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.60), 
            aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 0.5) +
  #RTSS mid efficacy
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.56), 
            aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 1) +
  #RTSS min efficacy
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.51), 
            aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 0.5) +
  #RTSS label
  geom_label_repel(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.56) %>% filter(v == 1), 
                   aes(label = "RTS,S"), nudge_x = 6.1, na.rm = TRUE, color = "#B8AB72") +
  #RTSS paths
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.59), aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.58), aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.57), aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.55), aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.54), aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.53), aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 0.2) +
  geom_path(data = dfv %>% filter(s == 0.28) %>% filter(E_s == 0.52), aes(x = v_perc, y = perc_decline, group = E_s), color = "#CABA6B", linewidth = 1.3, alpha = 0.2) +

  #Overlaying intersection points
  #R21
  geom_point(data = df.v.points %>% filter(cat == "R21"),
             aes(x = v_perc, y = perc_decline), color = "#243E6D", shape = 1, size = 3, stroke = 1) +
  #RTS,S
  geom_point(data = df.v.points %>% filter(cat == "RTSS"),
             aes(x = v_perc, y = perc_decline), color = "#CABA6B", shape = 1, size = 3, stroke = 1) +
  
  #Scales and themes
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  xlab(expression(paste("Percent fully vaccinated at time of disruptive event (", italic("v"), ")"))) +
  ylab(expression(paste("Percent reduction in symptomatic infections expected (", italic("vE"), ")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16, family = "Arial"),
        axis.text = element_text(size = 13, family = "Arial"))
p1v2

#Pulling out some bounds
#Symptomatic rates for children: 37-53%
#R21
#min scenario
dfv %>% filter(s == 0.370) %>% filter(E_s == 0.68) %>% filter(v == 0.50)
dfv %>% filter(s == 0.370) %>% filter(E_s == 0.75) %>% filter(v == 0.50)
#max scenario
dfv %>% filter(s == 0.530) %>% filter(E_s == 0.68) %>% filter(v == 0.70)
dfv %>% filter(s == 0.530) %>% filter(E_s == 0.75) %>% filter(v == 0.70)

#RTSS
dfv %>% filter(s == 0.370) %>% filter(E_s == 0.50) %>% filter(v == 0.50)
dfv %>% filter(s == 0.530) %>% filter(E_s == 0.56) %>% filter(v == 0.70)


##################################################################
## Plotting hazard ratio time series
##################################################################

#Fix coverage at 70%, s at 50%
#FOI: age group: 5 yos
#High FOI site: MNJ.10
#Mid  FOI site: MNJ.07
#Low  FOI site: MNJ.06
#Start date: Freddy landfall date (February 21, 2023)

#FOI time series from foi_post_storm but run out to 90 days (ie 3 months)


df.inf <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/output/foi_post_storm.csv") %>%
  filter(storm == "CYCLONE FREDDY", age == 5) %>%
  #Subsetting to high - mid - low exemplar sites for plotting
  filter(code.new %in% c("MNJ.10", "MNJ.07", "MNJ.06")) %>%
  mutate(FOI_cat = case_when(
    code.new == "MNJ.10" ~ "foi.high",
    code.new == "MNJ.07" ~ "foi.mid",
    code.new == "MNJ.06" ~ "foi.low"))

dfv.hazard <- df.inf %>% 
  dplyr::select(day, prob.inf, FOI_cat) %>%
  pivot_wider(values_from = prob.inf, names_from = FOI_cat) %>%
  #Setting vaccine coverage (v) to 1 to show for vaccinated popn
  mutate(v = 1,
         s = 0.50,
         #E.90 = 0.90,
         #E.80 = 0.80,
         #E.75 = 0.75, #R21 high
         E.68 = 0.68,  #R21 mid
         #E.70 = 0.70,
         #E.60 = 0.60,
         E.56 = 0.56, #RTS,S
         #E.50 = 0.50,
         #E.40 = 0.40,
         #E.30 = 0.30,
         #E.20 = 0.20,
         #E.10 = 0.10,
         E.0 = 0) %>%
  pivot_longer(!c(day:s), values_to = "E_s", names_to = "efficacy.cat") %>%
  pivot_longer(!c(day, v, s, efficacy.cat, E_s), values_to = "p_I", names_to = "foi.cat") %>%
  #setting fixed paramaters
  mutate(E_i = 0, v_p = 0, E_p = 0) %>%
  #Calculate the probability of a symptomatic infection
  mutate(PR.sympt.inf = v * p_I * (1 - E_i) * s * (1 - E_s) + v_p * p_I * (1 - E_i) * s * (1 - E_p) + (1 - v - v_p) * p_I * s) %>%
  #Calculating baseline number of expected cases and declines under vaccination
  mutate(baseline_proportion = p_I*s,
         perc_decline = (baseline_proportion - PR.sympt.inf)/baseline_proportion*100,
         cases_averted_p1000 = baseline_proportion*1000 - PR.sympt.inf*1000,
         baseline_case_count_p1000 = baseline_proportion*1000,
         vaccinated_case_count_p1000 = PR.sympt.inf*1000) %>%
  #showing vaccinated proportion as a percentage for plotting
  mutate(v_perc = v*100) %>%
  #cleaning up factors
  mutate(foi.cat = factor(foi.cat, levels = rev(c("foi.high", "foi.mid", "foi.low")))) %>%
  mutate(efficacy.cat = factor(efficacy.cat, levels = c("E.0", "E.68", "E.56")))

foi.cat_names <- c(
  foi.high = "High FOI locality (MNJ.10)",
  foi.mid  = "Mid FOI locality (MNJ.07)",
  foi.low  = "Low FOI locality (MNJ.06)"
)

p2 <- dfv.hazard %>% ggplot(aes(x = day, y = PR.sympt.inf, group = efficacy.cat)) +
  geom_line(aes(linetype = efficacy.cat), linewidth = 1, color = "#AE123A") +
  facet_wrap(vars(foi.cat), labeller = labeller(foi.cat = foi.cat_names)) +
  scale_linetype_discrete(name = "Vaccination Scenario",
                          labels = c("No vaccine", "Es = 56%", "Es = 68%")) +
  xlab("Days after storm landfall") + 
  ylab("Probability of symptomatic malaria infection (among young children)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "right",
        #legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))
p2


##################################################################
## Plotting cases averted
##################################################################

dfc.i <- tibble(expand.grid(v = (0:100)/100,
                            E_s = (0:100)/100,
                            p_I = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                            #no cases expected if symptomatic infection = 0
                            s = c(0.3, 0.4, 0.5, 0.6)))

dfc <- dfc.i %>%
  #setting fixed paramaters
  mutate(E_i = 0, v_p = 0, E_p = 0) %>%
  #Calculate the probability of a symptomatic infection
  mutate(PR.sympt.inf = v * p_I * (1 - E_i) * s * (1 - E_s) + v_p * p_I * (1 - E_i) * s * (1 - E_p) + (1 - v - v_p) * p_I * s) %>%
  #Calculating baseline number of expected cases and declines under vaccination
  mutate(baseline_proportion = p_I*s,
         perc_decline = (baseline_proportion - PR.sympt.inf)/baseline_proportion*100,
         cases_averted_p1000 = baseline_proportion*1000 - PR.sympt.inf*1000,
         baseline_case_count_p1000 = baseline_proportion*1000,
         vaccinated_case_count_p1000 = PR.sympt.inf*1000) %>%
  #showing vaccinated proportion as a percentage for plotting
  mutate(v_perc = v*100)
  

p.cases <- dfc %>% 
  ggplot(aes(x = v, y = E_s, fill = cases_averted_p1000)) +
  geom_tile() +
  scale_fill_viridis_c(option = "turbo",
                       name = "Cases averted\n(per 1000\nindividuals\ntargeted)") +
  facet_grid(rows = vars(p_I), cols = vars(s)) +
  xlab(expression('Proportion fully vaccinated at time of disruptive event, v')) +
  ylab(expression('Vaccine efficacy against symptomatic infection, E'[S])) +
  theme_bw() +
  theme(axis.title = element_text(size = 16, family = "Arial"),
        strip.background = element_rect(fill = "white"),
        legend.text = element_text(size = 12), legend.title = element_text(size = 14),
        panel.grid = element_blank())
p.cases


#Plotting a subset with contour lines
p.cases.trim <- dfc %>% 
  filter(s %in% c(0.4, 0.5)) %>%
  filter(p_I %in% c(0.3, 0.5, 0.7)) %>%
  ggplot(aes(x = v, y = E_s)) +
  geom_tile(aes(fill = cases_averted_p1000)) +
  geom_contour(aes(z = cases_averted_p1000), color = "black", breaks = c(100, 200, 300)) +
  scale_fill_viridis_c(option = "turbo",
                       name = "Cases averted\n(per 1000\nindividuals\ntargeted)") +
  facet_grid(rows = vars(p_I), cols = vars(s)) +
  xlab(expression('Proportion fully vaccinated at time of disruptive event, v')) +
  ylab(expression('Vaccine efficacy against symptomatic infection, E'[S])) +
  theme_bw() +
  theme(axis.title = element_text(size = 12, family = "Arial"),
        strip.background = element_rect(fill = "white"),
        legend.text = element_text(size = 10), legend.title = element_text(size = 12),
        panel.grid = element_blank())
p.cases.trim

##################################################################
## Plotting cases averted for supplement
##################################################################

dfc.i.s <- tibble(expand.grid(v = (0:100)/100,
                            E = (0:100)/100,
                            I = c(0.1, 0.2, 0.3, 0.4),
                            #no cases expected if symptomatic infection = 0
                            s = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)))

dfc.s <- dfc.i.s %>%
  #Calculate the probability of a case (i.e., symptomatic infection) for each scenario
  mutate(PR.case = v*I*s*(1-E) + (1-v)*I*s) %>%
  #Calculating baseline number of expected cases and declines under vaccination
  mutate(baseline_proportion = I*s,
         perc_decline = (baseline_proportion - PR.case)/baseline_proportion*100,
         cases_averted_p1000 = baseline_proportion*1000 - PR.case*1000,
         baseline_case_count_p1000 = baseline_proportion*1000,
         vaccinated_case_count_p1000 = PR.case*1000)

p.cases.s <- dfc.s %>% 
  ggplot(aes(x = v, y = E)) +
  geom_tile(aes(fill = cases_averted_p1000)) +
  metR::geom_contour2(aes(z = cases_averted_p1000, label = after_stat(level)),
                      color = "grey95",
                      skip = 0,
                      breaks = c(10, 25, 50, 100, 200, 300),
                      label.placer = label_placer_fraction(0.5)) +
  scale_fill_viridis_c(option = "turbo",
                       name = "Cases averted\n(per 1000 popn)",
                       direction = -1,
                       end = 0.9) +
  facet_grid(rows = vars(I), cols = vars(s)) +
  labs(subtitle = "Probability infection is symptomatic (s) vs Probability of infection") +
  xlab(expression('Proportion fully vaccinated at time of disruptive event, v')) +
  ylab(expression('Vaccine efficacy against symptomatic infection, E'[S])) +
  theme_bw() +
  theme(axis.title = element_text(size = 16, family = "Arial"),
        strip.background = element_rect(fill = "white"),
        panel.grid = element_blank())
p.cases.s


##################################################################
## Plotting cases averted for observed infection and s values
##################################################################

# Fever data by age group ########################################

#Reading in data (corresponding to function call in determining FOI script)
dff.i <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/Lab Projects/2020 Projects/CRS2020/1 DATA/DATA/DATA_QUESTIONNAIRE/R/output/cleaned_symptom_data 20240331.csv",
                       col_select = 1:24) %>%
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
  mutate(age.cat.at.sample.new = case_when(
    age.cat.at.sample == "A_0_2ys"     ~ "0-2ys",
    age.cat.at.sample == "B_2_5ys"     ~ "2-5ys",
    age.cat.at.sample == "C_6_13ys"    ~ "6-13ys",
    age.cat.at.sample == "D_14_21ys"   ~ "14-21ys",
    age.cat.at.sample == "E_22_40ys"   ~ "22-40ys",
    age.cat.at.sample == "F_40plus_ys" ~ "40+ys")) %>%
  mutate(age.cat.at.sample.new = factor(age.cat.at.sample.new, levels = c("0-2ys", "2-5ys", "6-13ys", "14-21ys", "22-40ys", "40+ys")))


#Calculating % with any fever within the last 2 weeks for RDT positives
dff1 <- dff.i %>% filter(rdt.result == 1) %>%
  mutate(fever.y.n = case_when(
    fever.recall.current.coded > 0 | fever.recall.2wks.coded > 0 ~ 1,
    .default = 0)) %>%
  group_by(age.cat.at.sample.new) %>% 
  summarize(n = n(),
            n.fever = sum(fever.y.n),
            prop.fever = sum(fever.y.n)/n())

#Confidence intervals: using binom.test() and Clopper and Pearson (1934) method
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

#Adding confidence intervals
dff2 <- dff1 %>% rowwise() %>% 
  mutate(CI.lower = f.CI_calc_lower(n.fever, n)) %>%
  mutate(CI.upper = f.CI_calc_upper(n.fever, n))


# Observed infected rates ########################################

df.inf.obs <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/output/foi_post_storm.csv") %>%
  filter(storm == "CYCLONE FREDDY", age == 5) %>%
  #Subsetting to day 90
  filter(day == 90) %>%
  #Multiplying by symptomatic infection proportion from above
  mutate(pI.s.upper = prob.inf * dff2$CI.upper[dff2$age.cat.at.sample.new == "2-5ys"],
         pI.s.mid   = prob.inf * dff2$prop.fever[dff2$age.cat.at.sample.new == "2-5ys"],
         pI.s.lower = prob.inf * dff2$CI.lower[dff2$age.cat.at.sample.new == "2-5ys"])


# Vaccination scenarios ########################################

#Making a data frame
#Vaccination coverage: varies from 0-100
#Efficacy: 0-100
#Keeping the observed symptomatic prob (pI * s.mid)
dfc.o.i <- tibble(expand.grid(v = (0:100)/100,
                              E_s = (0:100)/100,
                              pI.s = df.inf.obs$pI.s.mid)) %>%
  #Adding site codes
  mutate(code.new = case_when(
    pI.s == df.inf.obs$pI.s.mid[1] ~ df.inf.obs$code.new[1],
    pI.s == df.inf.obs$pI.s.mid[2] ~ df.inf.obs$code.new[2],
    pI.s == df.inf.obs$pI.s.mid[3] ~ df.inf.obs$code.new[3],
    pI.s == df.inf.obs$pI.s.mid[4] ~ df.inf.obs$code.new[4],
    pI.s == df.inf.obs$pI.s.mid[5] ~ df.inf.obs$code.new[5],
    pI.s == df.inf.obs$pI.s.mid[6] ~ df.inf.obs$code.new[6],
    pI.s == df.inf.obs$pI.s.mid[7] ~ df.inf.obs$code.new[7],
    pI.s == df.inf.obs$pI.s.mid[8] ~ df.inf.obs$code.new[8],
    pI.s == df.inf.obs$pI.s.mid[9] ~ df.inf.obs$code.new[9],
    pI.s == df.inf.obs$pI.s.mid[10] ~ df.inf.obs$code.new[10])) %>%
  mutate(code.new = factor(code.new, levels = c("MNJ.06", "MNJ.04", "MNJ.03", "MNJ.05", "MNJ.02", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.01", "MNJ.10")))
  
dfc.o <- dfc.o.i %>%
  #setting fixed paramaters
  mutate(E_i = 0, v_p = 0, E_p = 0) %>%
  #Calculate the probability of a symptomatic infection
  mutate(PR.sympt.inf = v * pI.s * (1 - E_i) * (1 - E_s) + v_p * pI.s * (1 - E_i) * (1 - E_p) + (1 - v - v_p) * pI.s) %>%
  #Calculating baseline number of expected cases and declines under vaccination
  mutate(baseline_proportion = pI.s,
         perc_decline = (baseline_proportion - PR.sympt.inf)/baseline_proportion*100,
         cases_averted_p1000 = baseline_proportion*1000 - PR.sympt.inf*1000,
         baseline_case_count_p1000 = baseline_proportion*1000,
         vaccinated_case_count_p1000 = PR.sympt.inf*1000)

#Vaccine coverage and efficacy scenarios to highlight
df.vos <- tibble(type = c("R21", "RTSS"),
                 v.mid = c(0.50/0.68, 0.50/0.56),
                 v.hi  = c(0.50/0.74, 0.50/0.51),
                 v.lo  = c(0.50/0.61, 0.50/0.60),
                 v.70  = c(0.70, 0.70),
                 E.mid = c(0.68, 0.56),
                 E.hi  = c(0.74, 0.60),
                 E.lo  = c(0.61, 0.51))

# Plotting ####################################################

#1 contour plot per locality
p.cases.o <- dfc.o %>% 
  ggplot(aes(x = v, y = E_s)) +
  geom_tile(aes(fill = cases_averted_p1000)) +
  metR::geom_contour2(aes(z = cases_averted_p1000, label = after_stat(level)),
                      color = "grey95",
                      skip = 0,
                      breaks = c(10, 25, 50, 100, 200, 300),
                      label.placer = label_placer_fraction(0.8)) +
  #Highlighting vacc scenarios of note
  geom_point(data = df.vos, aes(x = v.70, y = E.mid, color = type), size = 2) +
  geom_errorbar(data = df.vos, aes(x = v.70, ymin = E.lo, ymax = E.hi, color = type), width = 0.03) +
  #geom_errorbar(data = df.vos, aes(y = E.mid, xmin = v.lo, xmax = v.hi, color = type), width = 0.02) +
  #Scales and themes
  scale_fill_viridis_c(option = "mako",
                       name = "Cases averted\n(per 1000\nindividuals\ntargeted)",
                       end = 0.97,
                       begin = 0.15
                       ) +
  scale_color_manual(values = c("#ACD4EC", "#B8AB72"), name = "Vaccine type") +
  facet_wrap(vars(code.new), nrow = 2) +
  xlab(expression('Proportion fully vaccinated at time of disruptive event, v')) +
  ylab(expression('Vaccine efficacy against symptomatic infection, E'[S])) +
  theme_bw() +
  theme(axis.title  = element_text(size = 18, family = "Arial"),
        axis.text.y = element_text(size = 12, family = "Arial"),
        axis.text.x = element_text(size = 12, family = "Arial", angle = 90, vjust = 0.5),
        strip.background = element_rect(fill = "white"),
        legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        panel.grid  = element_blank())
p.cases.o


#######################################################################################
# Plotting r4 #########################################################################
#######################################################################################

#Vaccine efficacy estimates from literature
#R21:  E_s low = 61%; mid = 68%; high = 74%
R21.min <- 0.61
R21.mid <- 0.68
R21.max <- 0.74
#RTSS: E_s low = 51%; mid = 56%; high = 60%
RTS.min <- 0.51
RTS.mid <- 0.56
RTS.max <- 0.60

#Selecting a color for the 2 vaccines
# from viridis::mako(n = 30, begin = 0, end = 1)
# R21
color.R21 <- "#347FA4FF"
# RTSS
color.RTS <- "#787877FF"


# Figure 4A # Percent Reduction ####################################################

#Fixing symptomatic rate at an arbitrary level to reduce data frame size (s has no effect here)
dfv1 <- dfv %>% filter(s == 0.28)
#Calculating point where reduction is 50%
df.v.points <- data.frame(cat    = c("R21", "RTS"),
                          v_perc = c(50/R21.mid, 50/RTS.mid), 
                          perc_decline = rep(50, 2)) 

p4A <- dfv1 %>% 
  ggplot(aes(x = v_perc, y = perc_decline, group = E_s)) +
  
  #Shading portion of the plot to higlight v and E combos with reduction > 50%
  #R21
  annotate("rect", xmin=50/R21.min, xmax=50/R21.max, ymin=0, ymax=100, fill = color.R21, alpha = 0.07) +
  #RTS,S
  annotate("rect", xmin=50/RTS.min, xmax=50/RTS.max, ymin=0, ymax=100, fill = color.RTS, alpha = 0.07) +
  
  #Adding horizontal and vertical guidelines
  #50% reduction threshold
  geom_segment(aes(x=0, xend=100, y=50, yend=50), color="grey70", linetype="dashed", linewidth=0.4) +
  #R21 min
  geom_segment(aes(x=50/0.61, xend=50/R21.min, y=100, yend=0), color="grey70", linetype="dotted", linewidth=0.2) +
  #R21 mid
  geom_segment(aes(x=50/0.68, xend=50/R21.mid, y=100, yend=0), color="grey70", linetype="dashed", linewidth=0.4) +
  #R21 max
  geom_segment(aes(x=50/0.74, xend=50/R21.max, y=100, yend=0), color="grey70", linetype="dotted", linewidth=0.2) +
  
  #RTSS min
  geom_segment(aes(x=50/0.51, xend=50/RTS.min, y=100, yend=0), color="grey70", linetype="dotted", linewidth=0.2) +
  #RTSS mid
  geom_segment(aes(x=50/0.56, xend=50/RTS.mid, y=100, yend=0), color="grey70", linetype="dashed", linewidth=0.4) +
  #RTSS max
  geom_segment(aes(x=50/0.60, xend=50/RTS.max, y=100, yend=0), color="grey70", linetype="dotted", linewidth=0.2) +
  
  #Overlay R21
  #R21 max efficacy
  geom_path(data = dfv1 %>% filter(E_s == R21.max), aes(x = v_perc, y = perc_decline, group = E_s), color = color.R21, linewidth = 1.3, alpha = 0.5) +
  #R21 mid efficacy
  geom_path(data = dfv1 %>% filter(E_s == R21.mid), aes(x = v_perc, y = perc_decline, group = E_s), color = color.R21, linewidth = 1.3, alpha = 1) +
  #R21 min efficacy
  geom_path(data = dfv1 %>% filter(E_s == R21.min), aes(x = v_perc, y = perc_decline, group = E_s), color = color.R21, linewidth = 1.3, alpha = 0.5) +
  #R21 label
  geom_label_repel(data = dfv1 %>% filter(E_s == R21.mid) %>% filter(v == 1), aes(label = "R21"), nudge_x = 6.1, na.rm = TRUE, color = color.R21) +
  #R21 paths
  geom_path(data = dfv1 %>% filter(between(E_s, R21.min, R21.max)), aes(x = v_perc, y = perc_decline, group = E_s), color = color.R21, linewidth = 1.3, alpha = 0.2) +
  
  #Overlay RTSS
  #RTS max efficacy
  geom_path(data = dfv1 %>% filter(E_s == RTS.max), aes(x = v_perc, y = perc_decline, group = E_s), color = color.RTS, linewidth = 1.3, alpha = 0.5) +
  #RTS mid efficacy
  geom_path(data = dfv1 %>% filter(E_s == RTS.mid), aes(x = v_perc, y = perc_decline, group = E_s), color = color.RTS, linewidth = 1.3, alpha = 1) +
  #RTS min efficacy
  geom_path(data = dfv1 %>% filter(E_s == RTS.min), aes(x = v_perc, y = perc_decline, group = E_s), color = color.RTS, linewidth = 1.3, alpha = 0.5) +
  #RTS label
  geom_label_repel(data = dfv1 %>% filter(E_s == RTS.mid) %>% filter(v == 1), aes(label = "RTS,S"), nudge_x = 6.1, na.rm = TRUE, color = color.RTS) +
  #RTS paths
  geom_path(data = dfv1 %>% filter(between(E_s, RTS.min, RTS.max)), aes(x = v_perc, y = perc_decline, group = E_s), color = color.RTS, linewidth = 1.3, alpha = 0.2) +
  
  #Overlaying intersection points
  #R21
  geom_point(data = df.v.points %>% filter(cat == "R21"), aes(x = v_perc, y = perc_decline), color = color.R21, shape = 1, size = 3, stroke = 1) +
  #RTS,S
  geom_point(data = df.v.points %>% filter(cat == "RTS"), aes(x = v_perc, y = perc_decline), color = color.RTS, shape = 1, size = 3, stroke = 1) +
  
  #Scales and themes
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  xlab(expression(paste("Percent fully vaccinated at time of disruptive event (", italic("v"), ")"))) +
  ylab(expression(paste("Percent reduction in symptomatic infections expected (", italic("vE"), ")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16, family = "Arial"),
        axis.text = element_text(size = 13, family = "Arial"))
p4A



# Figure 4B # Hazard ratio ###########################################################

df.inf <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/output/foi_post_storm.csv") %>%
  filter(storm == "CYCLONE FREDDY", age == 5) %>%
  #Subsetting to high - mid - low exemplar sites for plotting
  filter(code.new %in% c("MNJ.10", "MNJ.07", "MNJ.06")) %>%
  mutate(FOI_cat = case_when(
    code.new == "MNJ.10" ~ "foi.high",
    code.new == "MNJ.07" ~ "foi.mid",
    code.new == "MNJ.06" ~ "foi.low"))



dfv.hazard <- df.inf %>% 
  dplyr::select(day, prob.inf, FOI_cat) %>%
  pivot_wider(values_from = prob.inf, names_from = FOI_cat) %>%
  #Setting vaccine coverage (v) to 1 to show for vaccinated popn
  mutate(v = 1,
         s = 0.50,

         E.74 = 0.74,
         E.73 = 0.73,
         E.72 = 0.72,
         E.71 = 0.71,
         E.70 = 0.70,
         E.69 = 0.69,
         E.68 = 0.68,  #R21 mid (Range: 0.61-0.74)
         E.67 = 0.67,
         E.66 = 0.66,
         E.65 = 0.65,
         E.64 = 0.64,
         E.63 = 0.63,
         E.62 = 0.62,
         E.61 = 0.61,

         E.60 = 0.60,
         E.59 = 0.59,
         E.58 = 0.58,
         E.57 = 0.57,
         E.56 = 0.56, #RTS,S mid (range: 0.51-0.60)
         E.55 = 0.55,
         E.54 = 0.54,
         E.53 = 0.53,
         E.52 = 0.52,
         E.51 = 0.51,
         
         E.0 = 0) %>%
  pivot_longer(!c(day:s), values_to = "E_s", names_to = "efficacy.cat") %>%
  pivot_longer(!c(day, v, s, efficacy.cat, E_s), values_to = "p_I", names_to = "foi.cat") %>%
  #setting fixed paramaters
  mutate(E_i = 0, v_p = 0, E_p = 0) %>%
  #Calculate the probability of a symptomatic infection
  mutate(PR.sympt.inf = v * p_I * (1 - E_i) * s * (1 - E_s) + v_p * p_I * (1 - E_i) * s * (1 - E_p) + (1 - v - v_p) * p_I * s) %>%
  #Calculating baseline number of expected cases and declines under vaccination
  mutate(baseline_proportion = p_I*s,
         perc_decline = (baseline_proportion - PR.sympt.inf)/baseline_proportion*100,
         cases_averted_p1000 = baseline_proportion*1000 - PR.sympt.inf*1000,
         baseline_case_count_p1000 = baseline_proportion*1000,
         vaccinated_case_count_p1000 = PR.sympt.inf*1000) %>%
  #showing vaccinated proportion as a percentage for plotting
  mutate(v_perc = v*100) %>%
  #cleaning up factors
  mutate(foi.cat = factor(foi.cat, levels = rev(c("foi.high", "foi.mid", "foi.low")))) %>%
  mutate(efficacy.cat = factor(efficacy.cat))

foi.cat_names <- c(
  foi.high = "High FOI locality (MNJ.10)",
  foi.mid  = "Mid FOI locality (MNJ.07)",
  foi.low  = "Low FOI locality (MNJ.06)"
)

p4B <- dfv.hazard %>% 
  
  ggplot(aes(x = day, y = PR.sympt.inf, group = efficacy.cat)) +
  
  #Overlay no vaccination scenario
  geom_path(data = dfv.hazard %>% filter(E_s == 0), aes(x = day, y = PR.sympt.inf, group = E_s), color = "#AE123A", alpha = 1, linetype = "dashed", linewidth = 1) +
  
  #Overlay R21
  #R21 max efficacy
  geom_path(data = dfv.hazard %>% filter(E_s == R21.max), aes(x = day, y = PR.sympt.inf, group = E_s), color = color.R21, alpha = 0.5) +
  #R21 mid efficacy
  geom_path(data = dfv.hazard %>% filter(E_s == R21.mid), aes(x = day, y = PR.sympt.inf, group = E_s), color = color.R21, alpha = 1) +
  #R21 min efficacy
  geom_path(data = dfv.hazard %>% filter(E_s == R21.min), aes(x = day, y = PR.sympt.inf, group = E_s), color = color.R21, alpha = 0.5) +
  #R21 paths
  geom_path(data = dfv.hazard %>% filter(between(E_s, R21.min, R21.max)), aes(x = day, y = PR.sympt.inf, group = E_s), color = color.R21, alpha = 0.2) +
  
  #Overlay RTS
  #RTS max efficacy
  geom_path(data = dfv.hazard %>% filter(E_s == RTS.max), aes(x = day, y = PR.sympt.inf, group = E_s), color = color.RTS, alpha = 0.5) +
  #RTS mid efficacy
  geom_path(data = dfv.hazard %>% filter(E_s == RTS.mid), aes(x = day, y = PR.sympt.inf, group = E_s), color = color.RTS, alpha = 1) +
  #RTS min efficacy
  geom_path(data = dfv.hazard %>% filter(E_s == RTS.min), aes(x = day, y = PR.sympt.inf, group = E_s), color = color.RTS, alpha = 0.5) +
  #RTS paths
  geom_path(data = dfv.hazard %>% filter(between(E_s, RTS.min, RTS.max)), aes(x = day, y = PR.sympt.inf, group = E_s), color = color.RTS, alpha = 0.2) +
  
  scale_x_continuous(breaks = c(0, 30, 60, 90)) +
  
  facet_wrap(vars(foi.cat), labeller = labeller(foi.cat = foi.cat_names)) +
  
  
  xlab("Days after storm landfall") + 
  ylab("Probability of symptomatic malaria infection (among young children)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "right",
        #legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))
p4B






# Figure 4C # Cases Averted (Line) ####################################################

#X-axis: vacc coverage
#Y-axis: cases averted
#Panel rows: Vaccine (R21 or RTSS)

dfc.o.p <- dfc.o %>% 
  #Scaling vaccine coverage as a percentage
  mutate(v = v * 100) %>%
  #Filtering to E_s values for R21 and RTS,S to reduce data frame size
  filter(between(E_s, RTS.min, R21.max))

p4C <- dfc.o.p %>% 
  ggplot() +
  #geom_line(aes(x = v, y = cases_averted_p1000, color = E_s, group = E_s)) +
  
  #Overlay R21
  #R21 max efficacy
  geom_path(data = dfc.o.p %>% filter(E_s == R21.max), aes(x = v, y = cases_averted_p1000, group = E_s), color = color.R21, linewidth = 0.5, alpha = 0.5) +
  #R21 mid efficacy
  geom_path(data = dfc.o.p %>% filter(E_s == R21.mid), aes(x = v, y = cases_averted_p1000, group = E_s), color = color.R21, linewidth = 0.5, alpha = 1) +
  #R21 min efficacy
  geom_path(data = dfc.o.p %>% filter(E_s == R21.min), aes(x = v, y = cases_averted_p1000, group = E_s), color = color.R21, linewidth = 0.5, alpha = 0.5) +
  #R21 label
  #geom_label_repel(data = dfc.o.p %>% filter(E_s == R21.mid) %>% filter(v == 1), aes(label = "R21"), nudge_x = 6.1, na.rm = TRUE, color = color.R21) +
  #R21 paths
  geom_path(data = dfc.o.p %>% filter(between(E_s, R21.min, R21.max)), aes(x = v, y = cases_averted_p1000, group = E_s), color = color.R21, linewidth = 0.2, alpha = 0.5) +
  
  #Overlay RTS,S
  #RTS max efficacy
  geom_path(data = dfc.o.p %>% filter(E_s == RTS.max), aes(x = v, y = cases_averted_p1000, group = E_s), color = color.RTS, linewidth = 0.5, alpha = 0.5) +
  #RTS mid efficacy
  geom_path(data = dfc.o.p %>% filter(E_s == RTS.mid), aes(x = v, y = cases_averted_p1000, group = E_s), color = color.RTS, linewidth = 0.5, alpha = 1) +
  #RTS min efficacy
  geom_path(data = dfc.o.p %>% filter(E_s == RTS.min), aes(x = v, y = cases_averted_p1000, group = E_s), color = color.RTS, linewidth = 0.5, alpha = 0.5) +
  #RTS label
  #geom_label_repel(data = dfc.o.p %>% filter(E_s == RTS.mid) %>% filter(v == 1), aes(label = "RTS"), nudge_x = 6.1, na.rm = TRUE, color = color.RTS) +
  #RTS paths
  geom_path(data = dfc.o.p %>% filter(between(E_s, RTS.min, RTS.max)), aes(x = v, y = cases_averted_p1000, group = E_s), color = color.RTS, linewidth = 0.2, alpha = 0.5) +
  
  
  #facet_wrap(vars(code.new), nrow = 1) +
  facet_wrap(vars(code.new), nrow = 2) +
  
  xlab(expression(paste("Percent fully vaccinated at time of disruptive event (", italic("v"), ")"))) +
  ylab("Symptomatic infections averted (per 1000 children targeted)") +
  theme_bw() +
  theme(axis.title  = element_text(size = 18, family = "Arial"),
        axis.text.y = element_text(size = 12, family = "Arial"),
        axis.text.x = element_text(size = 12, family = "Arial"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12, family = "Arial"),
        legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        #panel.grid  = element_blank()
        )
p4C


p4A
p4B
p4C








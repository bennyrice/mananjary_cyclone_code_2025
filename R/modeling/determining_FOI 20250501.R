library(tidyverse)
library(mgcv)
library(gratia)

### This provides the regression approach to estimating the force of infection as described in the paper
### Time spent in every month formally fit to allow for the time-varying covariates piece, see methods



##### A. Get the data and tidy ##################################################
##### A1: Malaria outcome data ##################################################
df <- read.csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/mnj_rdt_data_foi_20230821.csv")
## Making individual ID code and household ID code a factor for fitting random effects
df$unique_ind_id <- as.factor(df$unique_ind_id)
## First 5 digits of the individual ID code give the site code + household code
df$household <- as.factor(substring(df$unique_ind_id,1,5)) 

##### A1B: Updated malaria outcome data ##########################################
df1.i <- read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/mnj_rdt_data_foi_20230821.csv") %>%
  #Correcting time points (setting baseline as T0)
  mutate(interlude = case_when(
    interlude == "T01_T02" ~ "T0_T01",
    interlude == "T02_T03" ~ "T01_T02",
    interlude == "T03_T04" ~ "T02_T03",
    interlude == "T04_T05" ~ "T03_T04",
    interlude == "T05_T06" ~ "T04_T05",
    interlude == "T06_T07" ~ "T05_T06",
    interlude == "T07_T08" ~ "T06_T07",
    interlude == "T08_T09" ~ "T07_T08",
    interlude == "T09_T10" ~ "T08_T09",
    interlude == "T10_T11" ~ "T09_T10"))

##### A2: Temperature and precip data ###########################################
df.Tm <- read_csv('/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/processed/mean_daily_temp_ERA5.csv')
df.Pm <- read_csv('/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/processed/mean_daily_precip_ERA5.csv')

##### A3: Bednet data ###########################################################
df.bednets <- read_csv('/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_bednet_usage/bednet_data_20230907.csv') %>%
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
  mutate(bednet.stat.2wks = factor(bednet.stat.2wks, levels = c("No", "Yes (damaged)", "Yes"))) %>%
  #Dropping baseline
  filter(time_point != "T0") %>%
  #Making dichotomous (yes/no for 2 week recall)
  mutate(bednet.c = ifelse(bednet.stat.2wks == "No", 0, 1))
#Selecting only columns needed below
df.bednets.t <- df.bednets %>% dplyr::select(c(unique_ind_id, time_point, bednet.c))

##### Functions to clean data ###################################################

#Function: Temp mean finder
f.interval.temp <- function(start.date, end.date){
  df.Tm.trim <- df.Tm %>% filter(date >= ymd(start.date)) %>% filter(date <= ymd(end.date))
  mean.temp <- mean(df.Tm.trim$mean.temp.C)
  return(mean.temp)
}
#Function: Precip total finder
f.interval.precip <- function(start.date, end.date){
  df.Pm.trim <- df.Pm %>% filter(date >= ymd(start.date)) %>% filter(date <= ymd(end.date))
  mean.precip <- sum(df.Pm.trim$mean.precip.mm)
  return(mean.precip)
}

#Testing
f.interval.temp(start.date = "2021-01-01", end.date = "2021-01-02")
f.interval.precip(start.date = "2021-01-01", end.date = "2021-01-02")
df.Tm %>% filter(date >= "2021-01-01") %>% filter(date <= "2021-01-02")
df.Pm %>% filter(date >= "2021-01-01") %>% filter(date <= "2021-01-02")



##### Joining and tidying data ##################################################
df1 <- df1.i %>%
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
  #Ordering by average infection rate
  mutate(code.new = factor(code.new, levels = c("MNJ.06", "MNJ.04", "MNJ.03", "MNJ.05", "MNJ.02", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.01", "MNJ.10"))) %>%
  #cleaning up factors
  mutate(unique_ind_id = factor(unique_ind_id)) %>%
  #First 5 digits of the individual ID code give the site code + household code
  mutate(household = factor(substring(unique_ind_id, 1, 5))) %>%
  #Joining bednet data
  mutate(end.tp = case_when(
    interlude == "T0_T01"  ~ "T01",
    interlude == "T01_T02" ~ "T02",
    interlude == "T02_T03" ~ "T03",
    interlude == "T03_T04" ~ "T04",
    interlude == "T04_T05" ~ "T05",
    interlude == "T05_T06" ~ "T06",
    interlude == "T06_T07" ~ "T07",
    interlude == "T07_T08" ~ "T08",
    interlude == "T08_T09" ~ "T09",
    interlude == "T09_T10" ~ "T10")) %>%
  left_join(df.bednets.t, by = join_by(unique_ind_id, end.tp == time_point)) %>%
  #Dropping individuals with missing bednet data
  filter(!is.na(bednet.c)) %>%
  #Adding mean temp per interval
  rowwise() %>%
  mutate(temp.C = f.interval.temp(start.date = sample.date.1, end.date = sample.date.2)) %>%
  mutate(precip.mm = f.interval.precip(start.date = sample.date.1, end.date = sample.date.2))



#Date wrangler (chronological): Getting number of days in a month-year for a given interval

f.date.wrangler.chrono <- function(start.date, end.date){
  
  #counting days in a month for a start and end date
  v.interval <- seq(ymd(start.date), ymd(end.date), 1)
  #Project time line: Data collection started in July 2021 and ended in April 2023
  #2021
  days.in.2021.07 <- length(which(month(v.interval) == 7  & year(v.interval) == 2021))
  days.in.2021.08 <- length(which(month(v.interval) == 8  & year(v.interval) == 2021))
  days.in.2021.09 <- length(which(month(v.interval) == 9  & year(v.interval) == 2021))
  days.in.2021.10 <- length(which(month(v.interval) == 10 & year(v.interval) == 2021))
  days.in.2021.11 <- length(which(month(v.interval) == 11 & year(v.interval) == 2021))
  days.in.2021.12 <- length(which(month(v.interval) == 12 & year(v.interval) == 2021))
  #2022
  days.in.2022.01 <- length(which(month(v.interval) == 1  & year(v.interval) == 2022))
  days.in.2022.02 <- length(which(month(v.interval) == 2  & year(v.interval) == 2022))
  days.in.2022.03 <- length(which(month(v.interval) == 3  & year(v.interval) == 2022))
  days.in.2022.04 <- length(which(month(v.interval) == 4  & year(v.interval) == 2022))
  days.in.2022.05 <- length(which(month(v.interval) == 5  & year(v.interval) == 2022))
  days.in.2022.06 <- length(which(month(v.interval) == 6  & year(v.interval) == 2022))
  days.in.2022.07 <- length(which(month(v.interval) == 7  & year(v.interval) == 2022))
  days.in.2022.08 <- length(which(month(v.interval) == 8  & year(v.interval) == 2022))
  days.in.2022.09 <- length(which(month(v.interval) == 9  & year(v.interval) == 2022))
  days.in.2022.10 <- length(which(month(v.interval) == 10 & year(v.interval) == 2022))
  days.in.2022.11 <- length(which(month(v.interval) == 11 & year(v.interval) == 2022))
  days.in.2022.12 <- length(which(month(v.interval) == 12 & year(v.interval) == 2022))
  #2023
  days.in.2023.01 <- length(which(month(v.interval) ==  1 & year(v.interval) == 2023))
  days.in.2023.02 <- length(which(month(v.interval) ==  2 & year(v.interval) == 2023))
  days.in.2023.03 <- length(which(month(v.interval) ==  3 & year(v.interval) == 2023))
  days.in.2023.04 <- length(which(month(v.interval) ==  4 & year(v.interval) == 2023))
  
  #per month (without year)
  days.in.M01 <- length(which(month(v.interval) == 1))
  days.in.M02 <- length(which(month(v.interval) == 2))
  days.in.M03 <- length(which(month(v.interval) == 3))
  days.in.M04 <- length(which(month(v.interval) == 4))
  days.in.M05 <- length(which(month(v.interval) == 5))
  days.in.M06 <- length(which(month(v.interval) == 6))
  days.in.M07 <- length(which(month(v.interval) == 7))
  days.in.M08 <- length(which(month(v.interval) == 8))
  days.in.M09 <- length(which(month(v.interval) == 9))
  days.in.M10 <- length(which(month(v.interval) == 10))
  days.in.M11 <- length(which(month(v.interval) == 11))
  days.in.M12 <- length(which(month(v.interval) == 12))

  
  
  for(i in 1:length(v.my.years)){
    for(j in 1:12){
      
    }
  }
  
  
  #Creating a vector of all 365 days + a second year to allow interventions in late months to carry over
  #For standardization, using 2023-2024
  v.sequence.i <- seq(ymd("2023-01-01"), ymd("2024-12-31"), 1)
  
  #Timing interventions is an integer of the days after the start date where there are interventions
  #E.g., if the start date is Mar 01 and the interventions are 20, 40, 60 then there are interventions on
  #Mar01 + 20 = Mar21 ... etc
  #Note: must start with 1
  
  #Using mdy() to get a date format for start date
  start.date <- mdy(paste0(start.date.mo, "-", start.date.day, "-", "2023"))
  #Finding the index of the start date in the 2023-2024 sequence
  start.date.index <- which(v.sequence.i == start.date)
  
  
  #adding to a list
  days.in.M.l <- list(NA)
  for(i in 1:12){
    days.in.M.l[[i]] <- length(which(month(my.interval) == i))
  }
  return(days.in.M.l)
}



##### INCLUDING COVARIATES ##################################################

## Fit the force of infection with effects of age and site
## (and then time and household)

## Time: Fitting seasonality as a set of fixed effects using MGCV
## Household: Including random effect of household

#Fit: Smoothed age + Days in month 1-12 + site + offset log (interlude) + smoothed household
fit3 <- gam(rdt.result~s(age_yrs)+
              #s(temp)+
              days.in.MO1+days.in.MO2+days.in.MO3+days.in.MO4+days.in.MO5+days.in.MO6+
              days.in.MO7+days.in.MO8+days.in.MO9+days.in.M10+days.in.M11+days.in.M12+
              site_code+offset(log(interlude.days))+s(household, bs="re"),
            family=binomial(link="cloglog"), data=df)

summary(fit3)
gam.check(fit3)
## look at age pattern
par(mfrow=c(1,1))
plot(fit3)

## Plot month effects (subtracting one, because didn't do -1 in the fit above, so doing contrasts!)
plot(1:12, fit3$coeff[1]+fit3$coeff[2:13], xlab="month", ylab="FOI per day", pch=19)
for (j in 1:12) points(c(j,j),fit3$coeff[1]+fit3$coeff[j+1]+c(-1.96,1.96)*summary(fit3)$se[j+1],type="l",lty=3)




########################################################
#Models with temp and precip
#Household and temp (smooth)
fit4 <- gam(rdt.result~s(age_yrs)+
              s(temp.C)+
              days.in.MO1+days.in.MO2+days.in.MO3+days.in.MO4+days.in.MO5+days.in.MO6+
              days.in.MO7+days.in.MO8+days.in.MO9+days.in.M10+days.in.M11+days.in.M12+
              site_code+offset(log(interlude.days))+s(household, bs="re"),
            family=binomial(link="cloglog"), data=df)

#No household but temp (smooth)
fit4.small <- gam(rdt.result~s(age_yrs)+
                    s(temp.C)+
                    days.in.MO1+days.in.MO2+days.in.MO3+days.in.MO4+days.in.MO5+days.in.MO6+
                    days.in.MO7+days.in.MO8+days.in.MO9+days.in.M10+days.in.M11+days.in.M12+
                    site_code+offset(log(interlude.days)),
                  family=binomial(link="cloglog"), 
                  data=df1)


summary(fit4.small)
gam.check(fit4.small)
par(mfrow=c(1,1))
plot(fit4.small)

#No household but temp and precip
fit4.small.precip <- gam(rdt.result~s(age_yrs)+
                           s(temp.C)+s(precip.mm)+
                           days.in.MO1+days.in.MO2+days.in.MO3+days.in.MO4+days.in.MO5+days.in.MO6+
                           days.in.MO7+days.in.MO8+days.in.MO9+days.in.M10+days.in.M11+days.in.M12+
                           site_code+offset(log(interlude.days)),
                         family=binomial(link="cloglog"), 
                         data=df1)


summary(fit4.small.precip)
gam.check(fit4.small.precip)
par(mfrow=c(1,1))
plot(fit4.small.precip)

#Temp and precip as nonlinear predictors
fit4.t.precip <- gam(rdt.result~s(age_yrs)+
                       s(temp.C)+s(precip.mm)+
                       days.in.MO1+days.in.MO2+days.in.MO3+days.in.MO4+days.in.MO5+days.in.MO6+
                       days.in.MO7+days.in.MO8+days.in.MO9+days.in.M10+days.in.M11+days.in.M12+
                       site_code+offset(log(interlude.days))+s(household, bs="re"),
                     family=binomial(link="cloglog"), 
                     data=df1)


summary(fit4.t.precip)
gam.check(fit4.t.precip)
par(mfrow=c(1,1))
plot(fit4.t.precip)

#Temperature as a linear predictor
fit4.t.precip.l <- gam(rdt.result~s(age_yrs)+
                       temp.C+
                       days.in.MO1+days.in.MO2+days.in.MO3+days.in.MO4+days.in.MO5+days.in.MO6+
                       days.in.MO7+days.in.MO8+days.in.MO9+days.in.M10+days.in.M11+days.in.M12+
                       site_code+offset(log(interlude.days))+s(household, bs="re"),
                     family=binomial(link="cloglog"), 
                     data=df1)

summary(fit4.t.precip.l)
gam.check(fit4.t.precip.l)
par(mfrow=c(1,1))
plot(fit4.t.precip.l$linear.predictors)


#Full model: temperature, precip, bednets
fit5.t.p.net <- gam(rdt.result~s(age_yrs)+
                      s(temp.C)+s(precip.mm)+
                      bednet.c+
                      days.in.MO1+days.in.MO2+days.in.MO3+days.in.MO4+days.in.MO5+days.in.MO6+
                      days.in.MO7+days.in.MO8+days.in.MO9+days.in.M10+days.in.M11+days.in.M12+
                      site_code+s(household, bs="re")+
                      offset(log(interlude.days)),
                    family=binomial(link="cloglog"), 
                    data=df1)

summary(fit5.t.p.net)
gam.check(fit5.t.p.net)
par(mfrow=c(1,1))
plot(fit5.t.p.net)


gratia::draw(fit5.t.p.net)


#temperature, precip, bednets, NO Month
fit6 <- gam(rdt.result~s(age_yrs)+
              s(temp.C)+
              s(precip.mm)+
              bednet.c+
              #days.in.MO1+days.in.MO2+days.in.MO3+days.in.MO4+days.in.MO5+days.in.MO6+
              #days.in.MO7+days.in.MO8+days.in.MO9+days.in.M10+days.in.M11+days.in.M12+
              site_code+s(household, bs="re")+
              offset(log(interlude.days)),
            family=binomial(link="cloglog"), 
            data=df1)
summary(fit6)
gam.check(fit6)
par(mfrow=c(1,1))
plot(fit6)

#bednets and month; NO temp NO precip
fit7 <- gam(rdt.result~s(age_yrs)+
              #s(temp.C)+
              #s(precip.mm)+
              bednet.c+
              days.in.MO1+days.in.MO2+days.in.MO3+days.in.MO4+days.in.MO5+days.in.MO6+
              days.in.MO7+days.in.MO8+days.in.MO9+days.in.M10+days.in.M11+days.in.M12+
              site_code+s(household, bs="re")+
              offset(log(interlude.days)),
            family=binomial(link="cloglog"), 
            data=df1)
summary(fit7)
gam.check(fit7)
par(mfrow=c(1,1))
plot(fit7)


## Using predict to generate estimates #############################################################################################

#   Test case: 
#   Age: 13 year olds
#   Temp: Mean for April
#   Precip: mean for April
#   bednet: 0 (ie no)
#   Days in months 1-12: April 1 day
#   site_code: S6
#   household: S1.37 (standard)
#   interlude.days: 1 days

df.test1 <- tibble(
  age_yrs = 13,
  #temp.C = mean(df.Tm$mean.temp.C[month(df.Tm$date) == 4]),
  #temp.C = 25.6,
  #temp.C = seq(17, 30, by = 0.1),
  #precip.mm = sum(df.Pm$mean.precip.mm[month(df.Pm$date) == 4])/6,
  bednet.c = c(0, 1),
  days.in.MO1 = 0,
  days.in.MO2 = 0,
  days.in.MO3 = 0,
  days.in.MO4 = 30,
  days.in.MO5 = 0,
  days.in.MO6 = 0,
  days.in.MO7 = 0,
  days.in.MO8 = 0,
  days.in.MO9 = 0,
  days.in.M10 = 0,
  days.in.M11 = 0,
  days.in.M12 = 0,
  site_code = "S6",
  household = "S1.37",
  interlude.days = 30
)

pred.f5 <- predict(fit5.t.p.net,
                   newdata=df.test1,
                   se.fit=TRUE,
                   type="response")

pred.f3 <- predict(fit3,
                   newdata=df.test1 %>% dplyr::select(-c(temp.C, precip.mm, bednet.c)),
                   se.fit=TRUE,
                   type="response")

pred.f7 <- predict(fit7,
                   newdata=df.test1,
                   se.fit=TRUE,
                   type="response")



df.pred.f5 <- df.test1 %>% 
  mutate(fit = pred.f5$fit,
         se.fit = pred.f5$se.fit)
df.pred.f5$fit

df.pred.f3 <- df.test1 %>% 
  mutate(fit = pred.f3$fit,
         se.fit = pred.f3$se.fit)
df.pred.f3$fit

df.pred.f7 <- df.test1 %>% 
  mutate(fit = pred.f7$fit,
         se.fit = pred.f7$se.fit)
df.pred.f7$fit


df.pred.f5 %>% 
  ggplot(aes(x = temp.C, y = fit)) +
  geom_line() +
  scale_y_continuous(transform = "log10") +
  ylab("Proportion infected at t = 1 day") +
  xlab("Temperature (C)") +
  theme_bw()
  



## Age predictions ################################################
v.site_codes <- unique(df1$site_code)

df.test2 <- tibble(expand.grid(
  #Age: 1 to 70
  age_yrs = 1:70,
  #Bednets 0 and 1
  bednet.c = 0:1,
  days.in.MO1 = 31,
  days.in.MO2 = 0,
  days.in.MO3 = 0,
  days.in.MO4 = 0,
  days.in.MO5 = 0,
  days.in.MO6 = 0,
  days.in.MO7 = 0,
  days.in.MO8 = 0,
  days.in.MO9 = 0,
  days.in.M10 = 0,
  days.in.M11 = 0,
  days.in.M12 = 0,
  #Site code: All ten
  site_code = v.site_codes,
  household = "S1.37",
  interlude.days = 31
))

pred.age <- predict(fit7,
                   newdata=df.test2,
                   type="response")

df.pred.age <- df.test2 %>% 
  mutate(fit = as.numeric(pred.age))
#Export for plotting
# write_csv(df.pred.age, "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_foi/foi_age_exemplar 20250220.csv")








###############################################################################################
## D. WRITING FUNCTIONS TO SIMULATE AND PLOT ##################################################
###############################################################################################

## Coded as a function, so nominally could include any chosen fit - leverages R's 'predict' functions
## Running off the fit above

## First, a function to work with times/seasons using calendar dates
## Input: Start date month; start date day; timing of any interventions
## Create a vector of the number of days in month M.01 to M.12

###############################################################################################
#Function 1
#getting date intervals for a given start date and timing of interventions
f.date.intervals <- function(start.date.mo, start.date.day, timing.interventions){
  
  #Creating a vector of all 365 days + a second year to allow interventions in late months to carry over
  #For standardization, using 2023-2024
  v.sequence.i <- seq(ymd("2023-01-01"), ymd("2024-12-31"), 1)
  
  #Timing interventions is an integer of the days after the start date where there are interventions
  #E.g., if the start date is Mar 01 and the interventions are 20, 40, 60 then there are interventions on
  #Mar01 + 20 = Mar21 ... etc
  #Note: must start with 1
  
  #Using mdy() to get a date format for start date
  start.date <- mdy(paste0(start.date.mo, "-", start.date.day, "-", "2023"))
  #Finding the index of the start date in the 2023-2024 sequence
  start.date.index <- which(v.sequence.i == start.date)
  #Finding the end date (stop at last date of intervention)
  end.date.count <- max(timing.interventions)
  end.date <- ymd(v.sequence.i[start.date.index + end.date.count])
  end.date.index <- which(v.sequence.i == end.date)
  #Pulling out our interval of interest
  my.interval <- v.sequence.i[(start.date.index):end.date.index]
  
  return(my.interval)
}


###############################################################################################
## Second, a function to count the number of days in a given month for an interval
## E.g., for an interval for January 15 to Mar 2: 16 days in Jan, 28 days in Feb, 2 days in Mar
## Input: an interval sequence (e.g., Mar 15 to April 28)
## Output: A list of days per month example (list length = 12)
## List used to allow easy access when predicting

#Function 2
f.date.wrangler.v3 <- function(my.interval){
  
  #counting days per month for a vector that is a sequence of dates
  #adding to a list
  days.in.M.l <- list(NA)
  for(i in 1:12){
    days.in.M.l[[i]] <- length(which(month(my.interval) == i))
  }
  return(days.in.M.l)
}


###############################################################################################
## Third, a function to take a fitted model and predict; 
## with timing.interventions a vector giving what day(s) the interventions happened
##** scale.interventions not implemented

#Function 3
#Using a fitted model, intervention start date, intervention timing, and ages of interest
findYearTimeCourse.v3 <- function(fitted.model,           #fitted model from above
                                  start.date.mo,          #Integer from 1:12
                                  start.date.day,         #Integer from 1:31 (or last day of month)
                                  timing.interventions,   #e.g., c(1, 30, 60)
                                  my.ages,                #Integers of age in years
                                  scale.interventions){   #scale.interventions not used yet
  
  #** month a legacy
  
  #Making a vector of site codes
  sites <- c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5")
  ages <- my.ages
  #Passing my start date to the date.interval function to get the interval as a sequence of dates
  my.interval <- f.date.intervals(start.date.mo, start.date.day, timing.interventions)
  
  #time steps evaluated
  tvals <- 1:(max(timing.interventions))
  intervention <- rep(0,length(tvals))
  #noting which days in the sequence have interventions
  intervention[timing.interventions] <- 1
  
  #** make month continuous to smooth out prediction
  month <- rep(seq(1,12,length=30*12),3)[1:length(tvals)]
  
  # storage
  pred <- se.pred <- array(dim=c(length(sites),length(ages),max(timing.interventions)))
  
  #Loop through each site
  for (s in 1:length(sites)) {
    #Loop through each age
    for (a in 1:length(ages)) {
      #Loop through each day of the segments of the intervention sequence
      #E.g., if interventions on day 1, day 50, and day 100 then
      #   run from day 1 to day 50, restart on day 51 and run to day 100, restart on day 101... then bind together
      for (j in 2:length(timing.interventions)){
        #Offsetting by 1 in indices to deal with start day
        interval.days.indices <- tvals[timing.interventions[j-1]:timing.interventions[j]]-tvals[timing.interventions[j-1]]+1
        interval.days <- my.interval[interval.days.indices]
        #Passing the interval segment to date wrangler function to get days per month
        days.in.M.l <- f.date.wrangler.v3(interval.days)
        #Making a temp data frame with the ages, days in months, site code, etc to pass to predict function
        newData <- data.frame(age_yrs = ages[a],
                              ##* Is this mid.month used?
                              mid.month=month[timing.interventions[j-1]:timing.interventions[j]],
                              site_code=sites[s],
                              days.in.MO1=days.in.M.l[[1]],
                              days.in.MO2=days.in.M.l[[2]],
                              days.in.MO3=days.in.M.l[[3]],
                              days.in.MO4=days.in.M.l[[4]],
                              days.in.MO5=days.in.M.l[[5]],
                              days.in.MO6=days.in.M.l[[6]],
                              days.in.MO7=days.in.M.l[[7]],
                              days.in.MO8=days.in.M.l[[8]],
                              days.in.MO9=days.in.M.l[[9]],
                              days.in.M10=days.in.M.l[[10]],
                              days.in.M11=days.in.M.l[[11]],
                              days.in.M12=days.in.M.l[[12]],
                              household=as.factor("S1.37"), #blandest available household, random effect close to 0
                              interlude.days=tvals[timing.interventions[j-1]:timing.interventions[j]]-tvals[timing.interventions[j-1]]+1)
        
        tmp <- predict(fitted.model, newdata=newData, se=TRUE, type="response")
        pred[s,a,timing.interventions[j-1]:timing.interventions[j]] <- tmp$fit
        se.pred[s,a,timing.interventions[j-1]:timing.interventions[j]] <- tmp$se.fit
      }
    }
  }
  
  return(list(pred=pred, se.pred=se.pred, timing.interventions=timing.interventions, month=month, ages=ages))

}

###############################################################################################
## Fourth, a function to take a prediction output and make a plottable data frame

#Function 4
f.plot.tpp <- function(my.start.date.mo,
                       my.start.date.day,
                       my.timing.interventions,
                       sites.to.plot,
                       my.ages){
  #Params
  # my.start.date.mo        : an integer 1:12
  # my.start.date.day       : an integer 1:31 (or last day of that month)
  # my.timing.interventions : a sequence of counting since start date
  #                           Note: Must start with 1
  #                           E.g., c(1, 90) or c(1, 30, 60, 90)
  # my.ages                 : sequence of integers: ages in years for which to simulate
  #                           (e.g, 1:20 or c(2, 5, 15, 25))
  # sites.to.plot           : the subset of sites to plot
  #                           (e.g, c("S1", "S2"))
  
  #Calling the prediction function from above
  tpp <- findYearTimeCourse.v3(
    fitted.model = fit3,
    start.date.mo = my.start.date.mo,
    start.date.day = my.start.date.day,
    timing.interventions = my.timing.interventions,
    my.ages = my.ages,
    scale.interventions=NULL
  )
  
  sites <- c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5")
  sites.indices <- which(sites %in% sites.to.plot)
  
  #loop-loop then rowbind for each site and each age
  
  #tpp structure: tpp[site, age, day]
  
  #creating an empty list to hold temp dfs for each site
  l.df <- list(NA)
  
  for(i in 1:length(sites.indices)){
    
    #creating an empty list to hold temp dfs for each age
    l.df.i <- list(NA)
    
    for(j in 1:length(my.ages)){
      mydf.j <- tibble(day       = 1:length(tpp$pred[sites.indices[i], j, ]),
                       site_code = sites[sites.indices[i]],
                       age       = my.ages[j],
                       prob.inf  = tpp$pred[sites.indices[i], j, ])
      l.df.i[[j]] <- mydf.j
      df.i <- bind_rows(l.df.i)
    }
    l.df[[i]] <- df.i
  }

  df.p <- bind_rows(l.df)
  
  return(df.p)
}



###############################################################################################
## E. FROM REGRESSIONS START SIMULATING INFECTION PROBS UNDER  DISRUPTIONS ####################
###############################################################################################

#Creating data frames, exporting as csv files for downstream scripts that plot

## Figure S2: FOI over age ####################################################################

#FOI for the 12 months
df.1.01 <- f.plot.tpp(my.start.date.mo =  1, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.02 <- f.plot.tpp(my.start.date.mo =  2, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.03 <- f.plot.tpp(my.start.date.mo =  3, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.04 <- f.plot.tpp(my.start.date.mo =  4, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.05 <- f.plot.tpp(my.start.date.mo =  5, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.06 <- f.plot.tpp(my.start.date.mo =  6, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.07 <- f.plot.tpp(my.start.date.mo =  7, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.08 <- f.plot.tpp(my.start.date.mo =  8, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.09 <- f.plot.tpp(my.start.date.mo =  9, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.10 <- f.plot.tpp(my.start.date.mo = 10, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.11 <- f.plot.tpp(my.start.date.mo = 11, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.1.12 <- f.plot.tpp(my.start.date.mo = 12, my.start.date.day = 1, my.timing.interventions = c(1, 1), my.ages = 1:99, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))

#Adding a column for each month and binding the 12 months together
df.1.01 <- df.1.01 %>% mutate(month = "JAN")
df.1.02 <- df.1.02 %>% mutate(month = "FEB")
df.1.03 <- df.1.03 %>% mutate(month = "MAR")
df.1.04 <- df.1.04 %>% mutate(month = "APR")
df.1.05 <- df.1.05 %>% mutate(month = "MAY")
df.1.06 <- df.1.06 %>% mutate(month = "JUN")
df.1.07 <- df.1.07 %>% mutate(month = "JUL")
df.1.08 <- df.1.08 %>% mutate(month = "AUG")
df.1.09 <- df.1.09 %>% mutate(month = "SEP")
df.1.10 <- df.1.10 %>% mutate(month = "OCT")
df.1.11 <- df.1.11 %>% mutate(month = "NOV")
df.1.12 <- df.1.12 %>% mutate(month = "DEC")

df.foi.by.age <- rbind(df.1.01, df.1.02, df.1.03, df.1.04, df.1.05, df.1.06, df.1.07, df.1.08, df.1.09, df.1.10, df.1.11, df.1.12) %>% 
  mutate(month = factor(month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))) %>%
  mutate(site_code = factor(site_code, levels = c("S1", "N2", "N3", "N1", "N4", "S2", "S3", "S5", "N5", "S6"))) %>%
  mutate(age_cat = case_when(
    age >   0 & age <  6 ~ "Young children",
    age >=  6 & age < 14 ~ "School aged children",
    age >= 14 & age < 20 ~ "Teenagers",
    age >= 20            ~ "Adults")) %>%
  mutate(age_cat = factor(age_cat, levels = c("Young children", "School aged children", "Teenagers", "Adults"))) %>%
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

#Exporting for use in plotting
# write_csv(df.foi.by.age, "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/output/foi_by_age.csv")



## Figure 2C: FOI per month by age group and site ####################################################################

#FOI for 30 days for the 12 months
df.2.01 <- f.plot.tpp(my.start.date.mo =  1, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.02 <- f.plot.tpp(my.start.date.mo =  2, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.03 <- f.plot.tpp(my.start.date.mo =  3, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.04 <- f.plot.tpp(my.start.date.mo =  4, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.05 <- f.plot.tpp(my.start.date.mo =  5, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.06 <- f.plot.tpp(my.start.date.mo =  6, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.07 <- f.plot.tpp(my.start.date.mo =  7, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.08 <- f.plot.tpp(my.start.date.mo =  8, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.09 <- f.plot.tpp(my.start.date.mo =  9, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.10 <- f.plot.tpp(my.start.date.mo = 10, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.11 <- f.plot.tpp(my.start.date.mo = 11, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.2.12 <- f.plot.tpp(my.start.date.mo = 12, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))

#binding the 12 months together
df.2.01 <- df.2.01 %>% mutate(month = "JAN")
df.2.02 <- df.2.02 %>% mutate(month = "FEB")
df.2.03 <- df.2.03 %>% mutate(month = "MAR")
df.2.04 <- df.2.04 %>% mutate(month = "APR")
df.2.05 <- df.2.05 %>% mutate(month = "MAY")
df.2.06 <- df.2.06 %>% mutate(month = "JUN")
df.2.07 <- df.2.07 %>% mutate(month = "JUL")
df.2.08 <- df.2.08 %>% mutate(month = "AUG")
df.2.09 <- df.2.09 %>% mutate(month = "SEP")
df.2.10 <- df.2.10 %>% mutate(month = "OCT")
df.2.11 <- df.2.11 %>% mutate(month = "NOV")
df.2.12 <- df.2.12 %>% mutate(month = "DEC")

df.monthly.day.foi <- rbind(df.2.01, df.2.02, df.2.03, df.2.04, df.2.05, df.2.06, df.2.07, df.2.08, df.2.09, df.2.10, df.2.11, df.2.12) %>% 
  mutate(month = factor(month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))) %>%
  mutate(site_code = factor(site_code, levels = c("S1", "N2", "N3", "N1", "N4", "S2", "S3", "S5", "N5", "S6"))) %>%
  mutate(age_cat = case_when(
    age >   0 & age <  6 ~ "Young children",
    age >=  6 & age < 14 ~ "School aged children",
    age >= 14 & age < 20 ~ "Teenagers",
    age >= 20            ~ "Adults")) %>%
  mutate(age_cat = factor(age_cat, levels = c("Young children", "School aged children", "Teenagers", "Adults"))) %>%
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

#Exporting for use in plotting
# write_csv(df.monthly.day.foi, "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/output/foi_monthly.csv")




## Figure 2D: FOI for 90 days following two storms ####################################################################

#Cyclone Batsirai (BATS):  5 February
#Cyclone Freddy (FRED:    21 February
df.BATS <- f.plot.tpp(my.start.date.mo =  2, my.start.date.day =  5, my.timing.interventions = c(1, 90), my.ages = 1:50, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
df.FRED <- f.plot.tpp(my.start.date.mo =  2, my.start.date.day = 21, my.timing.interventions = c(1, 90), my.ages = 1:50, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))

#binding the 12 months together
df.BATS <- df.BATS %>% mutate(storm = "CYCLONE BATSIRAI")
df.FRED <- df.FRED %>% mutate(storm = "CYCLONE FREDDY")

df.storm.day.foi <- rbind(df.BATS, df.FRED) %>% 
  mutate(storm = factor(storm, levels = c("CYCLONE BATSIRAI", "CYCLONE FREDDY"))) %>%
  mutate(site_code = factor(site_code, levels = c("S1", "N2", "N3", "N1", "N4", "S2", "S3", "S5", "N5", "S6"))) %>%
  mutate(age_cat = case_when(
    age >   0 & age <  6 ~ "Young children",
    age >=  6 & age < 14 ~ "School aged children",
    age >= 14 & age < 20 ~ "Teenagers",
    age >= 20            ~ "Adults")) %>%
  mutate(age_cat = factor(age_cat, levels = c("Young children", "School aged children", "Teenagers", "Adults"))) %>%
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

#Exporting for use in plotting
# write_csv(df.storm.day.foi, "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/output/foi_post_storm.csv")






##############################################################################################################################
#FOR DECAY FIGURE
##############################################################################################################################
#FOI for 2 years for an exemplar start date (Jan 1) and exemplar age (age = 1 y)
df.foi.decay <- f.plot.tpp(my.start.date.mo =  1, my.start.date.day = 1, my.timing.interventions = c(1, 365*2), my.ages = 1, sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5"))
#Exporting for use in plotting
#write_csv(df.foi.decay, "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/foi_decay_exemplar.csv")




##############################################################################################################################
#FOI for 20 days for March 1st
df.MAR <- f.plot.tpp(my.start.date.mo =  3, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13), sites.to.plot = c("S5", "S6", "N5")) %>%
  filter(day %in% c(15, 20, 30))

df.APR <- f.plot.tpp(my.start.date.mo =  4, my.start.date.day = 1, my.timing.interventions = c(1, 30), my.ages = c(5, 13), sites.to.plot = c("S5", "S6", "N5")) %>%
  filter(day %in% c(15, 20, 30))


## Figure 3C: Plotting return times ####################################################################

## Scenario: March 1st for 254 days (sufficiently long time series)
df.RT.plotter <- f.plot.tpp(my.start.date.mo =  3, my.start.date.day = 1, my.timing.interventions = c(1, 254),  my.ages = c(5, 13, 18, 27), sites.to.plot = c("S1", "S2", "S3", "S5", "S6", "N1", "N2", "N3", "N4", "N5")) %>%
  filter(day == 1) %>%
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
    site_code == "N5" ~ "MNJ.01"))

#Exporting for use in plotting
# write_csv(df.RT.plotter, "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/R/modeling/output/foi_for_return_time_plotting.csv")


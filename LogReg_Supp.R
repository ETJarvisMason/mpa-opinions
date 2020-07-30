# Analyses in Mason et al.,
# "Long-term Participation in Collaborative Fisheries Research Improves 
#   Volunteer Angler Opinions on Marine Protected Area Performance" 

# Multinomial logistic regression - MPAs ----------------------------------
# test for opinion change of CCFRP volunteers on MPAs
# across different levels of volunteer participation

# Three measures of volunteer participation:
# 1) Years since joining CCFRP,
# 2) Number of sampling trips attended,
# 3) Number of Volunteer Appreciation and Data Workshops.

library(haven) # read spss files
library(tidyverse) # dplyr and ggplot
library(nnet) # multinom function
library(car) # Anova table on multinom object
library(broom) # test individual effects
library(MNLpred) # predicted values

# read in anonymous Qualtrix survey responses
df <- read_sav("survdata.sav")
names(df)

# select and compute variables of interest:
# YrStart = year respondent became a CCFRP volunteer
# YrsVol = number of years respondent spent volunteering for CCFRP
# Tpy = number of CCFRP sampling trips respondent reported going on per year
# Evnts = number of Volunteer Angler Appreciation and Data Workshops respondent 
# reported attending
mydata <- df %>% 
  dplyr::select("Q1","Q2","Q3","Q6A", "Q11", "Q14_1","Q18_1") %>% 
  rename(YrStart = "Q1", YrsVol = "Q2", Tpy = "Q3", Evnts = "Q6A",
         Before = "Q14_1", After = "Q18_1") %>% 
  filter(YrsVol > 0) %>% # filter respondents who have volunteered 
  drop_na(Before) %>% 
  mutate(Change = Before-After) %>%
  mutate(OpinChange = case_when(
    Change > 0 ~ 1, # Positive change
    Change < 0 ~ 3, # Negative change
    Change == 0 ~ 2)) %>% # No change
  drop_na(OpinChange) %>%
  mutate(TotTrps = as.numeric(YrsVol) * as.numeric(Tpy)) 

lr <- mydata %>% # format variables for multiple logistic regression
  mutate(OpinChange = as.factor(OpinChange)) %>% 
  mutate(NumYrStart = as.numeric(YrStart)) %>% 
  mutate(NumEvnts = as.numeric(Evnts)) %>% 
  mutate(NumEvnts = replace_na(NumEvnts, 0)) %>% # NA = 0 volunteer events attended
  filter(NumYrStart > 0) %>%
  drop_na(TotTrps) %>% 
  mutate(YrsSince = 2018 - NumYrStart) %>% 
  mutate(YrsSince = replace(YrsSince, YrsSince > 11,11)) %>% # max number of volunteer years = 11 
  dplyr::select(OpinChange,YrsSince,TotTrps,NumEvnts)

# run full multinomial logistic regression model (all variables)
# default reference category is 1 if y is based on numeric factors
# therefore, reference category is Positive Change (in opinion)
mymod <- multinom(OpinChange~YrsSince+TotTrps+NumEvnts, 
                  data = lr,
                  Hess = TRUE,
                  na.action = na.exclude) 

# test model fit
lrt <- Anova(mymod, type="II") # Anova table of likelihood ratio tests
lrt

# summary of model output
summary(mymod)

# predicted values
# https://cran.r-project.org/web/packages/MNLpred/vignettes/OVA_Predictions_For_MNL.html
preds <- mnl_pred_ova(model = mymod,
                      data = lr,
                      xvari = "YrsSince",
                      by = 0.25,
                      seed = "525625", 
                      nsim = 1000, # default
                      probs = c(0.025, 0.975)) # default

# label opinion change categories
library(scales)
OC_cat <- c(
  "1" = "Positive Change",
  "2" = "No Change",
  "3" = "Negative Change")

# determine how long before predicted probability of positive change in opinion >= 50%
y.50 <- which(preds$plotdata[1:26,3] >= 0.50) # y vals >= 50%
x.50 <- first(y.50) # index of x val where first y val >= 50%
x.val <- preds$plotdata[x.50,1] # coressponding x val

# plot predicted probabilites across opin. change categories
plot <- ggplot(data = preds$plotdata, aes(x = YrsSince, y = mean,
                                  ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1) + # Confidence intervals
  geom_line() + # Mean
  facet_wrap(OpinChange ~.,
             labeller = labeller(OpinChange = OC_cat)) +
  geom_segment(aes(x = x.val, y = .5, xend = 0, yend = .5), 
               linetype = "dashed",data = preds$plotdata[1:26,])+
  geom_segment(aes(x = x.val, y = .5, xend = x.val, yend = 0), 
               linetype = "dashed",data = preds$plotdata[1:26,])+
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # % labels
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))+
  labs(y = "Predicted probability",
       x = "Years since joining CCFRP")


# Logistic regression - Data Quality --------------------------------------
# test for opinion change of CCFRP volunteers on the
# data quality used in resource management
# across different levels of volunteer participation

# Three measures of volunteer participation:
# 1) Years since joining CCFRP,
# 2) Number of sampling trips attended,
# 3) Number of Volunteer Appreciation and Data Workshops.

# select and compute variables of interest:
# OpinDQ = opinion of data quality?
# ChangeDQ = of those with opinion, did it change?
# CngType = how did opinion change?
# DQOpinChange = positive or negative
mydata_dq <- df %>% 
  dplyr::select("Q1","Q2","Q3","Q5","Q6A", "Q11", "Q11A","Q11B_1","Q14_1","Q18_1") %>% 
  rename(YrStart = "Q1", YrsVol = "Q2", Tpy = "Q3", VolAgain = "Q5", Evnts = "Q6A",
         OpinDQ = "Q11", ChangeDQ = "Q11A", ChngType = "Q11B_1",
         Before = "Q14_1", After = "Q18_1") %>% 
  filter(YrsVol > 0) %>% # filter respondents who have volunteered 
  mutate(DQOpinChange = case_when(
    OpinDQ > 1 ~ 4, # No opinion before
    ChngType == 1 | ChngType == 2 ~ 1, # Positive change
    ChangeDQ == 2 ~ 0, # No change
    ChngType > 2 ~ 2)) %>% # Negative change
  drop_na(DQOpinChange) %>% 
  filter(DQOpinChange < 2) %>% # filter out No opinion before; category Negative change excluded for analysis below because n = 1
  mutate(TotTrps = as.numeric(YrsVol) * as.numeric(Tpy)) 

lr_dq <- mydata_dq %>% # format variables for logistic regression
  mutate(DQOpinChange = as.factor(DQOpinChange)) %>%  
  mutate(NumYrStart = as.numeric(YrStart)) %>%
  mutate(NumEvnts = as.numeric(Evnts)) %>% 
  mutate(NumEvnts = replace_na(NumEvnts, 0)) %>% # NA = 0 volunteer events attended
  filter(NumYrStart > 0) %>%
  drop_na(TotTrps) %>% 
  mutate(YrsSince = 2018 - NumYrStart) %>% 
  mutate(YrsSince = replace(YrsSince, YrsSince > 11,11)) %>% # max number of volunteer years = 11 
  dplyr::select(DQOpinChange,YrsSince,TotTrps,NumEvnts)

# run binomial logistic regression model
# Test influence of different measures of volunteer participation
# on having a Positive Opinion Change (=1) vs No Opinion Change (=0) 
# NOTE:  The no opinion change category lacks information on what the opinion was before CCFRP participation 

library(stats) # run generalized linear model (glm) function
mymodDQ <- glm(DQOpinChange~., 
               family = binomial(link = "logit"),
               data = lr_dq) 

# test model fit
lrt_dq <- Anova(mymodDQ, type="II") # Anova table of likelihood ratio tests
lrt_dq

# summary of model output
summary(mymodDQ)


# a posteriori analysis (Discussion) --------------------------------------


# How do different levels of CCFRP participation relate to 
# a volunteer's willingness to continue volunteering?
# VolAgain = will respondents participate again?
mydata_vol <- df %>% 
  dplyr::select("Q1","Q2","Q3","Q5","Q6A", "Q11", "Q11A","Q11B_1","Q14_1","Q18_1") %>% 
  rename(YrStart = "Q1", YrsVol = "Q2", Tpy = "Q3", VolAgain = "Q5", Evnts = "Q6A",
         OpinDQ = "Q11", ChangeDQ = "Q11A", ChngType = "Q11B_1",
         Before = "Q14_1", After = "Q18_1") %>% 
  mutate(TotTrps = as.numeric(YrsVol) * as.numeric(Tpy)) 

lr_vol <- mydata_vol %>% # format variables for logistic regression
  mutate(NumYrStart = as.numeric(YrStart)) %>%
  mutate(NumEvnts = as.numeric(Evnts)) %>% 
  mutate(NumEvnts = replace_na(NumEvnts, 0)) %>% # NA = 0 volunteer events attended
  filter(NumYrStart > 0) %>%
  drop_na(TotTrps) %>% 
  mutate(YrsSince = 2018 - NumYrStart) %>%
  mutate(YrsSince = replace(YrsSince, YrsSince > 11,11)) %>% # max number of volunteer years = 11 
  mutate(Return = case_when(
    VolAgain == 1 ~ 1, # Yes
    VolAgain  == 2 ~ 0)) %>% # No
  dplyr::select(Return,YrsSince,TotTrps,NumEvnts)

# run binomial logistic regression model
# Test influence of different measures of volunteer participation
# on a volunteer's willingness to continue participating (YES=1, NO =0) 
mymodvol <- glm(Return~., 
               family = binomial(link = "logit"),
               data = lr_vol)

# test model fit
lrt_vol <- Anova(mymodvol, type="II") # Anova table of likelihood ratio tests
lrt_vol

# summary of model output
summary(mymodvol)
# only the intercept is significant

# rerun model with only an intercept (no predictor vars)
mymodvol2 <- glm(Return~1, 
                family = binomial(link = "logit"),
                data = lr_vol)

# summary of model output
summary(mymodvol2)

# So p = 100/108 = 0.926 (i.e. the overall probability of continuing to volunteer). 
# The odds are 0.926/(1-0.926) = 12.514 and the log of the odds (logit) is logit(0.926) = 2.526.
# The intercept for the model with no predictor variables is the estimated log odds of continuing to volunteer with CCFRP for the survey respondents.
# To trasform the log of the odds back to a probability: p = exp(2.526)/(1+exp(2.526)) = 0.926.

# let's get 95% CI for overall probability
library(MASS) # calculate 95% CI (MLE profile intervals)
library(boot) # transform back to probability space
inv.logit(confint(mymodvol2))

# overall 95% CI: 87 to 97% probability of returning

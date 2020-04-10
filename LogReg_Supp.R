# Multinomial logistic regression
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

# read in Qualtrix survey responses
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
  drop_na(Before) %>% 
  filter(Before > 0) %>% 
  mutate(Change = Before-After) %>%
  mutate(OpinChange = case_when(
    Change > 0 ~ 1, # Positive change
    Change < 0 ~ 3, # Negative change
    Change == 0 ~ 2)) %>% # No change
  drop_na(OpinChange) %>%
  mutate(TotTrps = as.numeric(YrsVol) * as.numeric(Tpy)) 

lr <- mydata %>% # format variables for logistic regression
  mutate(OpinChange = as.factor(OpinChange)) %>% 
  mutate(NumYrStart = as.numeric(YrStart)) %>%
  mutate(NumEvnts = as.numeric(Evnts)) %>% 
  mutate(NumEvnts = replace_na(NumEvnts, 0)) %>% # NA = 0 volunteer events attended
  filter(NumYrStart > 0) %>%
  # drop_na(TotTrps) %>% 
  mutate(YrsSince = 2018 - NumYrStart) %>% 
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

# individual effects
effects <- tidy(lrt, conf.int = TRUE)
effects

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
y.50 <- which(preds$plotdata[1:73,3] >= 0.50) # y vals >= 50%
x.50 <- first(y.50) # index of x val where first y val >= 50%
x.val <- preds$plotdata[x.50,1] # coressponding x val

# plot predicted probabilites across opin. change categories
ggplot(data = preds$plotdata, aes(x = YrsSince, y = mean,
                                  ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1) + # Confidence intervals
  geom_line() + # Mean
  facet_wrap(OpinChange ~.,
             labeller = labeller(OpinChange = OC_cat)) +
  geom_segment(aes(x = 7.25, y = .5, xend = 0, yend = .5), linetype = "dashed",data = preds$plotdata[1:73,])+
  geom_segment(aes(x = 7.25, y = .5, xend = 7.25, yend = 0), linetype = "dashed",data = preds$plotdata[1:73,])+
  
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

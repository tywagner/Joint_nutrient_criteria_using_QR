library(tidyverse) # tidyverse is for data input here
library(quantreg)   # the R package for quantile regression
d = read_csv(file = "../DC_QR.csv") #data input

# filter the raw data
TP_upper = quantile(d$TP, 0.9)
TP_lower = quantile(d$TP, 0.1)
TN_upper = quantile(d$TN, 0.9)
TN_lower = quantile(d$TN, 0.1)
df = d %>%
  dplyr::select(TP, TN, CHL) %>%
  filter(TP < TP_upper, TP > TP_lower,
         TN < TN_upper, TN > TN_lower)  


# quantile regression, tau is the regression quantile
#tau_all = c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
tau = 0.5 
f1 = rq(CHL ~ TP * TN, data = df, tau = tau)
f2 = rq(CHL ~ TP * TN - 1, data = df, tau = tau)
f3 = rq(CHL ~ TP + TN - 1, data = df, tau = tau)
f4 = rq(CHL ~ TP - 1, data = df, tau = tau)
f5 = rq(CHL ~ TN - 1, data = df, tau = tau)

# model selection based on sequential Wald test using anova function
anova(f1, f2, f3, f4, f5)
anova(f2, f3, f4, f5)
anova(f3, f4, f5)

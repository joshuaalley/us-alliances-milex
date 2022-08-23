# Joshua Alley & Matthew Fuhrmann
# Webb et al Bounds Approach with ADL model


# the bounds approach is especially helpful 
# because drift and trend results are all over the place


###  Test for unit root
# kpss test for outcome: stationary in first differences
summary(ur.kpss(us.data$milex_SI))
summary(ur.kpss(us.data$change_milex_SI))

# dickey-fuller
summary(ur.df(us.data$milex_SI[!is.na(us.data$milex_SI)]))
# add drift
summary(ur.df(us.data$milex_SI[!is.na(us.data$milex_SI)], type = "drift"))
# add trend
summary(ur.df(us.data$milex_SI[!is.na(us.data$milex_SI)], type = "trend"))
summary(ur.df(us.data$change_milex_SI[!is.na(us.data$change_milex_SI)]))


# phillips-perron
summary(ur.pp(us.data$milex_SI))
summary(ur.pp(us.data$change_milex_SI)) 
# add trend
summary(ur.pp(us.data$milex_SI, model = "trend"))
summary(ur.pp(us.data$change_milex_SI, model = "trend")) 




### Fit models
# ADL here 
lm.adl <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
               peace_5 + log_fatalities_combined +
               lag_rep_pres + cold_war +
               lag_budget_deficit + lag_change_gdp +
               lag_mprival_milex,
              data = us.data)
reg.check(lm.adl)
summary(lm.adl)
# diagnose and print results
qqnorm(lm.adl$residuals)
qqline(lm.adl$residuals)
adl.res <- summary(lm.adl)


# Test the LRM
# 11 coefficients and 69 time points
# for alliances
lrm.adl <- (adl.res$coefficients[3]) /(1 -  adl.res$coefficients[2])
lrm.adl
deltamethod(~ (x3) / (1 - x2), coef(lm.adl), vcov(lm.adl))
# # 95% upper bound of LRM
lrm.adl + (2 * deltamethod(~ (x3) / (1 - x2), coef(lm.adl), vcov(lm.adl)))
# # 95% lower bound of LRM
lrm.adl - (2 * deltamethod(~ (x3) / (1 - x2), coef(lm.adl), vcov(lm.adl)))
# 
# T-stat:   
lrm.adl / deltamethod(~ (x3) / (1 - x2), coef(lm.adl), vcov(lm.adl))


# in logs of commitments
lm.adl.log <- lm(milex_SI ~ lag_milex_SI + lag_ln_commitments + 
                   peace_5 + log_fatalities_combined +
                   lag_rep_pres + cold_war +
                   lag_budget_deficit + lag_change_gdp +
                   lag_mprival_milex,
             data = us.data,
)
summary(lm.adl.log)
reg.check(lm.adl.log)
# diagnose and print results
adl.res.log <- summary(lm.adl.log)
adl.res.log$coefficients[3]*log(1.1)


# Test the LRM
# 11 coefficients and 69 time points
# for alliances
lrm.adl.log <- (adl.res.log$coefficients[3]) / 
                        (1 -  adl.res.log$coefficients[2])
lrm.adl.log
deltamethod(~ (x3) / (1 - x2), coef(lm.adl.log), vcov(lm.adl.log))
# T-stat:   
lrm.adl.log / deltamethod(~ (x3) / (1 - x2), coef(lm.adl.log), vcov(lm.adl.log))


# calculate LRM in logs
lrm.adl.log*log(1.1)
# 95% upper bound of LRM in logs
(lrm.adl.log + (2 * deltamethod(~ x3 / (1 - x2), coef(lm.adl.log), 
                                 vcov(lm.adl.log))))*log(1.1)
# 95% lower bound of LRM in logs
(lrm.adl.log - (2 * deltamethod(~ x3 / (1 - x2), coef(lm.adl.log), 
                                 vcov(lm.adl.log))))*log(1.1)



# Tabulate results
stargazer(lm.adl, lm.adl.log,
          omit.table.layout = "m",
          style = "apsr",
          dep.var.labels=c("Military Spending"),
          omit = c("dec"),
          order = c("lag_milex_SI",
                    "lag_commitments","lag_ln_commitments", 
                    "lag_change_gdp", 
                    "lag_mprival_milex", "cold_war",
                    "peace_5", "log_fatalities_combined",
                    "lag_rep_pres", "lag_budget_deficit"),
          covariate.labels=c("Lagged Military Spending",
                             "Lag Alliance Commitments",
                             "Lag Log Alliance Commitments", 
                             "Lag Change in GDP",
                             "Lag Major Rival Spending", "Cold War",
                             "Post-Conflict Years", "Log Combat Fatalities",
                             "Lag Republican President", 
                             "Lag Budget Deficit"
          ),
          keep.stat = c("n"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:adl-coefs")
)




### Robustness checks in paper



# Estimate model with data from 1954 on
fit.55 <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
               peace_5 + log_fatalities_combined +
               lag_rep_pres + cold_war +
               lag_budget_deficit + lag_change_gdp +
               lag_mprival_milex,
                  data = us.data.1955)

# diagnose and print results
reg.check(fit.55)
summary(fit.55)

# GWOT (post 9-11 dummy)
fit.911 <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                peace_5 + log_fatalities_combined +
                lag_rep_pres + cold_war + 
                lag_budget_deficit + lag_change_gdp +
                lag_mprival_milex +
                     post_911,
                   data = us.data)
# diagnose and print results
reg.check(fit.911)
summary(fit.911)


# 52 and before dummy
fit.p52 <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                peace_5 + log_fatalities_combined +
                lag_rep_pres + cold_war + 
                lag_budget_deficit + lag_change_gdp +
                lag_mprival_milex +
                     pre_52, 
                   data = us.data)
# diagnose and print results
reg.check(fit.p52)
summary(fit.p52)


# both atheoretical time dummies
fit.tdum <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                 peace_5 + log_fatalities_combined +
                 lag_rep_pres + cold_war + 
                 lag_budget_deficit + lag_change_gdp +
                 lag_mprival_milex +
                      pre_52 + post_911, 
                    data = us.data)
# diagnose and print results
reg.check(fit.tdum)
summary(fit.tdum)


# Estimate model with Presidential FE
fit.pres <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                 peace_5 + log_fatalities_combined +
                 lag_budget_deficit + lag_change_gdp +
                 lag_mprival_milex +
                       Carter + Clinton + Eisenhower + Ford +
                       HW.Bush + Johnson + Kennedy + Nixon +
                       Reagan + Truman + Trump + W.Bush,
                     data = us.data)
# diagnose and print results
reg.check(fit.pres)
summary(fit.pres)




# Create a figure w/ long-run multipliers
lrm.rob <- cbind.data.frame(
  rbind("Sample: 1955-2019", "Presidential Fixed Effects", "Post 9/11 dummy",
        "Pre 1953 Dummy", "Post 9/11 and Pre 1953"), 
  rbind(coef(fit.55)[3] / (1 - coef(fit.55)[2]), 
        coef(fit.pres)[3] / (1 - coef(fit.pres)[2]),
        coef(fit.911)[3] / (1 - coef(fit.911)[2]),
        coef(fit.p52)[3] / (1 - coef(fit.p52)[2]),
        coef(fit.tdum)[3] / (1 -coef(fit.tdum)[2])
        ), # estimates, then SE 
  rbind(deltamethod(~ x3 / (1 - x2), coef(fit.55), vcov(fit.55)),
        deltamethod(~ x3 / (1 - x2), coef(fit.pres), vcov(fit.pres)),
        deltamethod(~ x3 / (1 - x2), coef(fit.911), vcov(fit.911)),
        deltamethod(~ x3 / (1 - x2), coef(fit.p52), vcov(fit.p52)),
        deltamethod(~ x3 / (1 - x2), coef(fit.tdum), vcov(fit.tdum))
        )
  )
colnames(lrm.rob) <- c("Model", "lrm.est", "lrm.se")


ggplot(lrm.rob, aes(x = lrm.est, y = Model)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lrm.est - 2*lrm.se,
                     xmax = lrm.est + 2*lrm.se),
                 height = .1,
                 size = 1) +
  scale_x_continuous(limits = c(0, 40)) +
  labs(x = "Long Run Multiplier Estimate")
ggsave("figures/lrm-rob.tiff",
       dpi = 300,
       height = 6, width = 8)


# sensitivity analysis 
library(sensemakr)

# use lm-adl lm object
sense.adl <- sensemakr(
  model = lm.adl,
  treatment = "lag_commitments",
  benchmark_covariates = "log_fatalities_combined",
  kd = c(.5, 1, 2, 3),
  q = .5
)
plot(sense.adl)
plot(sense.adl, type = "extreme")
print(sense.adl)
summary(sense.adl)

ovb_minimal_reporting(sense.adl, format = "latex")

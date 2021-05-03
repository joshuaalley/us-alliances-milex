# Joshua Alley
# Texas A&M 
# Analysis of Alliance Commitments and US military spending

# packages and data from other scripts


# kpss test for commitments IV: also stationary in first differences
summary(ur.kpss(us.data$commitments))
summary(ur.kpss(dshift(us.data$commitments)))


# Check the other independent variables
# GDP
ggplot(us.data, aes(x = year, y = gdp)) + geom_line()
# kpss test for GDP: stationary in second differences
summary(ur.kpss(us.data$gdp))
summary(ur.kpss(us.data$change_gdp))
ggplot(us.data, aes(x = year, y = change_gdp)) + geom_line()
summary(ur.kpss(dshift(us.data$change_gdp)))


# phillips-perron
summary(ur.pp(us.data$gdp))
summary(ur.pp(us.data$change_gdp)) 
# add trend
summary(ur.pp(us.data$gdp, model = "trend"))
summary(ur.pp(us.data$change_gdp, model = "trend")) 


# kpss test for deficit: unclear
ggplot(us.data, aes(x = year, y = budget_deficit)) + geom_line()
ggplot(us.data, aes(x = year, y = change_budget_deficit)) + geom_line()

summary(ur.kpss(us.data$budget_deficit))
summary(ur.kpss(dshift(us.data$budget_deficit)))

summary(ur.pp(us.data$budget_deficit))
summary(ur.pp(dshift(us.data$budget_deficit))) 

# dickey-fuller
summary(ur.df(us.data$budget_deficit))
# add drift
summary(ur.df(us.data$budget_deficit[!is.na(us.data$budget_deficit)], type = "drift"))
# add trend
summary(ur.df(us.data$budget_deficit[!is.na(us.data$budget_deficit)], type = "trend"))


# Test Soviet spending
ggplot(us.data, aes(x = year, y = russian_milex_combined)) + geom_line()
summary(ur.kpss(us.data$russian_milex_combined))
summary(ur.kpss(dshift(us.data$russian_milex_combined)))

summary(ur.pp(us.data$russian_milex_combined))
summary(ur.pp(dshift(us.data$russian_milex_combined))) 




# run the ECM 
# fit ols 
lm.ecm <- lm(change_milex_SI ~ lag_milex_SI +
               lag_change_milex_SI +
               lag_commitments + change_commitments +
               peace_5 + log_fatalities_combined +
               cold_war + lag_rep_pres +
               lag_budget_deficit + lag_change_gdp +
               lag_mprival_milex,
             data = us.data,
)
summary(lm.ecm)
reg.check(lm.ecm)
# diagnose and print results
qqnorm(lm.ecm$residuals)
qqline(lm.ecm$residuals)
gecm.res <- summary(lm.ecm)


# Test the LRM
# 11 coefficients and 69 time points
# for alliances
lrm.comm <- -(gecm.res$coefficients[4] / gecm.res$coefficients[2])
lrm.comm
deltamethod(~ x4 / x2, coef(lm.ecm), vcov(lm.ecm))
# # 95% upper bound of LRM
lrm.comm + (2 * deltamethod(~ x4 / x2, coef(lm.ecm), vcov(lm.ecm)))
# # 95% lower bound of LRM
lrm.comm - (2 * deltamethod(~ x4 / x2, coef(lm.ecm), vcov(lm.ecm)))
# 
# T-stat:   
lrm.comm / deltamethod(~ x4 / x2, coef(lm.ecm), vcov(lm.ecm))



### run the model with logged commitments
# cannot interpret substance directly 
# L = lag, d = difference, & number is number of lags
lm.ecm.log <- lm(change_milex_SI ~ lag_milex_SI +
                   lag_change_milex_SI +
                   lag_ln_commitments + change_ln_commitments +
                   peace_5 + log_fatalities_combined +
                   cold_war + lag_rep_pres +
                   lag_budget_deficit + lag_change_gdp +
                   lag_mprival_milex,
                 data = us.data
)
summary(lm.ecm.log)
gecm.res.log <- summary(lm.ecm.log)
reg.check(lm.ecm.log)



# Test the LRM
# 11 coefficients and 69 time points
# for alliances
lrm.comm.log <- -(gecm.res.log$coefficients[4] / gecm.res.log$coefficients[2])
lrm.comm.log
deltamethod(~ x4 / x2, coef(lm.ecm.log), vcov(lm.ecm.log))

# 95% upper bound of LRM
lrm.comm.log + (2 * deltamethod(~ x4 / x2, coef(lm.ecm.log), vcov(lm.ecm.log)))
# 95% lower bound of LRM
lrm.comm.log - (2 * deltamethod(~ x4 / x2, coef(lm.ecm.log), vcov(lm.ecm.log)))

# T-stat 
lrm.comm.log / deltamethod(~ x4 / x2, coef(lm.ecm.log), vcov(lm.ecm.log))

# Based on bounds, conclude unclear long-run relationship 

# calculate LRM in logs
lrm.comm.log*log(1.10)
# 95% upper bound of LRM in logs
(lrm.comm.log + (2 * deltamethod(~ x4 / x2, coef(lm.ecm.log), 
                                 vcov(lm.ecm.log))))*log(1.1)
# 95% lower bound of LRM in logs
(lrm.comm.log - (2 * deltamethod(~ x4 / x2, coef(lm.ecm.log), 
                                 vcov(lm.ecm.log))))*log(1.1)

# bounds test 
ecm1 <- dynardl(milex_SI ~ commitments +
                  peace_5 + log_fatalities_combined +
                  cold_war + rep_pres +
                  budget_deficit + change_gdp +
                  mprival_milex_sipri,
                data = us.data,
                lags = list("milex_SI" = 1, "commitments" = 1,
                            "rep_pres" = 1, "budget_deficit" = 1,
                            "change_gdp" = 1, "mprival_milex_sipri" = 1),
                diffs = c("commitments"),
                lagdiffs = list("milex_SI" = c(1)),
                levels = c("peace_5", "cold_war",
                           "log_fatalities_combined"),
                ec = TRUE) # error correction: differenced DV
dynardl.auto.correlated(ecm1)
pssbounds(ecm1, case = 3) # bounds cointegration test


# add a linear trend
ecm2 <-  dynardl(milex_SI ~ commitments +
                   peace_5 + log_fatalities_combined +
                   cold_war + rep_pres +
                   budget_deficit + change_gdp +
                   mprival_milex_sipri,
                 data = us.data,
                 trend = TRUE,
                 lags = list("milex_SI" = 1, "commitments" = 1,
                             "rep_pres" = 1, "budget_deficit" = 1,
                             "change_gdp" = 1, "mprival_milex_sipri" = 1),
                 diffs = c("commitments"),
                 lagdiffs = list("milex_SI" = c(1)),
                 levels = c("peace_5", "cold_war",
                            "log_fatalities_combined"),
                 ec = TRUE) # error correction: differenced DV
dynardl.auto.correlated(ecm2)
pssbounds(ecm2, case = 5) # unrestricted intercept and linear trend
summary(ecm2)
# bounds test shows cointegration at 5% level in 4/4 tests



# bounds test 
ecm1.log <- dynardl(milex_SI ~ ln_commitments +
                      peace_5 + log_fatalities_combined +
                      cold_war + rep_pres +
                      budget_deficit + change_gdp +
                      mprival_milex_sipri,
                    data = us.data,
                    lags = list("milex_SI" = 1, "ln_commitments" = 1,
                                "rep_pres" = 1, "budget_deficit" = 1,
                                "change_gdp" = 1, "mprival_milex_sipri" = 1),
                    diffs = c("ln_commitments"),
                    lagdiffs = list("milex_SI" = c(1)),
                    levels = c("peace_5", "cold_war",
                               "log_fatalities_combined"),
                    ec = TRUE) # error correction: differenced DV 
dynardl.auto.correlated(ecm1.log)
pssbounds(ecm1.log, case = 3) # bounds cointegration test


# add a linear trend
ecm2.log <- dynardl(milex_SI ~ ln_commitments +
                      peace_5 + log_fatalities_combined +
                      cold_war + rep_pres +
                      budget_deficit + change_gdp +
                      mprival_milex_sipri,
                    data = us.data,
                    trend = TRUE,
                    lags = list("milex_SI" = 1, "ln_commitments" = 1,
                                "rep_pres" = 1, "budget_deficit" = 1,
                                "change_gdp" = 1, "mprival_milex_sipri" = 1),
                    diffs = c("ln_commitments"),
                    lagdiffs = list("milex_SI" = c(1)),
                    levels = c("peace_5", "cold_war",
                               "log_fatalities_combined"),
                    ec = TRUE) # error correction: differenced DV 
dynardl.auto.correlated(ecm2.log)
pssbounds(ecm2.log, case = 5) # unrestricted intercept and linear trend
summary(ecm2.log)
# bounds test shows cointegration at 5% level in 4/4 tests




### tabulate results w/ ols 
stargazer(lm.ecm, lm.ecm.log,
          omit.table.layout = "m",
          style = "apsr",
          dep.var.labels=c("Change in Military Spending"),
          omit = c("dec"),
          order = c("lag_milex_SI", "lag_change_milex_SI",
                    "lag_commitments", "change_commitments",
                    "lag_ln_commitments", "change_ln_commitments",
                    "lag_change_gdp", "cold_war",
                    "lag_mprival_milex",
                    "peace_5", "log_fatalities_combined",
                    "lag_rep_pres", "lag_budget_deficit"),
          covariate.labels=c("Lagged Military Spending",
                             "Lag Changes in Military Spending",
                             "Lag Alliance Commitments", "Change Alliance Commitments",
                             "Lag Log Alliance Commitments", "Change in Log Alliances",
                             "Lag Change in GDP", "Cold War",
                             "Lag Rival Spending",
                             "Post-Conflict Years", "Log Combat Fatalities",
                             "Lag Republican President", 
                             "Lag Budget Deficit"
          ),
          keep.stat = c("n"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:ecm-coefs")
)



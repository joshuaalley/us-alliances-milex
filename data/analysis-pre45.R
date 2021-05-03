# Joshua Alley and Matthew Fuhrmann
# Analysis before 1945


# rely on us.data.pre45 w/ maddison GDP data
glimpse(us.data.pre45)

# create some key variables
us.data.pre45 <- mutate(us.data.pre45,
                        change_madd_gdp = madd_gdp - lag(madd_gdp),
                        lag_change_madd_gdp = lag(change_madd_gdp),
                        post45 = ifelse(year >= 1946, 1, 0),
                        ww2 = ifelse(year >= 1941 & year <= 1945, 1, 0),
                        ln_milex = log(milex_SI),
                        lag_ln_milex = lag(ln_milex))

# plot logged spending
summary(us.data.pre45$ln_milex)
ggplot(us.data.pre45, aes(x = year, y = ln_milex)) + geom_line()
# plot spending without log and commitments
ggplot(us.data.pre45, aes(x = year, y = milex_SI)) + geom_line()
ggplot(us.data.pre45, aes(x = year, y = commitments)) + geom_line()

# Plot with only the IV and DV
select(us.data.pre45, year, milex_SI, # select variables and years, pivot long
       commitments) %>%
  pivot_longer(-year) %>% 
  ggplot( aes(x = year, y = value)) + # plot 
  facet_wrap(~name, scales = "free",
             labeller = labeller(name = c(commitments = "Alliance Commitments",
                                          milex_SI = "Military Spending"))
  ) +
  geom_line(size = 1) +
  labs(x = "Year") +
  theme_bw()
ggsave("appendix/pre45-outcome-iv.png", height = 6, width = 8)



### regression models 
# Simple regression model
lm.adl.pre45 <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                 peace_5 + total_rival_cap + ww2 + post45 +
                 lag_rep_pres + lag_change_madd_gdp,
                 data = us.data.pre45)

reg.check(lm.adl.pre45)
summary(lm.adl.pre45)
# diagnose and print results
qqnorm(lm.adl.pre45$residuals)
qqline(lm.adl.pre45$residuals)


adl.res.pre45 <- summary(lm.adl.pre45)

# Test the LRM
# 11 coefficients and 69 time points
# for alliances
lrm.adl.pre45 <- (adl.res.pre45$coefficients[3]) /(1 -  adl.res.pre45$coefficients[2])
lrm.adl.pre45
deltamethod(~ (x3) / (1 - x2), coef(lm.adl.pre45), vcov(lm.adl.pre45))
# # 95% upper bound of LRM
lrm.adl.pre45 + (2 * deltamethod(~ (x3) / (1 - x2), coef(lm.adl.pre45), vcov(lm.adl.pre45)))
# # 95% lower bound of LRM
lrm.adl.pre45 - (2 * deltamethod(~ (x3) / (1 - x2), coef(lm.adl.pre45), vcov(lm.adl.pre45)))
# 
# T-stat:   
lrm.adl.pre45 / deltamethod(~ (x3) / (1 - x2), coef(lm.adl.pre45), vcov(lm.adl.pre45))


# logged alliances
adl.pre45.log <- lm(milex_SI ~ lag_milex_SI + lag_ln_commitments +
                      peace_5 + total_rival_cap + ww2 + post45 +
                     lag_rep_pres + lag_change_madd_gdp,
                   data = us.data.pre45)
reg.check(adl.pre45.log)
summary(adl.pre45.log)
# diagnose and print results
qqnorm(adl.pre45.log$residuals)
qqline(adl.pre45.log$residuals)

# OLS with pres FE
# wilson is base category
# total rival cap requires cutting Obama and Trump- through 2012
lm.adl.pre45.pres <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                            peace_5 + total_rival_cap +
                            lag_change_madd_gdp +
                            Harding + Coolidge +
                            Hoover + Roosevelt + Truman +
                            Eisenhower + Kennedy + Johnson +
                            Nixon + Ford + Carter + Reagan +
                            HW.Bush + Clinton + W.Bush, #+ Obama + Trump,
                          data = us.data.pre45)
summary(lm.adl.pre45.pres)



### robust regression
# Simple regression model
rlm.adl.pre45 <- rlm(milex_SI ~ lag_milex_SI + lag_commitments +
                       peace_5 + total_rival_cap + ww2 + post45 +
                     lag_rep_pres + lag_change_madd_gdp,
                   data = us.data.pre45,
                   maxit = 80)
reg.check(rlm.adl.pre45)
summary(rlm.adl.pre45)
plot(rlm.adl.pre45$residuals, rlm.adl.pre45$w)
summary(rlm.adl.pre45$w)


# logged alliances
adl.pre45.logr <- rlm(milex_SI ~ lag_milex_SI + lag_ln_commitments +
                        peace_5 + total_rival_cap + ww2 + post45 +
                      lag_rep_pres + lag_change_madd_gdp,
                      maxit = 80,
                    data = us.data.pre45)
reg.check(adl.pre45.logr)
summary(adl.pre45.logr)

# robust reg w/ pres FE
rlm.adl.pre45.pres <- rlm(milex_SI ~ lag_milex_SI + lag_commitments +
                       peace_5 + total_rival_cap +
                       lag_change_madd_gdp +
                        Harding + Coolidge +
                        Hoover + Roosevelt + Truman +
                       Eisenhower + Kennedy + Johnson +
                        Nixon + Ford + Carter + Reagan +
                       HW.Bush + Clinton + W.Bush, #+ Obama,
                     data = us.data.pre45,
                     maxit = 80)
reg.check(rlm.adl.pre45.pres)
summary(rlm.adl.pre45.pres)



# Tabulate results
adl.res.pre45 <- summary(lm.adl.pre45)

# Test the LRM
# 11 coefficients and 69 time points
# for alliances
lrm.adl.post45 <- (adl.res.pre45$coefficients[3]) /(1 -  adl.res.pre45$coefficients[2])
lrm.adl.post45
deltamethod(~ (x3) / (1 - x2), coef(lm.adl.pre45), vcov(lm.adl.pre45))
# # 95% upper bound of LRM
lrm.adl + (2 * deltamethod(~ (x3) / (1 - x2), coef(lm.adl.pre45), vcov(lm.adl.pre45)))
# # 95% lower bound of LRM
lrm.adl - (2 * deltamethod(~ (x3) / (1 - x2), coef(lm.adl.pre45), vcov(lm.adl.pre45)))
# 
# T-stat for LRM:   
lrm.adl / deltamethod(~ (x3) / (1 - x2), coef(lm.adl.pre45), vcov(lm.adl.pre45))





### Tabulate results
# should be translated into sidewaystable in LaTeX. 
stargazer(lm.adl.pre45, adl.pre45.log, lm.adl.pre45.pres,
          rlm.adl.pre45, adl.pre45.logr, rlm.adl.pre45.pres,
          omit.table.layout = "m",
          style = "apsr",
          dep.var.labels=c("Military Spending"),
          order = c("lag_milex_SI",
                    "lag_commitments", "lag_ln_commitments", 
                    "peace_5", "total_rival_cap", 
                    "ww2", "post45",
                    "lag_rep_pres", "lag_change_madd_gdp"),
          covariate.labels=c("Lagged Military Spending",
                             "Lag Alliance Commitments",
                             "Lag Log Alliance Commitments", 
                             "Post-Conflict Years", "Total Rival CINC",
                             "World War II", "Post 1945", "Lag Republican President", 
                             "Lag GDP Change"
          ),
          keep.stat = c("n"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses. Models 1-3 estimated with OLS.
          Models 4-6 estimated with robust regression. Presidential fixed effects omitted.", 
          notes.append = FALSE,
          label = c("tab:adl-coefs"),
          omit = c(11:24)
)




### plot LRMS of each model 
lrm.rob.pre45 <- cbind.data.frame(
  rbind("OLS", "OLS: Logged Alliances", "OLS: Pres. FE",
        "Robust Reg.", "Robust Reg.: Pres. FE", "Robust Reg: Logged Alliances"), 
  rbind(coef(lm.adl.pre45)[3] / (1 - coef(lm.adl.pre45)[2]), 
        (coef(adl.pre45.log)[3] / (1 - coef(adl.pre45.log)[2]))*log(1.10),
        coef(lm.adl.pre45.pres)[3] / (1 - coef(lm.adl.pre45.pres)[2]),
        coef(rlm.adl.pre45)[3] / (1 - coef(rlm.adl.pre45)[2]),
        coef(rlm.adl.pre45.pres)[3] / (1 - coef(rlm.adl.pre45.pres)[2]),
        (coef(adl.pre45.logr)[3] / (1 - coef(adl.pre45.logr)[2]))*log(1.10)
  ), # estimates, then SE 
  rbind(deltamethod(~ x3 / (1 - x2), coef(lm.adl.pre45), vcov(lm.adl.pre45)),
        deltamethod(~ x3 / (1 - x2), coef(adl.pre45.log), vcov(adl.pre45.log))*log(1.10),
        deltamethod(~ x3 / (1 - x2), coef(lm.adl.pre45.pres), vcov(lm.adl.pre45.pres)),
        deltamethod(~ x3 / (1 - x2), coef(rlm.adl.pre45), vcov(rlm.adl.pre45)),
        deltamethod(~ x3 / (1 - x2), coef(rlm.adl.pre45.pres), vcov(rlm.adl.pre45.pres)),
        deltamethod(~ x3 / (1 - x2), coef(adl.pre45.logr), vcov(adl.pre45.logr))*log(1.10)
  )
)
colnames(lrm.rob.pre45) <- c("Model", "lrm.est", "lrm.se")

# plot: likely inflated- interpret with caution.
ggplot(lrm.rob.pre45, aes(x = lrm.est, y = Model)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lrm.est - 2*lrm.se,
                     xmax = lrm.est + 2*lrm.se),
                 height = .1,
                 size = 1) +
  labs(x = "Long Run Multiplier Estimate")
ggsave("appendix/lrm-rob-pre45.png", height = 6, width = 8)


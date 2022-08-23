# Joshua Alley & Matthew Fuhrmann
# Other robustness checks: models in logs, split by region and BSTS



### Extreme Bounds: specification Unc
eba.all <- eba(data = us.data, y = "milex_SI", 
               free = c("lag_milex_SI"),
               doubtful = c("lag_commitments", 
                              "lag_gdp", "lag_change_gdp", "change_gdp",
                              "lag_budget_deficit", "change_budget_deficit",  
                              "budget_deficit", "peace_5", "log_fatalities_combined",
                               "lag_mprival_milex", "ln_majorcom_dep",
                              "lag_rep_pres", "cold_war", "post_911", "pre_52"
                              ), 
               focus = c("lag_commitments"),
               k = 0:8, reg.fun = lm)
print(eba.all)
hist(eba.all, variables = c("lag_commitments"),
     main = c(lag_commitments = "Lag Commitments"),
     normal.show = TRUE
)

# Export plot
eba.coefs <- data.frame(eba.all[["regressions"]][["beta"]])
ggplot(eba.coefs, aes(x = lag_commitments)) +
  geom_histogram(color = "black", fill = "grey") +
  labs(x = "Lag Commitments Coefficient", y = "Models")
ggsave("appendix/eba-res.png", height = 6, width = 8)


# use eba res to calculate LRM of lagged alliance commitments
eba.lrm <- cbind.data.frame(
  beta.comm = eba.all[["regressions"]][["beta"]][, 3],
  lag.level = eba.all[["regressions"]][["beta"]][, 2], 
  model = seq(1, 12911, by = 1)  
  ) %>% 
  drop_na() %>%
  mutate(
  lrm.est = beta.comm / (1 - lag.level)
  )

# summarize results
summary(eba.lrm$beta.comm)
summary(eba.lrm$lag.level)
summary(eba.lrm$lrm.est)
quantile(eba.lrm$lrm.est, c(.025, .975))
# some huge results- interp with caution
ggplot(eba.lrm, aes(x = lrm.est)) +
  geom_histogram(color = "black", fill = "grey") +
  labs(x = "Long-Run Multiplier", y = "Models")
ggsave("appendix/lrm-eba.png", height = 6, width = 8)





### Alternative expressions of threat

# model with annual MIDS
lm.adl.mids <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                  peace_5 + log_fatalities_combined +
                  disputes + 
                  cold_war + lag_rep_pres +
                  lag_budget_deficit + lag_change_gdp,
                data = us.data)
reg.check(lm.adl.mids)
summary(lm.adl.mids)

# model with ICB crises 
# by level 
lm.adl.icb <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                    peace_5 + icb_level + log_fatalities_combined +
                    cold_war + lag_rep_pres +
                    lag_budget_deficit + lag_change_gdp,
                  data = us.data)
reg.check(lm.adl.icb)
summary(lm.adl.icb)

# by count
lm.adl.icbc <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                   peace_5 + icb_count +  log_fatalities_combined +
                   cold_war + lag_rep_pres +
                   lag_budget_deficit + lag_change_gdp,
                 data = us.data)
reg.check(lm.adl.icbc)
summary(lm.adl.icbc)

# strategic rival milex: CINC data here
lm.adl.rival <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                       peace_5 + lag_rival_milexc + log_fatalities_combined +
                       lag_rep_pres + cold_war + 
                       lag_budget_deficit + lag_change_gdp,
                     data = us.data)
reg.check(lm.adl.rival)
summary(lm.adl.rival)

# strategic rival cinc score
lm.adl.rivalcap <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                      peace_5 + lag_rival_cap + log_fatalities_combined +
                      lag_rep_pres + cold_war +
                      lag_budget_deficit + lag_change_gdp,
                    data = us.data)
reg.check(lm.adl.rivalcap)
summary(lm.adl.rivalcap)

# strategic rival count
lm.adl.rivals <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
                        peace_5 + total_rivals + log_fatalities_combined +
                        lag_rep_pres + cold_war +
                        lag_budget_deficit + lag_change_gdp,
                      data = us.data)
reg.check(lm.adl.rivals)
summary(lm.adl.rivals)


# oatley security shocks variable
ggplot(us.data, aes(x = year, y = oatley_shocks)) + geom_line()
ggplot(us.data, aes(x = year, y = log_fatalities_combined,
                    color = oatley_shocks)) + geom_line()

# concern with autocorrelation in lags without fatalities variable
lm.adl.shocks <- lm(milex_SI ~ lag_milex_SI + 
                      lag_commitments +
                      peace_5 + oatley_shocks + log_fatalities_combined +
                      lag_rep_pres + cold_war + 
                      lag_budget_deficit + lag_change_gdp,
                    data = us.data)
reg.check(lm.adl.shocks)
summary(lm.adl.shocks)


# model with russian spending and CW Interaction
lm.adl.cw <- lm(milex_SI ~ lag_milex_SI + lag_commitments +
               peace_5 + log_fatalities_combined +
               cold_war + lag_rep_pres +
               lag_budget_deficit + lag_change_gdp +
               lag_russian_milex_combined +
               cold_war:lag_russian_milex_combined,
             data = us.data)
reg.check(lm.adl.cw)
summary(lm.adl.cw)


# model with russian spending and CW Interaction
lm.adl.cw.log <- lm(milex_SI ~ lag_milex_SI + lag_ln_commitments +
                  peace_5 + log_fatalities_combined +
                  cold_war + lag_rep_pres +
                  lag_budget_deficit + lag_change_gdp +
                  lag_russian_milex_combined +
                  cold_war:lag_russian_milex_combined,
                data = us.data)
reg.check(lm.adl.cw.log)
summary(lm.adl.cw.log)


# plot lagged alliance coef throughout
multiplot(lm.adl.mids, lm.adl.icb, lm.adl.icbc,
          lm.adl.rival, lm.adl.rivalcap, lm.adl.rivals,
          lm.adl.shocks, lm.adl.cw,
          coefficients = "lag_commitments",
          color = "black",
          lwdOuter = 1,
          horizontal = TRUE,
          xlab = "Lag Alliance Commitments Estimate",
          ylab = "Threat Measure",
          title = "Estimated Alliance Coefficient Adjusting for Different Threat Measures",
          names = c("MIDS", "ICB Level", "ICB Count",
                    "Rival Spending (COW)", "Rival CINC", "Rival Count",
                    "Security Shocks",
                    "Russ. Spending and Cold War"),
          by = "Model"
          )
ggsave("appendix/alt-threat-res.png", height = 6, width = 8)


# Tabulate results: cold war interaction w/ and w/o logs
stargazer(lm.adl.cw, lm.adl.cw.log,
          omit.table.layout = "m",
          style = "apsr",
          dep.var.labels=c("Military Spending"),
          order = c("lag_milex_SI",
                    "lag_commitments","lag_ln_commitments", 
                    "lag_change_gdp", 
                    "peace_5", "log_fatalities_combined",
                    "lag_rep_pres", "lag_budget_deficit",
                    "cold_war",
                    "lag_russian_milex_combined",
                    "cold_war:lag_russian_milex_combined"),
          covariate.labels=c("Lagged Military Spending",
                             "Lag Alliance Commitments",
                             "Lag Log Alliance Commitments", 
                             "Lag Change in GDP", 
                             "Post-Conflict Years", "Log Combat Fatalities",
                             "Lag Republican President", 
                             "Lag Budget Deficit",
                             "Cold War", "Russian Spending and Cold War",
                             "Lag Russian Spending"
          ),
          keep.stat = c("n"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:adl-coefs-inter")
)



### Alternative lag structures
# add second outcome lag to ADL ADL(2, 1)
lm.adl.2l <- lm(milex_SI ~ lag_milex_SI + lag2_milex_SI +
               lag_commitments +
               peace_5 + log_fatalities_combined +
               lag_rep_pres + cold_war + 
               lag_budget_deficit + lag_change_gdp +
               lag_mprival_milex,
             data = us.data)
reg.check(lm.adl.2l)
summary(lm.adl.2l)

# smaller LRM- similar test result 
adl.res.2l <- summary(lm.adl.2l)
lrm.adl.2l <- (adl.res.2l$coefficients[4]) / 
  (1 -  (adl.res.2l$coefficients[2] + adl.res.2l$coefficients[3]))
lrm.adl.2l
# T-stat:   
lrm.adl.2l / deltamethod(~ (x4) / (1 - x2 - x3), coef(lm.adl.2l), vcov(lm.adl.2l))
# # 95% upper bound of LRM
lrm.adl.2l + (2 * 
        deltamethod(~ (x4) / (1 - x2 - x3), coef(lm.adl.2l), vcov(lm.adl.2l)))
# # 95% lower bound of LRM
lrm.adl.2l - (2 * 
        deltamethod(~ (x4) / (1 - x2 - x3), coef(lm.adl.2l), vcov(lm.adl.2l)))



# include current commitment levels and lag
lm.adl.comm <- lm(milex_SI ~ lag_milex_SI + 
                  commitments + lag_commitments +
                  peace_5 + log_fatalities_combined +
                  lag_rep_pres + cold_war + 
                  lag_budget_deficit + lag_change_gdp +
                  lag_mprival_milex,
                data = us.data)
reg.check(lm.adl.comm)
summary(lm.adl.comm)

# similar lrm and test result
adl.res.comm <- summary(lm.adl.comm)
lrm.adl.comm <- (adl.res.comm$coefficients[3] + adl.res.comm$coefficients[4]) / 
                      (1 -  adl.res.comm$coefficients[2])
lrm.adl.comm 
deltamethod(~ (x3 + x4) / (1 - x2), coef(lm.adl.comm), vcov(lm.adl.comm))
# T-stat:   
lrm.adl.comm / deltamethod(~ (x3 + x4) / (1 - x2), coef(lm.adl.comm), vcov(lm.adl.comm))


# regression table 
stargazer(lm.adl.2l, lm.adl.comm,
          omit.table.layout = "m",
          style = "apsr",
          dep.var.labels=c("Military Spending"),
          order = c("lag_milex_SI", "lag2_milex_SI",
                    "lag_commitments","commitments", 
                    "lag_change_gdp", "Cold War",
                    "peace_5", "log_fatalities_combined",
                    "lag_rep_pres", "lag_budget_deficit",
                    "lag_mprival_milex"),
          covariate.labels=c("Lagged Military Spending", "Second Lag Military Spend.",
                             "Lag Alliance Commitments",
                             "Alliance Commitments", 
                             "Lag Change in GDP", "Cold War",
                             "Post-Conflict Years", "Log Combat Fatalities",
                             "Lag Republican President", 
                             "Lag Budget Deficit",
                             "Lag Major Power Rival Spending"
          ),
          keep.stat = c("n"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:adl-coefs-lags")
)


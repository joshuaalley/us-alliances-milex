# Joshua Alley & Matthew Fuhrmann
# analysis of changes in US military spending


# plot IV
summary(us.data$change_commitments)
ggplot(us.data, aes(x = year, y = change_commitments)) + geom_line()

# plot outcome
ggplot(us.data, aes(x = year, y = change_milex_SI)) + geom_line()


### Regression models of changes in spending
# standard ols
change.ols <- lm(change_milex_SI ~ lag_change_milex_SI +
                   lag_change_commitments +
                   peace_5 + log_fatalities_combined +
                   cold_war + lag_rep_pres +
                   lag_change_budget_deficit + lag_gdp_growth +
                   lag_change_russian_milex_combined,
                 data = us.data
)
reg.check(change.ols)
summary(change.ols)

# long-run effect: coef / 1 - LDV
change.ols$coefficients[3]  / (1 -  change.ols$coefficients[2])
deltamethod(~ x3 / (1 - x2), coef(change.ols), vcov(change.ols))


# model with decade dummies
change.ols.dec <- lm(change_milex_SI ~ lag_change_milex_SI +
                       lag_change_commitments +
                       peace_5 + log_fatalities_combined +
                       lag_change_budget_deficit + lag_gdp_growth +
                       lag_change_russian_milex_combined +
                     dec_50 + dec_60 + dec_70 + dec_80 +
                     dec_00 + dec_10,
                 data = us.data
)
reg.check(change.ols.dec)
summary(change.ols.dec)
(change.ols.dec$coefficients[3]) / (1 - change.ols.dec$coefficients[2])
deltamethod(~ x3 / (1 - x2), coef(change.ols.dec), vcov(change.ols.dec))


### Robust Regression models of spending changes

# need robust regression throughout
change.rlm <- rlm(change_milex_SI ~ lag_change_milex_SI +
                    lag_change_commitments +
                    peace_5 + log_fatalities_combined +
                    cold_war + lag_rep_pres +
                    lag_change_budget_deficit + lag_gdp_growth +
                    lag_change_russian_milex_combined,
                   data = us.data,
                  maxit = 40
                 )
summary(change.rlm )
(coef(change.rlm )[3]) / (1 - coef(change.rlm )[2])
deltamethod(~ x3 / (1 - x2), coef(change.rlm ), 
            vcov(change.rlm ))


# Add decade FE 
change.rlm.dec <- rlm(change_milex_SI ~ lag_change_milex_SI +
                        lag_change_commitments +
                        peace_5 + log_fatalities_combined +
                        lag_change_budget_deficit + lag_gdp_growth +
                        lag_change_russian_milex_combined +
                        dec_50 + dec_60 + dec_70 + dec_80 +
                        dec_00 + dec_10,
                  data = us.data,
                  maxit = 40
)
summary(change.rlm.dec)
(coef(change.rlm.dec )[3]) / (1 - coef(change.rlm.dec )[2])
deltamethod(~ x3 / (1 - x2), coef(change.rlm.dec ),
            vcov(change.rlm.dec ))



# model with president dummmies
change.lm.pres <- lm(change_milex_SI ~ lag_change_milex_SI +
                       lag_change_commitments +
                       peace_5 + log_fatalities_combined +
                       lag_change_budget_deficit + lag_gdp_growth +
                       lag_change_russian_milex_combined +
                        Carter + Clinton + Eisenhower + Ford +
                         HW.Bush + Johnson + Kennedy + Nixon +
                         Reagan + Truman + Trump + W.Bush,
                      data = us.data
)
reg.check(change.lm.pres)
summary(change.lm.pres)
# Long-run effect
change.lm.pres$coefficients[3] / (1 - change.lm.pres$coefficients[2])
deltamethod(~ x3 / (1 - x2), coef(change.lm.pres), vcov(change.lm.pres))


# robust reg model of changes
change.rlm.pres <- rlm(change_milex_SI ~ lag_change_milex_SI +
                         lag_change_commitments +
                         peace_5 + log_fatalities_combined +
                         lag_change_budget_deficit + lag_gdp_growth +
                         lag_change_russian_milex_combined +
                         Carter + Clinton + Eisenhower + Ford +
                         HW.Bush + Johnson + Kennedy + Nixon +
                         Reagan + Truman + Trump + W.Bush,
                     data = us.data,
                     maxit = 40
                  )
summary(change.rlm.pres)




# tabulate the results 
stargazer(change.ols, change.ols.dec, change.lm.pres,
          change.rlm, change.rlm.dec, change.rlm.pres,
          column.labels = c("OLS", "OLS: Decade FE", "OLS: Pres FE",
                            "Robust Reg", "Robust Reg: Decade FE", 
                            "Robust Reg: Pres FE"),
          omit.table.layout = "m",
          style = "apsr",
          dep.var.labels=c("Change in Military Spending"),
          order = c("lag_change_milex_SI", "change_commitments",
                    "cold_war", "change_budget_deficit", "lag_gdp_growth",
                    "rep_pres", "atwar"),
          omit = c("dec", "Carter", "Clinton", "Eisenhower", "Ford",
                     "HW.Bush", "Johnson", "Kennedy", "Nixon",
                     "Reagan", "Truman", "Trump", "W.Bush"),
          covariate.labels=c(
            "Lag Changes in Military Spending",
            "Change in Alliances", "Cold War",
            "Change in Budget Deficit", "GDP Growth",
            "Republican President", "At War"
          ),
          keep.stat = c("n"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses. Decade and Presidential fixed effects omitted to save space.", 
          notes.append = FALSE,
          label = c("tab:results-tab-change"))


# Plot the change_commitments coefficient across the range of models
multiplot(change.ols, change.ols.dec, change.lm.pres,
          change.rlm, change.rlm.dec, change.rlm.pres,
          coefficients = "lag_change_commitments",
          color = "black",
          outerCI = 2,
          lwdOuter = 1,
          names = c("OLS", "OLS: Decade FE", "OLS: Pres FE",
                    "Robust Reg", "Robust Reg: Decade FE", "Robust Reg: Pres FE"),
          ylab = "Model",
          xlab = "Coefficient Estimate",
          title = "Effect of Changes in Alliance Commitments on Annual Defense Spending Changes",
          secret.weapon = TRUE)
ggsave("appendix/change-coefplot.png", height = 6, width = 8)


# Plot long-run effect
long.run.effects <- rbind.data.frame(
  deltaMethod(change.ols,"lag_change_commitments/(1-lag_change_milex_SI)"),
  deltaMethod(change.ols.dec,"lag_change_commitments/(1-lag_change_milex_SI)"),
  deltaMethod(change.lm.pres,"lag_change_commitments/(1-lag_change_milex_SI)"),
  deltaMethod(change.rlm,"lag_change_commitments/(1-lag_change_milex_SI)"),
  deltaMethod(change.rlm.dec,"lag_change_commitments/(1-lag_change_milex_SI)"),
  deltaMethod(change.rlm.pres,"lag_change_commitments/(1-lag_change_milex_SI)")
)
long.run.effects$Model <- c("OLS", "OLS: Decade FE", "OLS: Pres FE",
                            "Robust Reg", "Robust Reg: Decade FE", "Robust Reg: Pres FE")


ggplot(long.run.effects, aes(x = Estimate, y = Model)) +
  xlim(0, 30) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = `2.5 %`,
                     xmax = `97.5 %`),
                 height = .1,
                 size = 1) +
  labs(x = "Long Run Effect Estimate") +
  ggtitle("Estimated Long-Run Effect of Changes in Alliance Commitments")
ggsave("appendix/changes-long-run-est.png", height = 6, width = 8)







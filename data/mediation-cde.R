# Joshua Alley and Matthew Fuhrmann
# Mediation/direct effects


# load packages
library(mediation)
library(DirectEffects)


# controlled direct effects
direct.all.conf <- sequential_g(milex_SI ~ lag_milex_SI + cold_war +
                lag_rep_pres + lag_budget_deficit + lag_change_gdp +
                lag_commitments | lag_mprival_milex | 
                  peace_5 + log_fatalities_combined,
                data = us.data)
summary(direct.all.conf)

# normality diagnostics
shapiro.test(direct.all.conf$residuals) # normality
qqnorm(direct.all.conf$residuals)
qqline(direct.all.conf$residuals)



# mediation
# conflict
conf.model <- lm(log_fatalities_combined ~ 
                   cold_war +  lag_mprival_milex +
                   lag_rep_pres +
                   lag_commitments,
                  data = us.data)
summary(conf.model)

milex.mod <- glm(milex_SI ~ lag_milex_SI + cold_war +
                   lag_rep_pres + lag_budget_deficit + lag_change_gdp +
                   lag_commitments + lag_mprival_milex + 
                   peace_5 + log_fatalities_combined,
                data = us.data)
summary(milex.mod)

mediate.mod.conf <- mediate(model.m = conf.model, 
                       model.y = milex.mod,
                       treat = "lag_commitments", 
                       mediator = "log_fatalities_combined",
                       boot = TRUE,
                       robust = TRUE,
                       sims = 1000)
summary(mediate.mod.conf)
plot(mediate.mod.conf)
medsens(mediate.mod.conf)



# major power military spending
rival.model <- lm(lag_mprival_milex ~ 
                   cold_war +  
                   lag_rep_pres +
                   lag_commitments,
                 data = us.data)
summary(conf.model)

milex.mod <- glm(milex_SI ~ lag_milex_SI + cold_war +
                   lag_rep_pres + lag_budget_deficit + lag_change_gdp +
                   lag_commitments + lag_mprival_milex + 
                   peace_5 + log_fatalities_combined,
                 data = us.data)
summary(milex.mod)

mediate.mod.riv <- mediate(model.m = rival.model, 
                            model.y = milex.mod,
                            treat = "lag_commitments", 
                            mediator = "lag_mprival_milex",
                            boot = TRUE,
                            sims = 1000)
summary(mediate.mod.riv)

# Joshua Alley and Matthew Fuhrmann
# counterfactual analysis




# ADL sim 
### function to implement counterfactual simulation of NATO
# with different models
counterfactual.sim.adl <- function(model, log, st.year, 
                                   simulations, folder){
  
  # start year of simulation
  obs.year <- us.data$year[1:nrow(model$model)]
  obs.start <- which(grepl(st.year, us.data$year))
  
  # Simulate from 'posterior'
  sim <- mvrnorm(simulations, coef(model), 
                 vcov(model))
  
  # ifelse to capture models in logs
  if(log == "log"){
    # predictions without NATO expansion
    xreg.no.expand <- cbind(rep(1, nrow(model$model)),
                            model$model[2:ncol(model$model)]
    )
    xreg.no.expand$lag_ln_commitments[obs.year >= st.year] <- 
            xreg.no.expand$lag_ln_commitments[obs.year >= st.year] - # obs level
                 (log(63) - log(60)) # difference in log
    
    xreg.expand <- cbind(rep(1, nrow(model$model)),
                         model$model[2:ncol(model$model)]
    )
  }else{
    # predictions without NATO expansion
    xreg.no.expand <- cbind(rep(1, nrow(model$model)),
                            model$model[2:ncol(model$model)]
    )
    xreg.no.expand$lag_commitments[obs.year >= st.year] <- 
                   xreg.no.expand$lag_commitments[obs.year >= st.year] - 3
    
    xreg.expand <- cbind(rep(1, nrow(model$model)),
                         model$model[2:ncol(model$model)]
    )
  }
  
  # multiply hypothetical and observed data by simulated coef vectors
  # replace observed lag with missing, as changes affect levels
  xreg.no.expand$lag_milex_SI[obs.start:nrow(xreg.no.expand)] <- NA 
  n <- length(seq(from = obs.start, to = nrow(xreg.no.expand), by = 1))
  
  # create a list of dataframes simulations replicates of no expand data
  xreg.noex.list <- vector(mode = "list", length = simulations)
  for(i in seq_along(xreg.noex.list)){
    xreg.noex.list[[i]] <- xreg.no.expand
  }
  
  # loop over simulated datasets and parameters
  sim.no.expand <- matrix(data = NA, nrow = simulations, ncol = nrow(model$model))
  for(nsim in 1:simulations){ # simulated data and parameters
    
    # subloop by year
    for(obs in (obs.start-1):(ncol(sim.no.expand)-1)){
      sim.no.expand[nsim, obs] <- sim[nsim, ] %*% as.matrix(
        t(xreg.noex.list[[nsim]])[, obs])
      
      # assign lags and levels in each simulated dataset of list
      xreg.noex.list[[nsim]]$lag_milex_SI[obs + 1] <- sim.no.expand[nsim, obs]
      
    }
  }
  
  # summarize output
  summary(sim.no.expand)
  # print SD of simulated values
  print(sd(sim.no.expand, na.rm = TRUE))
  sim.no.expand.sum <- apply(sim.no.expand[, 1:ncol(sim.no.expand)], 2, 
                             function(x){quantile(x, c(0.025, .975), na.rm = TRUE)})
  # create dataframe
  sim.data.noex <- cbind.data.frame(
    year = obs.year,
    sim.low = t(sim.no.expand.sum)[, 1],
    sim.high = t(sim.no.expand.sum)[, 2],
    scen = 1
  )
  
  
  # Predicted values with expansion
  sim.expand <- sim %*% t(xreg.expand)
  sim.expand.sum <- apply(sim.expand, 2, 
                          function(x){quantile(x, c(0.025, .975))})
  summary(sim.expand)
  
  
  # create dataframe with each simulated
  sim.data.ex <- cbind.data.frame(
    year = obs.year,
    sim.low = t(sim.expand.sum)[, 1],
    sim.high = t(sim.expand.sum)[, 2],
    scen = 2
  )

  
  
  # combine dataframes 
  sim.data.all <- rbind.data.frame(sim.data.noex, 
                                   sim.data.ex) %>%
    filter(year >= st.year & year < max(obs.year)) %>%
    left_join(select(us.data, year, milex_SI))
  
  
  
  # Plot each 
  sim.nato.plot <- ggplot(sim.data.all, aes(x = year, y = sim.low,
                                            group = factor(scen),
                                            color = factor(scen))) +
    geom_line(aes(x = year, y = milex_SI)) +
    geom_errorbar(aes(ymin = sim.low,
                      ymax = sim.high
    ), width = .5, size = 1,
    position = position_dodge(width = 0.5)) +
    scale_color_manual(name = "Scenario",
                       values = c("#666666", "#333333"),
                       breaks = c("1", "2"),
                       labels = c("No Expansion", "Observed: Expansion")) +
    labs(x = "Year", 
         y = "Predicted Military Spending") +
    ggtitle(paste0("Predicted US Military Spending ", 
                   st.year, "-", max(obs.year)-1)) 
  sim.nato.plot
  
  
  # plot differences by year
  diff.nato.exp <- sim.expand - sim.no.expand
  diff.nato.exp <- apply(diff.nato.exp[, 1:(nrow(model$model)-1)], 2, 
                         function(x){quantile(x, c(0.025, .975), na.rm = TRUE)})
  
  
  diff.nato.data <- cbind.data.frame(
    year = obs.year[obs.year < max(obs.year)],
    lower = t(diff.nato.exp)[, 1],
    upper = t(diff.nato.exp)[, 2]
  ) %>% filter(year >= st.year)
  
  
  # plot predicted differences
  sim.nato.diff <- ggplot(diff.nato.data, aes(x = year, y = lower)) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper),
                  width = .5, size = 1) +
    labs(x = "year", 
         y = "Difference in Military Spending") +
    ggtitle("Difference Between Observed and Counterfactual Predictions")
  sim.nato.diff
  
  
  
  # plot the results
  grid.arrange(sim.nato.diff, sim.nato.plot,
               nrow = 2)
  nato.counterfactual <- arrangeGrob(sim.nato.diff, sim.nato.plot,
                                     nrow = 2)
  ggsave(paste0(folder, "/nato-counterfactual-adl-", log, ".tiff"),
         nato.counterfactual, 
         dpi = 300,
         height = 6, width = 8)  
} # end function


# run with ADL in level 
counterfactual.sim.adl(lm.adl, log = "nolog",
                       st.year = 2004, simulations = 1000,
                       folder = "figures")


# ADL in level with logs 
counterfactual.sim.adl(lm.adl.log, log = "log",
                       st.year = 2004, simulations = 1000,
                       folder = "appendix")

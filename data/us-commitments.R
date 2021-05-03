# Joshua Alley & Matthew Fuhrmann
# Measure of the number of US allies in a given year



# Load data
atop.ddy <- read.csv("data/atop4_01ddyr.csv")
atop <- read.csv("data/atop-alliance-level.csv")


# Focus on US alliances: 
us.ddy <- filter(atop.ddy, stateA == 2)

# fix a Spain data issue with ATOPID 
us.ddy$defense[us.ddy$year >= 1970 & us.ddy$stateB == 230] <- 1

# Select defense pacts only and key variables
us.ddy.def <- filter(us.ddy, defense == 1 | offense == 1) %>% 
               select(year, asymm, atopid1, atopid2, atopid3, atopid4,
                      stateB)

# pull out West Germany and Unified Germany in 1990:
# avoids a double-count with unified Germany
us.ddy.def <- filter(us.ddy.def, !(stateB == 260 & year == 1990)) %>%
               mutate(americas = ifelse(stateB < 200, 1, 0),
                      europe = ifelse(stateB >= 200 & stateB < 400, 1, 0),
                      asia = ifelse(stateB >= 400, 1, 0)) # no african states w/ formal pact

# place Turkey in Europe 
us.ddy.def$asia[us.ddy.def$stateB == 640] <- 0
us.ddy.def$europe[us.ddy.def$stateB == 640] <- 1

# Count up US Commitments by state
us.comm.total <- us.ddy.def %>%
            group_by(year) %>%
             summarize(
               commitments = n(),
               asymm = sum(asymm, na.rm = TRUE),
               americas_comm = sum(americas, na.rm = TRUE),
               europe_comm = sum(europe, na.rm = TRUE),
               asia_comm = sum(asia, na.rm = TRUE),
               .groups = "keep"
             )
# zero Asia commitments in some years 
us.comm.total$asia_comm[us.comm.total$asia_comm == -Inf] <- 0


# plot every state with a US commitment: 
ggplot(us.comm.total, aes(x = year, y = commitments)) + 
  geom_line(size = 1) +
  labs(x = "Year", y = "US Allies") +
  ggtitle("Number of US Treaty Allies: 1942-2016") 
ggsave("figures/alliances-total.png", height = 6, width = 8)


# plot regional commitments 
select(us.comm.total, year, 
                            americas_comm, europe_comm,
                            asia_comm) %>%
  pivot_longer(cols = c(americas_comm, europe_comm,
                        asia_comm)) %>%
ggplot(aes(x = year, y = value, group = name)) +
  geom_line(aes(linetype = name), size = 1) +
  labs(x = "Year", y = "Number of Commitments", linetype = "Commitment Type") 
#  scale_linetype_manual(values = c("solid", "dashed"), 
#                        labels = c("US Treaty Allies", "Alliance Treaties")) 


### Count membership by treaty
# There are 4 possible ATOPID variables, which makes counting difficult
# Use a for loop on each variable 
# Example of what's going into the for loop 
us.comm.aid1 <- us.ddy.def %>% 
                group_by(year, atopid1) %>%
                 summarize(
                   members = n()
                 ) 

# Count members by ATOPID variable 
atopid.list <- paste(colnames(us.ddy.def[3:6]))

# List ATOPID variables and loop over the list in group_by
us.comm.aid <- vector(mode = "list", length = length(atopid.list))

for(i in 1:length(atopid.list)){
  us.comm.aid[[i]] <- us.ddy.def %>% 
    group_by(year, get(atopid.list[i])) %>%
    summarize(
      members = n()
    ) 
}


# List has four separate datasets with a count of members by ATOPID: 
# merge these together, with full join, which retains all ATOPIDs 
# first and second
us.comm.treaty <- full_join(us.comm.aid[[1]], us.comm.aid[[2]], 
                            by = c("year", "get(atopid.list[i])"))
# Add the third dataset 
us.comm.treaty <- full_join(us.comm.treaty, us.comm.aid[[3]], 
                     by = c("year", "get(atopid.list[i])"))

# Add the fourth dataset
us.comm.treaty <- full_join(us.comm.treaty, us.comm.aid[[4]], 
                            by = c("year", "get(atopid.list[i])"))


# rename columns
colnames(us.comm.treaty) <- c("year", "atopid", "mem1", "mem2", 
                              "mem3", "mem4")

# calculate total members by treaty
us.comm.treaty <- us.comm.treaty %>%
                  drop_na(atopid) %>%
                  group_by(atopid, year) %>%
                   mutate(
                     members = sum(mem1, mem2, mem3, mem4, na.rm = TRUE)
                   )
us.comm.treaty <- left_join(us.comm.treaty, select(atop, atopid,
                                                   defense))
# going to miss one offensive alliance 
# filter out non-aggression/consultation pacts
us.comm.treaty <- filter(us.comm.treaty, defense == 1)

# Check work by looking at NATO
us.comm.treaty %>% filter(atopid == 3180) %>% 
     ggplot(aes(x = year, y = members)) +
       geom_line(size = 1) +
       ggtitle("Non-US NATO Members 1949-2016") +
       theme_bw()

# reshape data 
us.comm.treaty.wide <- select(us.comm.treaty, year, atopid, members) %>%
                        pivot_wider(names_from = atopid,
                                    values_from = members,
                                    values_fill = list(members = 0)
                        )

# Create count of bilateral and multilateral alliances
us.comm.lat <- filter(us.comm.treaty, year >= 1946) %>%
                mutate(
                  bilat = ifelse(members == 1, 1, 0),
                  multilat = ifelse(members > 1, 1, 0)
                ) %>%
              group_by(year, bilat) %>% 
               summarize( # bilateral and multilateral by year
                 treaty = n()
              #   comm = sum(members)
               ) %>% 
              mutate(
                bilat_treaty = ifelse(bilat == 1, treaty, NA),
                multilat_treaty = ifelse(bilat == 0, treaty, NA)
              #  bilat_comm = ifelse(bilat == 1, comm, NA),
              #  multilat_comm = ifelse(bilat == 0, comm, NA)
              ) %>% 
             group_by(year) %>%
              summarize(
                bilat_treaty = max(bilat_treaty, na.rm = TRUE),
                multilat_treaty = max(multilat_treaty, na.rm = TRUE)
              )

# fix a zero issue
us.comm.lat$bilat_treaty[us.comm.lat$bilat_treaty == -Inf] <- 0

# Heatmap of alliances over time
us.comm.treaty.wide %>% 
              pivot_longer(-year, names_to = "atopid",
              values_to = "members") %>%
  ggplot(aes(x = atopid, y = year, fill = members)) + 
  geom_tile() +
  scale_fill_gradientn(colours = c("white", "green", "red"), 
                       values = c(0, 1, 40)) +
  labs(x = "Alliance", y = "Year") +
  theme_bw()


# Without the zeros
us.comm.treaty.wide %>% 
  pivot_longer(-year, names_to = "atopid",
               values_to = "members") %>%
  filter(members > 0) %>%
ggplot(aes(x = atopid, y = year, fill = members)) + 
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = "Alliance", y = "Year") +
  ggtitle("Membership in Alliances with the United States: 1942-2016") +
  theme_bw()
ggsave("figures/alliances-split.png", height = 6, width = 8)


## Count up US Commitments by treaty 
us.comm.total.treaty <- us.comm.treaty %>%
  group_by(year) %>%
  summarize(
    treaties = n()
  ) %>% 
  right_join(us.comm.total)

ggplot(us.comm.total.treaty, aes(x = year, y = treaties)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "US Alliance Treaties") +
  ggtitle("Number of US Alliance Treaties: 1942-2016") +
  theme_bw()

# reshape long to plot both lines together
us.comm.total.long <- select(us.comm.total.treaty, year, treaties, commitments) %>%
  pivot_longer(cols = c(treaties, commitments))


# plot both lines together
ggplot(us.comm.total.long, aes(x = year, y = value, group = name)) +
   geom_line(aes(linetype = name), size = 1) +
  labs(x = "Year", y = "Number of Commitments", linetype = "Commitment Type") +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("US Treaty Allies", "Alliance Treaties")) +
  theme_bw() 
ggsave("appendix/treaties-and-members.png", height = 6, width = 8)



# create a table of all states with a US alliance commitment
us.partners <- select(us.ddy.def, stateB, year) %>%
                group_by(stateB) %>%
                filter(year >= 1946) %>%
                mutate(
                  alliance.year = min(year, na.rm = TRUE),
                  end.alliance = max(year, na.rm = TRUE),
                  country.name = countrycode(stateB, 
                                              origin = "cown",
                                              destination = "country.name"))
# non-allies
past.comm <- filter(us.partners, 
                    end.alliance != 2016 &
                    !is.na(country.name)) %>%
                ungroup() %>%
                select(country.name, alliance.year, end.alliance)
past.comm <- unique(past.comm) # all duplicates sans year variable
# past US allies table 
xtable(past.comm,
       caption = c("List of past formal U.S. treaty allies."),
       label = c("tab:us-oldally-list")
)


# allies at present
us.partners <- us.partners %>%
                filter(year == 2016) %>% 
                ungroup()
# US partners
us.partners <- rbind.data.frame(select(us.partners, country.name, alliance.year) %>%
                  arrange(., alliance.year),
                  c("North Macedonia", 2020)) # add north macedonia
xtable(us.partners,
       caption = c("List of states with a defensive alliance with the United States in 2020."),
       label = c("tab:us-ally-list")
)

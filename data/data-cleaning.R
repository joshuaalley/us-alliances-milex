# Joshua Alley and Matthew Fuhrmann
# Load data on US military spending and other variables

# run military spending data script
source("data/milex-data.R", echo = TRUE)

# Also load Gibler, Miller and Little's recoding of the MID data: directed-dyad data
gml.mid.part <- read.csv("data/gml-ddy-disputes-2.1.1.csv")

# Group data, collapse and get total disputes by year
gml.mid.part <- gml.mid.part %>%
  mutate(mid = 1) %>%
  select(ccode1, ccode2, year, dispnum, sidea2, mid) %>% 
  gather(side, ccode, c(ccode1, ccode2)) %>%
  select(-side)

gml.mid.part <- unique(gml.mid.part) 
gml.mid.part <- gml.mid.part %>%
  group_by(ccode, year) %>%
  summarize(
    mid.pres = 1,
    disputes = n(),
    .groups = "keep"
  ) %>% 
  filter(ccode == 2) %>% 
  group_by() %>%
  select(year, disputes)

# ICB data
icb13 <- read.csv("data/icb1v13.csv") %>%
          filter(usactor == 2) %>% # greater than low-level in
           group_by(yrtrig) %>%
            summarize(
              icb_count = n(),
              icb_level = sum(usinv, na.rm = TRUE),
              .groups = "keep"
            ) %>%
         rename(year = yrtrig)


# Load alt GDP data from FRED
us.gdp <- read.csv("data/fred-data.csv")
us.gdp$year <- as.numeric(substr(as.character(us.gdp$DATE), start = 6, stop = 9))

us.gdp <- select(us.gdp, -DATE) %>% 
            mutate(
              #GDP changes 
              ln_gdp = log(gdp),
              lag_gdp = lag(gdp),
              lag2_gdp = lag(lag_gdp),
              change_gdp = gdp - lag_gdp,
              lag_change_gdp = lag(change_gdp),
              growth_gdp = change_gdp / lag_gdp,
              lag_gdp_growth = lag(growth_gdp),
              
              # difference budget deficit
              lag_budget_deficit = lag(budget_deficit),
              lag2_budget_deficit = lag(lag_budget_deficit),
              change_budget_deficit = budget_deficit - lag(budget_deficit),
              lag_change_budget_deficit = lag(change_budget_deficit)
            )


# Load war fatalities data 
# from Mintz and Huang as well as
# Defense Manpower Data Center: http://icasualties.org/
# icasualties: https://fas.org/sgp/crs/natsec/RL32492.pdf
us.fatalities <- readxl::read_excel("data/fatalities_data.xlsx")
us.fatalities$log_fatalities_combined <- log(us.fatalities$fatalities_combined + 1)


# maddison GDP data
maddison.gdp <- read.csv("data/us-data-madison.csv")
maddison.gdp[, 4:5] <- apply(maddison.gdp[, 4:5], 2, function(x)
                      as.numeric(gsub(",", "", x)))
maddison.gdp$madd_gdp <- maddison.gdp$gdppc * maddison.gdp$pop

### Create the dataset: US data 
us.data <- us.spending.full %>%
           left_join(us.comm.total.treaty) %>% 
           left_join(us.comm.lat) %>%
           left_join(maddison.gdp) %>%
           left_join(us.gdp) %>%
           left_join(gml.mid.part) %>%
           left_join(icb13) %>%
           left_join(post45.rivals) %>%
           left_join(rival.cap.year) %>% 
           left_join(russian.spending) %>%
           left_join(us.fatalities)

# fix no-MIDs and ICB observations- no observed conflicts are NA
us.data$disputes[is.na(us.data$disputes) & us.data$year <= 2012] <- 0
us.data$icb_count[is.na(us.data$icb_count) & us.data$year <= 2016] <- 0
us.data$icb_level[is.na(us.data$icb_level) & us.data$year <= 2016] <- 0
# also a stretch in the 90s with no major power rivals
us.data$mprival_milex_sipri[is.na(us.data$mprival_milex_sipri)] <- 0


# address US alliances post 2016: add Montenegro to NATO for 67 total
us.data$commitments[us.data$year >= 2017] <- 67
us.data$europe_comm[us.data$year >= 2017] <- 28
us.data$americas_comm[us.data$year >= 2017] <- 33
us.data$americas_comm[us.data$year >= 2019] <- 32
us.data$asia_comm[us.data$year >= 2017] <- 6


# fill in alliance variables with zeros
us.data[, 3:10][is.na(us.data[, 3:10])] <- 0


# create a set of new variables
us.data <- mutate(us.data,
            cold_war = ifelse(year >= 1946 & year <= 1991, 1, 0),
            post45_years = year - 1945,
            post_911 = ifelse(year >= 2001, 1, 0),
            
            # log alliance commitments
            lag_commitments = lag(commitments),
            lag2_commitments = lag(lag_commitments),
            ln_commitments = log(commitments),
            lag_ln_commitments = lag(ln_commitments),
            lag2_ln_commitments = lag(lag_ln_commitments),               
             
            log_europe_comm = log(europe_comm),
            log_americas_comm = log(americas_comm),
            log_asia_comm = log(asia_comm),
            
            # log transform milex
            log_milex_SI = log(milex_SI),
            
            lag_milex_SI = lag(milex_SI), # lags 
            lag2_milex_SI = lag(lag_milex_SI),
            lag_log_milex_SI = lag(log_milex_SI), 
            
            change_milex_SI = milex_SI - lag_milex_SI, # changes
            change_log_milex_SI = log_milex_SI - lag_log_milex_SI,
            lag_change_milex_SI = lag(change_milex_SI),
            lag2_change_milex_SI = lag(lag_change_milex_SI),
            lag_change_log_milex_SI = lag(change_log_milex_SI), 
            lag2_change_log_milex_SI = lag(lag_change_log_milex_SI), 
            
            pc_milex_SI = change_milex_SI / lag_milex_SI, # % changes
            lag_pc_milex_SI = lag(pc_milex_SI), 
            lag2_pc_milex_SI = lag(lag_pc_milex_SI), 
            
            ihs_pc_SI = asinh(pc_milex_SI), # Inverse hyperbolic sine
            ihs_change_SI = asinh(change_milex_SI),
            
            # changes in commitments
            change_commitments = commitments - lag(commitments),
            lag_change_commitments = lag(change_commitments),
            lag_treaties = lag(treaties), 
            change_treaties = treaties - lag(treaties),
            lag_change_treaties = lag(change_treaties),
            
            change_ln_commitments = ln_commitments - lag(ln_commitments),
            lag_change_ln_commitments = lag(change_ln_commitments),
            
            change_europe_comm = europe_comm - lag(europe_comm),
            change_americas_comm = americas_comm - lag(americas_comm),
            change_asia_comm = asia_comm - lag(asia_comm),
            
            # lag rival spending
            lag_mprival_milex = lag(mprival_milex_sipri),
            lag_rival_cap = lag(total_rival_cap),
            lag_rival_milexc = lag(total_rival_milex),
            
            # interact soviet spending and cold war
            # variable enters in changes
            lag_russian_milex_combined = lag(russian_milex_combined),
            change_russian_milex_combined = russian_milex_combined - lag(russian_milex_combined),
            lag_change_russian_milex_combined = lag(change_russian_milex_combined),
            russian_milex_combined_cw = lag_russian_milex_combined * cold_war,
            
            # oatley security shocks measure
            oatley_shocks = ifelse((year >= 1950 & year <= 1953) |
                                   (year >= 1965 & year <= 1975) |
                                   (year >= 1979 & year <= 1988) |
                                   (year >= 2001 & year <= 2015),
                                   1, 0),
            
            # presidential partisanship
            rep_pres = ifelse((year >= 1921 & year <= 1932) | # Harding, Coolidge, Hoover
                              (year >= 1953 & year <= 1960) | # Ike
                              (year >= 1969 & year <= 1976) | # Nixon/Ford
                                (year >= 1981 & year <= 1992) | # Reagan/Bush
                                (year >= 2001 & year <= 2008) | # HW Bush
                                (year >= 2017), # Trump
                              1, 0),
            lag_rep_pres = lag(rep_pres), 
            
            # War: based on reiter et al data
            atwar = ifelse((year >= 1941 & year <= 1945) | # WWII 
                           (year >= 1950 & year <= 1953) | # Korea
                           (year >= 1965 & year <= 1973) | # Vietnam 
                           (year == 1991) | # Gulf War 1
                           (year == 1999) | # kosovo
                           (year == 2001) | # Afghanistan
                           (year >= 2003 & year <= 2011), # Iraq 
            1, 0),
            lag_atwar = lag(atwar)
          )

# zero logged commitments go to -Inf in some years 
us.data[us.data == -Inf] <- 0
# fatalities NA with data start
us.data$log_fatalities_combined[is.na(us.data$log_fatalities_combined)] <- 0

# List sequences of war and peace years
war.runs <- vector(mode = "list", length = length(rle(us.data$atwar)$length))
for(i in 1:length(rle(us.data$atwar)$length)){
  war.runs[[i]] <- data.frame(seq(1:rle(us.data$atwar)$length[i]))
}

# Add to data
war.runs <- bind_rows(war.runs)
us.data$conf_runs <- war.runs[, 1]
us.data <- us.data %>% mutate( 
  war_count = ifelse(atwar == 1, conf_runs, 0),
  peace_count = ifelse(atwar == 0, conf_runs, 0),
  peace_5 = ifelse(peace_count <= 5 & peace_count >= 1, 1, 0),
)


# Create a vector of presidential administrations
us.data$president <- rep(NA, times = nrow(us.data))
us.data$president[us.data$year >= 1913 & us.data$year < 1921] <- "Wilson"
us.data$president[us.data$year >= 1921 & us.data$year < 1923] <- "Harding"
us.data$president[us.data$year >= 1923 & us.data$year < 1929] <- "Coolidge"
us.data$president[us.data$year >= 1929 & us.data$year < 1933] <- "Hoover"
us.data$president[us.data$year >= 1933 & us.data$year < 1945] <- "Roosevelt"
us.data$president[us.data$year >= 1945 & us.data$year < 1953] <- "Truman"
us.data$president[us.data$year >= 1953 & us.data$year < 1961] <- "Eisenhower"
us.data$president[us.data$year >= 1961 & us.data$year < 1964] <- "Kennedy"
us.data$president[us.data$year >= 1964 & us.data$year < 1969] <- "Johnson"
us.data$president[us.data$year >= 1969 & us.data$year <= 1974] <- "Nixon"
us.data$president[us.data$year > 1974 & us.data$year < 1977] <- "Ford"
us.data$president[us.data$year >= 1977 & us.data$year < 1981] <- "Carter"
us.data$president[us.data$year >= 1981 & us.data$year < 1989] <- "Reagan"
us.data$president[us.data$year >= 1989 & us.data$year < 1993] <- "HW Bush"
us.data$president[us.data$year >= 1993 & us.data$year < 2001] <- "Clinton"
us.data$president[us.data$year >= 2001 & us.data$year < 2009] <- "W Bush"
us.data$president[us.data$year >= 2009 & us.data$year < 2017] <- "Obama"
us.data$president[us.data$year >= 2017] <- "Trump"

# Create a set of decade and year dummies
us.data <- us.data %>% 
  mutate(
    dec_20 = ifelse(year >= 1920 & year < 1930, 1, 0),
    dec_30 = ifelse(year >= 1930 & year < 1940, 1, 0),
    dec_40 = ifelse(year >= 1940 & year < 1950, 1, 0),
    dec_50 = ifelse(year >= 1950 & year < 1960, 1, 0),
    dec_60 = ifelse(year >= 1960 & year < 1970, 1, 0),
    dec_70 = ifelse(year >= 1970 & year < 1980, 1, 0),
    dec_80 = ifelse(year >= 1980 & year < 1990, 1, 0),
    dec_90 = ifelse(year >= 1990 & year < 2000, 1, 0),
    dec_00 = ifelse(year >= 2000 & year < 2010, 1, 0),
    dec_10 = ifelse(year >= 2010 & year < 2020, 1, 0),
    
    # dummies for years with big alliance change and milex increase
    year_51 = ifelse(year == 1951, 1, 0),
    year_03 = ifelse(year == 2003, 1, 0),
    pre_52 = ifelse(year >= 1952, 1, 0),
    iraq_dum = ifelse(year >= 2003 & year <= 2012, 1, 0),
    korea_dum = ifelse(year >= 1950 & year <= 1953, 1, 0),
    vietnam_dum = ifelse(year >= 1965 & year <= 1973, 1, 0)
  )

# Presidential administration dummies
us.pres.dum <- data.frame(model.matrix(~ factor(us.data$president) + 0))
colnames(us.pres.dum) <- str_remove(colnames(us.pres.dum), 
                                    "factor.us.data.president.")
us.data <- cbind(us.data, us.pres.dum)
  

# data on deployments to Korea, Vietnam, Kuwait, Iraq and Afghanistan
us.data$major_com_dep <- 0
# Korea and Vietnam using replication data from: https://academic.oup.com/fpa/article-abstract/12/4/674/2469900
# available on Michael Allen's website: http://ma-allen.com/publication-and-data/ 
# Korea
  us.data$major_com_dep[us.data$year == 1950] <- 80000 # Pre-war.... 
  us.data$major_com_dep[us.data$year == 1951] <- 450000 # interpolated.
  us.data$major_com_dep[us.data$year == 1952] <- 480000 # interpolated.  
  us.data$major_com_dep[us.data$year == 1953] <- 326863
  us.data$major_com_dep[us.data$year == 1954] <- 225590
# Vietnam
  us.data$major_com_dep[us.data$year == 1962] <- 8498
  us.data$major_com_dep[us.data$year == 1963] <- 15620
  us.data$major_com_dep[us.data$year == 1964] <- 17280
  us.data$major_com_dep[us.data$year == 1965] <- 129611
  us.data$major_com_dep[us.data$year == 1966] <- 317007
  us.data$major_com_dep[us.data$year == 1967] <- 451752
  us.data$major_com_dep[us.data$year == 1968] <- 537377
  us.data$major_com_dep[us.data$year == 1969] <- 510054
  us.data$major_com_dep[us.data$year == 1970] <- 390278
  us.data$major_com_dep[us.data$year == 1971] <- 212925
  us.data$major_com_dep[us.data$year == 1972] <- 35292
  us.data$major_com_dep[us.data$year == 1973] <- 265
# Gulf War https://www.defense.gov/Explore/Features/story/Article/1728715/desert-storm-a-look-back/
  us.data$major_com_dep[us.data$year == 1991] <- 697000 
# Iraq and Afghanistan: https://fas.org/sgp/crs/natsec/R40682.pdf
  us.data$major_com_dep[us.data$year == 2001] <- 2000
  us.data$major_com_dep[us.data$year == 2002] <- 5200
  us.data$major_com_dep[us.data$year == 2003] <- 78100
  us.data$major_com_dep[us.data$year == 2004] <- 145800
  us.data$major_com_dep[us.data$year == 2005] <- 162900
  us.data$major_com_dep[us.data$year == 2006] <- 161500
  us.data$major_com_dep[us.data$year == 2007] <- 172000
  us.data$major_com_dep[us.data$year == 2008] <- 187900
  us.data$major_com_dep[us.data$year == 2009] <- 186300
  us.data$major_com_dep[us.data$year == 2010] <- 151800
  us.data$major_com_dep[us.data$year == 2011] <- 106200
  us.data$major_com_dep[us.data$year == 2012] <- 67500
# Afghanistan from here on: https://fas.org/sgp/crs/natsec/R44116.pdf
# Taking max of the quarterly values for each year
  us.data$major_com_dep[us.data$year == 2013] <- 65800
  us.data$major_com_dep[us.data$year == 2014] <- 43400
  # us.data$major_com_dep[us.data$year == 2015] <- 9100
  # us.data$major_com_dep[us.data$year == 2016] <- 9800
  # us.data$major_com_dep[us.data$year == 2017] <- 8300 # higher value reflects a revision
  
  
# summarize results
summary(us.data$major_com_dep)
us.data$major_com_dep
us.data$ln_majorcom_dep <- log(us.data$major_com_dep + 1)


# Iraq, Vietnam and Korea split of deployments
# iraq
us.data$iraq_dep <- us.data$major_com_dep
us.data$iraq_dep[us.data$year < 2001] <- 0
us.data$iraq_dep
# vietnam
us.data$vietnam_dep <- us.data$major_com_dep
us.data$vietnam_dep[us.data$year < 1962 | us.data$year > 1973] <- 0
us.data$vietnam_dep
# Korea
us.data$korea_dep <- us.data$major_com_dep
us.data$korea_dep[us.data$year > 1954] <- 0
us.data$korea_dep


# Compare the transformed and untransformed outcomes
# creates a thoroughly bimodal distribution
ggplot(us.data, aes(x = milex_SI)) + geom_histogram()
ggplot(us.data, aes(x = log_milex_SI)) + geom_histogram()

ggplot(us.data, aes(x = year, y = log_milex_SI)) + geom_line()

# NB: GDP and milex have different dollar conversions (2018 milex, 2005 GDP)
# GDP is in billions. 
# MIDS end in 2010. 

# Different time samples
us.data.full <- filter(us.data, year >= 1946)
us.data.1955 <- filter(us.data, year >= 1954)

# Filter down 
us.data.pre45 <- us.data # for analysis before 1945.
# filter to after 1947
us.data <- filter(us.data, year >= 1947) 

# Make US data a time-series for dynlm
us.data.ts <- ts(us.data, start = 1947, end = 2019, frequency = 1)
class(us.data.ts)

# plot key variables over time
key.vars <- select(us.data, year, milex_SI, 
                   lag_commitments,
                   peace_5, log_fatalities_combined,
                     lag_rep_pres, cold_war,
                     lag_budget_deficit, lag_change_gdp,
                     lag_mprival_milex)

pivot_longer(key.vars, -year) %>% 
  ggplot(aes(x = year, y = value)) +
    facet_wrap(~name, scales = "free") +
    geom_line()

# table of key variables for the appendix
stargazer(select(key.vars, -year),
          label = "tab:summary-stat",
          covariate.labels = c("Military Spending", "Lag Alliance Commitments",
                               "Post-Conflict Period", "Log Fatalities",
                               "Lag Republican President", "Cold War",
                               "Lag Budget Deficit", "Lag Change GDP",
                               "Lag Major Rival Spending"))



# Plot with only the IV and DV
select(us.data, year, milex_SI, # select variables and years, pivot long
                   commitments) %>%
  filter(year >= 1947) %>%
  pivot_longer(-year) %>% 
ggplot( aes(x = year, y = value)) + # plot 
  facet_wrap(~name, scales = "free",
             labeller = labeller(name = c(commitments = "Alliance Commitments",
                                milex_SI = "Military Spending"))
             ) +
  geom_line(size = 1) +
  labs(x = "Year") +
  theme_bw()
ggsave("figures/outcome-iv.tiff", 
       dpi = 300,
       height = 6, width = 8)



# Plot three formulations of outcome
select(us.data, year, milex_SI, # select variables and years, pivot long
       change_milex_SI, pc_milex_SI) %>%
  pivot_longer(-year) %>% 
  ggplot( aes(x = year, y = value)) + # plot 
  facet_wrap(~name, scales = "free",
             labeller = labeller(name = c(change_milex_SI = "Changes", 
                                          milex_SI = "Levels",
                                          pc_milex_SI = "Percentage Change"))
  ) +
  geom_line(size = .5) +
  labs(x = "Year") +
  theme_bw()




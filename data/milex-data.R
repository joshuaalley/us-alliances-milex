# Joshua Alley and Matthew Fuhrmann
# Combine data on US military spending



# US military spending data from SIPRI 
sipri.spending <- read.csv("data/us-spending-sipri.csv")

us.spending <- sipri.spending %>%
  pivot_longer(-Country, names_to = "year") %>% 
  drop_na() %>%
  rename(milex_SI = value) %>%
  select(year, milex_SI)
us.spending$year <- as.numeric(substring(us.spending$year, 2, 5))

# Add spending from 1945 to 1948
# took data from here: http://academic.brooklyn.cuny.edu/history/johnson/milspend.htm
# rebased to 2018 dollars (January as month) with: https://www.bls.gov/data/inflation_calculator.htm

# 1945: 1545476
# 1946: 803641
#  1947: 214636
# 1948: 152027
spending.pre49 <- cbind.data.frame(
  year = c(1945, 1946, 1947, 1948),
  milex_SI = c(1545476, 803641, 214636, 152027)
)



# Source for pre-1940 data here (see table 8): 
  # https://www.cna.org/CNA_files/PDF/D0007249.A1.pdf
# Reported in millions of 1937 dollars: converted with:
  # https://www.bls.gov/data/inflation_calculator.htm

### starts in 1920: report values then enter in data vectors 
# 1920: 57,941.11    1921: 31,677.75
# 1922: 14,502.86    1923: 11,953.87
# 1924: 11,180.38    1925: 10,600.27
# 1926: 10,494.79    1927: 11,145.23
# 1928: 12,199.98    1929: 13,500.84
# 1930: 14,854.44    1931: 15,135.71
# 1932: 15,329.08    1933: 14,784.12
# 1934: 12,815.25    1935: 16,049.83
# 1936: 19,811.78    1937: 19,723.88
# 1938: 20,690.74    1939: 22,852.99
# 1940: 41,399.06

spending.pre40 <- cbind.data.frame(
  year = c(1920, 1921, 1922, 1923,
            1924, 1925, 1926, 1927,
             1928, 1929, 1930, 1931,
              1932, 1933, 1934, 1935,
               1936, 1937, 1938, 1939,
                1940),
  milex_SI = c(57941.11, 31677.75, 14502.86, 11953.87,
                11180.38, 10600.27, 10494.79, 11145.23,
                 12199.98, 13500.84, 14854.44, 15135.71,
                  15329.08, 14784.12, 12815.25, 16049.83,
                   19811.78, 19723.88, 20690.74, 22852.99,
                    41399.06)
)

# data from 1941 to 1944: Table 3.1 here: 
  # https://www.whitehouse.gov/omb/historical-tables/ 
  # in current dollars
spending.ww2 <- cbind.data.frame(
  year = c(1941, 1942, 
            1943, 1944),
  milex_SI = c(113122.28, 405080.99,
               978253.32, 1127410.23)
)

# unified spending 
us.spending.full <- rbind(spending.pre40, spending.ww2,
                          spending.pre49, us.spending)

# Convert to Billions by dividing by 1000 
us.spending.full$milex_SI <- us.spending.full$milex_SI / 1000

# check with a plot
ggplot(us.spending.full, aes(x = year, y = milex_SI)) + geom_line()



### Look at number of strategic rivals and their military spending
# strategic rivals data through 2008. Less than ideal. 
td.rivalry <- read.csv("data/thompson-dreyer-rivalry.csv") %>%
               filter(year >= 1919 & (ccode1 == 2 | ccode2 == 2) &
                        rivalry == 1)
table(td.rivalry$ccode2)
table(td.rivalry$rivalry)
td.rivalry <- select(td.rivalry, 
                     ccode2, year) %>%
               rename(ccode = ccode2)
# add three rivals through 2018
td.additions <- as.data.frame(expand.grid(td.rivalry[172:174, 1],
                   c(2009, 2010, 2011, 2012, 2013, 2014,
                    2015, 2016, 2017, 2018)))
colnames(td.additions) <- c("ccode", "year")
td.rivalry <- rbind(td.rivalry, 
          td.additions
         )



# Get data on rival spending from COW
cinc.data <- read.csv("data/NMC_5_0.csv") %>%
               select(ccode, year, milex, cinc)
# load CPI for conversion
cpi.2018 <- haven::read_dta("data/cpi_avg.dta")


# look at SIPRI as well 
# load our russian spending data here as well
russian.spending <- read.csv("data/russian-spending.csv")

# load china cinc data
cinc.china <- filter(cinc.data, ccode == 740 & year >= 1913) %>%
               left_join(cpi.2018) %>%
                mutate(
                milex_constant = (milex * cpi_avg_2018) / cpi_avg
                ) %>%
                select(year, milex_constant)
# convert milex to billions
cinc.china$milex_constant <- cinc.china$milex_constant / 1000000

# pull Cuban and Chinese spending: both strategic rivals
ch.cb.spend <- read.csv("data/china-cuba-sipri.csv") %>%
                 pivot_longer(-c(country, ccode),
                              names_to = "year") %>%
                 rename(milex = value) %>%
                 mutate(milex = milex / 1000) # convert to billions
ch.cb.spend$year <- as.numeric(str_remove(ch.cb.spend$year, "X"))

# pull china observations w/o rivalry
# pull cuba entirely- major power rivalry 
ch.spend <- filter(ch.cb.spend,
                      !(ccode == 710 & (year < 1996 & year > 1972)) &
                          ccode != 40) %>%
              left_join(cinc.china)
ch.spend$milex[is.na(ch.spend$milex)] <- ch.spend$milex_constant
                    
# combine with russian spending
post45.rivals <- rbind.data.frame(select(ch.spend, year, milex),
                                  filter(rename(russian.spending,
                                         milex = russian_milex_combined),
                                  year <= 1989 | year >= 2007)) %>%
                group_by(year) %>%
                summarize(
                  mprival_milex_sipri = sum(milex, na.rm = TRUE),
                  .groups = "keep"
                ) %>% filter(year < 2020)
ggplot(post45.rivals, aes(x = year, y = mprival_milex_sipri)) + geom_line()


# Alternative approach with the cINC data
# combine the different measures 
rival.cap <- td.rivalry %>%
              left_join(cinc.data)
# summarize by year
rival.cap.year <- rival.cap %>%
                   group_by(year) %>%
                   summarize(
                     total_rival_cap = sum(cinc, na.rm = TRUE),
                     total_rival_milex = sum(milex, na.rm = TRUE) / 1000000, 
                     total_rivals = n(),
                     .groups = "keep"
                   ) %>%
                filter(year < 2012)
ggplot(rival.cap.year, aes(x = year, y = total_rival_cap)) +
   geom_line()
ggplot(rival.cap.year, aes(x = year, y = total_rival_milex)) +
  geom_line()
ggplot(rival.cap.year, aes(x = year, y = total_rivals)) +
  geom_line()


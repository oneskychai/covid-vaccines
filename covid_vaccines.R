# This script analyzes COVID vaccines tweets alongside
# global vaccination progress
# Data downloaded from kaggle
# Vaccine tweets: https://www.kaggle.com/gpreda/all-covid19-vaccines-tweets
# Vaccine progress: https://www.kaggle.com/gpreda/covid-world-vaccination-progress
# Country populations: https://www.kaggle.com/tanuprabhu/population-by-country-2020
# Country continents: https://www.kaggle.com/statchaitya/country-to-continent

# Install necessary libraries
if (!require(tidyverse))
  install.packages("tidyverse", repos="http://cran.r-project.org")
if (!require(tidytext))
  install.packages("tidytext", repos="http://cran.r-project.org")
if (!require(textdata))
  install.packages("textdata", repos="http://cran.r-project.org")
if (!require(lexicon))
  install.packages("lexicon", repos="http://cran.r-project.org")
if (!require(sentimentr))
  install.packages("sentimentr", repos="http://cran.r-project.org")
if (!require(ggrepel))
  install.packages("ggrepel", repos="http://cran.r-project.org")
if (!require(scales))
  install.packages("scales", repos="http://cran.r-project.org")
if (!require(plotly))
  install.packages("plotly", repos="http://cran.r-project.org")
if (!require(gganimate))
  install.packages("gganimate", repos="http://cran.r-project.org")
if (!require(gifski))
  install.packages("gifski", repos="http://cran.r-project.org")
if (!require(zoo))
  install.packages("zoo", repos="http://cran.r-project.org")

# Load necessary libraries
library("tidyverse")
library("tidytext")
library("textdata")
library("lexicon")
library("sentimentr")
library("ggrepel")
library("scales")
library("plotly")
library("gganimate")
library("gifski")
library("zoo")

# Read data in from github
progress <- read_csv("https://raw.githubusercontent.com/oneskychai/covid-vaccines/trunk/country_vaccinations.csv")
tweets <- read_csv("https://raw.githubusercontent.com/oneskychai/covid-vaccines/trunk/vaccination_all_tweets.csv")
populations <- read_csv("https://raw.githubusercontent.com/oneskychai/covid-vaccines/trunk/population_by_country_2020.csv")
continents <- read_csv("https://raw.githubusercontent.com/oneskychai/covid-vaccines/trunk/countryContinent.csv")
vacs_by_man <- read_csv("https://raw.githubusercontent.com/oneskychai/covid-vaccines/trunk/country_vaccinations_by_manufacturer.csv")

# Turn off scientific notation and set digits to 3
options(scipen = 999,
        digits = 3)

# View data
view(progress)
view(tweets)
view(populations)

#================#
# Clean progress #
#================#

# Summarize progress data by country
country_sums <- progress %>%
  group_by(country) %>%
  summarize(iso_code = iso_code,
            total_vacs = max(total_vaccinations, na.rm = TRUE),
            people_vaxed = max(people_vaccinated, na.rm = TRUE),
            people_fully_vaxed = max(people_fully_vaccinated, na.rm = TRUE),
            sum_raw = sum(daily_vaccinations_raw, na.rm = TRUE),
            sum = sum(daily_vaccinations, na.rm = TRUE))

# View country sums
view(country_sums)

# Remove daily_vaccinations_raw column from progress
# Remove sum_raw column from country_sums
# These columns are very inconsistent with the rest of the data
progress <- progress[,-7]
country_sums <- country_sums[,-6]

# Make vector of country names
# Count number of countries
countries <- progress %>%
  .$country %>%
  unique()
length(countries)

#----------------------------------------------#
# Replace all NA's in daily_vaccination column #
#----------------------------------------------#

# Create vector of indexes for NA's in daily_vaccination column
# Count NA's in daily_vaccinations column
ind_na <- which(is.na(progress$daily_vaccinations))
length(ind_na)

# Create vector of indexes for first row of each country
ind_first <- sapply(countries, function(c) first(which(progress$country == c)))

# Inspect rows in ind_na not in ind_first
ind <- setdiff(ind_na, ind_first)
progress[ind,] %>% view()

# Set NA's in daily_vaccinations column of first rows of countries to 0 or
# total_vaccinations or sum of people_vaccinated and people_fully_vaccinated,
# whichever is larger
# If larger value is larger than first non-NA value for daily_vaccinations for
# that country, set value to first non-NA value for that country
values <- sapply(ind_first, function(ind) {
  
  # Get value in total_vaccinations column
  total <- progress$total_vaccinations[ind]
  
  # Calculate sum of people_vaccinated and people_fully_vaccinated
  # Set value to 0 if NA
  sum <- progress[ind,] %>%
    mutate(p_v = ifelse(is.na(people_vaccinated), 0, people_vaccinated),
           p_f_v = ifelse(is.na(people_fully_vaccinated),
                          0,
                          people_fully_vaccinated),
           sum = p_v + p_f_v) %>%
    .$sum
  
  # Take the greater of total and sum
  max(c(total, sum), na.rm = TRUE)
})

# Find first non-NA value of daily_vaccinations column for each country
country_first <- sapply(countries, function(c) {
  progress %>%
    filter(country == c & !is.na(daily_vaccinations)) %>%
    .$daily_vaccinations %>%
    first()
})

# If there are no non-NA values of daily_vaccinations for a country,
# country_first will be NA for that country
# Count NA's in country_first
sum(is.na(country_first))

# Replace NA's in country_first with value in values
country_first <- ifelse(is.na(country_first), values, country_first)

# Display range of values
range(values)

# Display 5 largest values
top_5 <- head(sort(values, decreasing = TRUE), 5)
top_5

# Display country_first for countries with 5 largest values
names <- names(top_5)
country_first[names]

# If values is greater than country_first, replace with country_first
values <- ifelse(values > country_first, country_first, values)

# Set first row of daily_vaccinations for each country to values
progress$daily_vaccinations[ind_first] <- values

#..............................................#
# Create rows for 13 missing dates for Belarus #
#''''''''''''''''''''''''''''''''''''''''''''''#

# Find row for Belarus with NA in daily_vaccinations column
ind <- which(is.na(progress$daily_vaccinations) & progress$country == "Belarus")

# Copy that row
row <- progress[ind,]

# Set people_vaccinated to NA
row$people_vaccinated <- NA

# Calculate number of days between row and previous row
days <- as.numeric(progress$date[ind] - progress$date[ind - 1])

# Calculate number of total vaccinations for those days
vacs <- progress$people_vaccinated[ind] - progress$people_vaccinated[ind - 1]

# Set daily_vaccinations in row to rounded average
row$daily_vaccinations <- round(vacs / days)

# Make data frame with row repeated days - 1 times
df <- row[rep(1, days - 1),]

# Create sequence of missing dates
dates <- (progress$date[ind - 1] + 1):(progress$date[ind] - 1)

# Convert dates to Date format
dates <- as.Date(dates, origin = "1970-01-01")

# Set date column in df to dates vector
df$date <- dates

# Add df to progress and arrange in proper order
progress <- progress %>%
  rbind(df) %>%
  arrange(country, date)

# Set NA value for daily_vaccinations column for Belarus
value <- vacs - (days - 1) * row$daily_vaccinations
progress$daily_vaccinations[ind + days - 1] <- value

#...........................................#
# Create rows for 6 missing dates for Egypt #
#'''''''''''''''''''''''''''''''''''''''''''#

# Find row for Egypt with NA in daily_vaccinations column
ind <- which(is.na(progress$daily_vaccinations) & progress$country == "Egypt")

# Copy that row
row <- progress[ind,]

# Set people_vaccinated to NA
row$people_vaccinated <- NA

# Calculate number of days between row and previous row
days <- as.numeric(progress$date[ind] - progress$date[ind - 1])

# Get value of daily_vaccinations from previous row
# Daily vaccinations column is very consistent, so we use this number
value <- progress$daily_vaccinations[ind - 1]

# Set daily_vaccinations in row to value
row$daily_vaccinations <- value

# Make data frame with row repeated days - 1 times
df <- row[rep(1, days - 1),]

# Create sequence of missing dates
dates <- (progress$date[ind - 1] + 1):(progress$date[ind] - 1)

# Convert dates to Date format
dates <- as.Date(dates, origin = "1970-01-01")

# Set date column in df to dates vector
df$date <- dates

# Add df to progress and arrange in proper order
progress <- progress %>%
  rbind(df) %>%
  arrange(country, date)

# Set NA value for daily_vaccinations column for Egypt
progress$daily_vaccinations[ind + days - 1] <- value

#............................................................#
# Calculate NA values for daily_vaccinations for El Salvador #
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#

# Find which rows for El Salvador have NA's for daily_vaccinations
ind <- which(is.na(progress$daily_vaccinations) &
               progress$country == "El Salvador")

# Set value to 0
value <- 0

# Iterate over rows in ind and set values for people_fully_vaccinated
for (i in ind) {
  
  # Set people_fully_vaccinated NA's to 0 until non-NA value is reached
  # Set people_fully_vaccinated NA's to previous non-NA value after
  if (is.na(progress$people_fully_vaccinated[i])) {
    progress$people_fully_vaccinated[i] <- value
  }
  else {
    value <- progress$people_fully_vaccinated[i]
  }
}

# Iterate over rows in ind and set values for total_vaccinations to sum of
# people_vaccinated and people_fully_vaccinated
for (i in ind) {
  progress$total_vaccinations[i] <- progress$people_vaccinated[i] +
    progress$people_fully_vaccinated[i]
}

# Iterate over rows in ind and set values for daily_vaccinations to difference
# between total_vaccinations for row and previous row
for (i in ind) {
  progress$daily_vaccinations[i] <- progress$total_vaccinations[i] -
    progress$total_vaccinations[i - 1]
}

#................................................#
# Set NA values in daily_vaccinations for Guinea #
#''''''''''''''''''''''''''''''''''''''''''''''''#

# Find which rows for Guinea have NA's for daily_vaccinations
ind <- which(is.na(progress$daily_vaccinations) & progress$country == "Guinea")

# Get value of daily_vaccinations from previous non-NA row
value <- progress$daily_vaccinations[ind[1] - 1]

# Set NA's to this value as daily_vaccinations column is very consistent here
progress$daily_vaccinations[ind] <- value

#.............................................#
# Create rows for 19 missing dates for Kuwait #
#'''''''''''''''''''''''''''''''''''''''''''''#

# Find which row for Kuwait has NA for daily_vaccinations
ind <- which(is.na(progress$daily_vaccinations) & progress$country == "Kuwait")

# Copy that row
row <- progress[ind,]

# Set people_vaccinated to NA
row$people_vaccinated <- NA

# Calculate number of days between row and previous row
days <- as.numeric(progress$date[ind] - progress$date[ind - 1])

# Get last non-NA value of people_fully_vaccinated
value <- progress %>%
  filter(country == "Kuwait" & !is.na(people_fully_vaccinated)) %>%
  .$people_fully_vaccinated %>%
  last()

# Calculate number of total vaccinations for missing days
vacs <- progress$people_vaccinated[ind] + value -
  progress$total_vaccinations[ind - 1]

# Set daily_vaccinations in row to rounded average
row$daily_vaccinations <- round(vacs / days)

# Make data frame with row repeated days - 1 times
df <- row[rep(1, days - 1),]

# Create sequence of missing dates
dates <- (progress$date[ind - 1] + 1):(progress$date[ind] - 1)

# Convert dates to Date format
dates <- as.Date(dates, origin = "1970-01-01")

# Set date column in df to dates vector
df$date <- dates

# Add df to progress and arrange in proper order
progress <- progress %>%
  rbind(df) %>%
  arrange(country, date)

# Set NA value for daily_vaccinations column for Kuwait
value <- vacs - (days - 1) * row$daily_vaccinations
progress$daily_vaccinations[ind + days - 1] <- value

#.................................................................#
# Calculate 17 NA values for daily_vaccinations column for Latvia #
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#

progress <- progress %>%
  
  # If people_fully_vaccinated is NA, create 0 placeholder
  mutate(p_f_v = ifelse(country == "Latvia" & is.na(people_fully_vaccinated),
                        0,
                        people_fully_vaccinated),
         
         # Calculate difference between sum of people_vaccinated and
         # people_fully_vaccinated for each row and previous row
         # This is the daily_vaccinations for that row
         daily_vaccinations =
           ifelse(country == "Latvia" & is.na(daily_vaccinations),
                  people_vaccinated + p_f_v -
                    lag(people_vaccinated) - lag(p_f_v),
                  daily_vaccinations)) %>%
  
  # Remove placeholder column
  select(-p_f_v)

#....................................................#
# Create rows for first 5 missing dates for Pakistan #
#''''''''''''''''''''''''''''''''''''''''''''''''''''#

# Find first row for Pakistan with NA for daily_vaccinations
ind <- which(is.na(progress$daily_vaccinations) &
               progress$country == "Pakistan")[1]

# Copy that row
row <- progress[ind,]

# Set people_vaccinated to NA
row$people_vaccinated <- NA

# Calculate number of days between row and previous row
days <- as.numeric(progress$date[ind] - progress$date[ind - 1])

# Calculate number of total vaccinations for those days
vacs <- progress$people_vaccinated[ind] - progress$total_vaccinations[ind - 1]

# Set daily_vaccinations in row to rounded average
row$daily_vaccinations <- round(vacs / days)

# Make data frame with row repeated days - 1 times
df <- row[rep(1, days - 1),]

# Create sequence of missing dates
dates <- (progress$date[ind - 1] + 1):(progress$date[ind] - 1)

# Convert dates to Date format
dates <- as.Date(dates, origin = "1970-01-01")

# Set date column in df to dates vector
df$date <- dates

# Add df to progress and arrange in proper order
progress <- progress %>%
  rbind(df) %>%
  arrange(country, date)

# Set first NA value for daily_vaccinations column for Pakistan
value <- vacs - (days - 1) * row$daily_vaccinations
progress$daily_vaccinations[ind + days - 1] <- value

#...............................................#
# Create row for next missing date for Pakistan #
#'''''''''''''''''''''''''''''''''''''''''''''''#

# Find next row for Pakistan with NA for daily_vaccinations
ind <- which(is.na(progress$daily_vaccinations) &
               progress$country == "Pakistan")[1]

# Copy that row
row <- progress[ind,]

# Set people_vaccinated to NA
row$people_vaccinated <- NA

# Calculate number of days between row and previous row
days <- as.numeric(progress$date[ind] - progress$date[ind - 1])

# Calculate number of total vaccinations for those days
vacs <- progress$people_vaccinated[ind] - progress$people_vaccinated[ind - 1]

# Set daily_vaccinations in row to rounded average
row$daily_vaccinations <- round(vacs / days)

# Create missing date
dates <- progress$date[ind - 1] + 1

# Set date in row to missing date
row$date <- dates

# Add row to progress and arrange in proper order
progress <- progress %>%
  rbind(row) %>%
  arrange(country, date)

# Set next NA value for daily_vaccinations column for Pakistan
value <- vacs - row$daily_vaccinations
progress$daily_vaccinations[ind + 1] <- value

#...................................................#
# Create rows for last 4 missing dates for Pakistan #
#'''''''''''''''''''''''''''''''''''''''''''''''''''#

# Find last row for Pakistan with NA for daily_vaccinations
ind <- which(is.na(progress$daily_vaccinations) &
               progress$country == "Pakistan")

# Copy that row
row <- progress[ind,]

# Set people_vaccinated to NA
row$people_vaccinated <- NA

# Calculate number of days between row and previous row
days <- as.numeric(progress$date[ind] - progress$date[ind - 1])

# Calculate number of total vaccinations for those days
vacs <- progress$people_vaccinated[ind] - progress$people_vaccinated[ind - 1]

# Set daily_vaccinations in row to rounded average
row$daily_vaccinations <- round(vacs / days)

# Make data frame with row repeated days - 1 times
df <- row[rep(1, days - 1),]

# Create sequence of missing dates
dates <- (progress$date[ind - 1] + 1):(progress$date[ind] - 1)

# Convert dates to Date format
dates <- as.Date(dates, origin = "1970-01-01")

# Set date column in df to dates vector
df$date <- dates

# Add df to progress and arrange in proper order
progress <- progress %>%
  rbind(df) %>%
  arrange(country, date)

# Set last NA value for daily_vaccinations column for Pakistan
value <- vacs - (days - 1) * row$daily_vaccinations
progress$daily_vaccinations[ind + days - 1] <- value

# Make sure there are no NA's left in daily_vaccinations column
sum(is.na(progress$daily_vaccinations))

# Clean up
rm(df, row, country_first, dates, days, names, top_5, vacs, value, values)

#--------------------------------------#
# Find any other date gaps in progress #
#--------------------------------------#

# Create empty vector to store indexes with date gaps
ind_gaps <- integer(0)
for (i in 2:nrow(progress)) {
  if (progress$date[i] - progress$date[i - 1] > 1) {
    ind_gaps <- append(ind_gaps, i)
  }
}

# Inspect rows with date gaps
ind <- c(ind_gaps, ind_gaps - 1)
ind <- sort(ind)
ind <- unique(ind)
view(progress[ind,])
# Note the only gaps needing attention are for Guinea 3/19 - 3/23 - 3/31
# The numbers for the missing dates for El Salvador and Latvia add up correctly
# The other date gaps are for rows with different countries

# Create vector of missing dates for Guinea
missing_dates_guinea <-
  seq(as.Date("2021-03-20"), as.Date("2021-03-30"), "days")
missing_dates_guinea <- missing_dates_guinea[-4]

#------------------------------------------#
# Create rows to fill date gaps for Guinea #
#------------------------------------------#

# Copy first row in Guinea with a date gap
row <- progress[ind_gaps[3],]

# daily_vaccinations column for Guinea is very consistent around missing dates
# Leave daily_vaccinations as is
# Set people_vaccinated to NA
row$people_vaccinated <- NA

# Make data frame with copies of row for dates in missing_dates_guinea
df <- row[rep(1, length(missing_dates_guinea)),]

# Set date column in df to missing_dates_guinea
df$date <- missing_dates_guinea

# Add df to progress and arrange in proper order
progress <- progress %>%
  rbind(df) %>%
  arrange(country, date)

# Clean up
rm(df, row, ind_gaps, missing_dates_guinea)

#-----------------------------------------------#
# Replace all NA's in total_vaccinations column #
#-----------------------------------------------#

# Count number of countries with all NA's in total_vaccinations column
sum(country_sums$total_vacs == -Inf)

# Recalculate ind_first now that rows have been added
ind_first <- sapply(countries, function(c) first(which(progress$country == c)))

# Count number of countries with NA in first row of total_vaccinations column
sum(is.na(progress$total_vaccinations[ind_first]))

# Find which countries have NA in first row of total_vaccinations column
ind <- ind_first[which(is.na(progress$total_vaccinations[ind_first]))]
ind

# Set first rows of total_vaccinations for Latvia and Guinea to
# people_vaccinated value
progress$total_vaccinations[ind] <- progress$people_vaccinated[ind]

# Set NA's in total_vaccinations column to sum of daily_vaccinations and
# previous value of total_vaccinations
# Use while loop to repeat until all NA's are replaced
while (sum(is.na(progress$total_vaccinations)) != 0) {
  progress <- progress %>%
    mutate(total_vaccinations = ifelse(is.na(total_vaccinations),
                                       daily_vaccinations +
                                         lag(total_vaccinations),
                                       total_vaccinations))
}

#----------------------------------------------------------------------------#
# Replace some NA's in people_vaccinated and people_fully_vaccinated columns #
#----------------------------------------------------------------------------#

# If people_fully_vaxed column in country_sums is -Inf, it is because all values
# of people_fully_vaccinated column for that country are NA
# Set NA's to 0 in people_fully_vaccinated column for these countries
countries_pfvna <- country_sums %>%
  filter(people_fully_vaxed == -Inf) %>%
  .$country
progress <- progress %>%
  mutate(people_fully_vaccinated = ifelse(country %in% countries_pfvna,
                                          0,
                                          people_fully_vaccinated))

# Examine countries using Johnson and Johnson vaccine
johnson_countries <- progress %>%
  select(country, vaccines) %>%
  unique() %>%
  filter(str_detect(vaccines, "Johnson"))
johnson_countries

# View data for johnson_countries
johnson_data <- progress[progress$country %in% johnson_countries$country,]
view(johnson_data)

# Calculate difference between total_vaccinations and sum of people_vaccinated
# and people_fully_vaccinated for last row of countries in johnson_data
# This should be the number of J&J vaccines used
johnson_data %>%
  group_by(country) %>%
  filter(date == last(date)) %>%
  summarize(diff = total_vaccinations -
              (people_vaccinated + people_fully_vaccinated))

# Find how many Johnson and Johnson vaccines used in johnson_countries
vacs_by_man %>%
  filter(location %in% johnson_countries$country) %>%
  filter(str_detect(vaccine, "Johnson")) %>%
  group_by(location) %>%
  summarize(j_vacs = last(total_vaccinations))

# Remove Poland from johnson_countries, it appears they have not used any J&J
# vaccines
johnson_countries <- johnson_countries %>%
  filter(country != "Poland")

# Extract country column from johnson_countries
johnson_countries <- johnson_countries$country

# If people_vaccinated is NA and people_fully_vaccinated is not NA,
# set people_vaccinated to total_vaccinations - people_fully_vaccinated
# Okay to include johnson_countries, affected rows are all before any J&J
# vaccines administered, no rows affected for South Africa
progress <- progress %>%
  mutate(people_vaccinated = ifelse(is.na(people_vaccinated) &
                                      !is.na(people_fully_vaccinated),
                                    total_vaccinations -
                                      people_fully_vaccinated,
                                    people_vaccinated))

# If people_vaccinated is not NA and people_fully_vaccinated is NA,
# set people_fully_vaccinated to total_vaccinations - people_vaccinated
# Okay to include johnson_countries, affected rows are all before any J&J
# vaccines administered, no rows affected for South Africa
# After this all NA's in columns 4 through 7 will be in pairs in columns 5 and 6
progress <- progress %>%
  mutate(people_fully_vaccinated = ifelse(!is.na(people_vaccinated) &
                                            is.na(people_fully_vaccinated) &
                                            !country %in% johnson_countries,
                                          total_vaccinations -
                                            people_vaccinated,
                                          people_fully_vaccinated))

# Clean up
rm(countries_pfvna)

#----------------------------------------------------------------------------#
# Adjust rows where a country's total_vaccinations is less than previous row #
#----------------------------------------------------------------------------#

# Create function to find indexes where a column's value is less than value in
# previous row for same country
find_problems <- function(v) {
  sapply(v, function(n) {
    
    # Find rows where column is less than previous value
    ind <- which(progress[,n] < lag(progress[,n]))
    
    # Exclude rows where country changes
    setdiff(ind, ind_first)
  })
}

# Find rows in total_vaccinations column with value less than previous row
ind <- find_problems(4)
length(ind)

# Create vector of indexes excluding countries' first rows
ind_not_first <- setdiff(1:nrow(progress), ind_first)

# If a row in a country's total_vaccinations column is less than the previous
# row's value, change it to the previous row's value
# Use for loop to continuously correct newly created problem rows
for (i in ind_not_first) {
  if (progress$total_vaccinations[i] < progress$total_vaccinations[i - 1]) {
    progress$total_vaccinations[i] <- progress$total_vaccinations[i - 1]
  }
}

# Verify no more problems in total_vaccinations column
find_problems(4)

# Adjust daily_vaccinations column to reflect changes just made
# Set daily_vaccinations for each row for each country to difference between
# total_vaccinations value of the row and the previous row
for (i in ind_not_first) {
  progress$daily_vaccinations[i] <- progress$total_vaccinations[i] -
    progress$total_vaccinations[i - 1]
}

# Make sure people_vaccinated = total_vaccinations - people_fully_vaccinated
# This will add any excess discrepancies to the people_vaccinated column
# Exclude johnson_countries
progress <- progress %>%
  mutate(people_vaccinated = ifelse(!country %in% johnson_countries,
                                    total_vaccinations -
                                      people_fully_vaccinated,
                                    people_vaccinated))

#----------------------------------------------#
# Replace all NA's in people_vaccinated column #
#----------------------------------------------#

# Count number of countries with NA's in first row for people_vaccinated and
# people_fully_vaccinated columns
countries_na_first <- progress[ind_first,] %>%
  filter(is.na(people_vaccinated) & is.na(people_fully_vaccinated)) %>%
  .$country
length(countries_na_first)

# For countries above, set first row of people_vaccinated column to value of
# total_vaccinations column
ind_na_first <- ind_first[countries_na_first]
for (i in ind_na_first) {
  progress$people_vaccinated[i] <- progress$total_vaccinations[i]
}

# For NA's in people_vaccinated column, add value in daily_vaccinations column
# to value of people_vaccinated column in previous row
# Some of these values may be people_fully_vaccinated, we will adjust for this
# later
ind_na <- which(is.na(progress$people_vaccinated))
for (i in ind_na) {
  progress$people_vaccinated[i] <- progress$people_vaccinated[i - 1] +
    progress$daily_vaccinations[i]
}

# Clean up
rm(countries_na_first, ind_na_first)

#-----------------------------------------------------------#
# Adjust rows where people_vaccinated is more than next row #
#-----------------------------------------------------------#

# Count number or rows in people_vaccinated column where value decreases #
find_problems(5) %>% length()

# If value of people_vaccinated column for a country is more than the value for
# the next row, set it equal to the value for the next row
# This will trim off excess values that we will move to the
# people_fully_vaccinated column
# Work backwards from the end of the data set to ensure no decreasing values
# remain for any country
# Exclude South Africa, we will deal with NA's there separately
for (i in rev(ind_not_first)) {
  if (progress$people_vaccinated[i - 1] > progress$people_vaccinated[i] &
      progress$country[i] != "South Africa") {
    progress$people_vaccinated[i - 1] <- progress$people_vaccinated[i]
  }
}

# Set people_vaccinated and people_fully_vaccinated to total_vaccinations for
# South Africa
progress <- progress %>%
  mutate(people_vaccinated = ifelse(country == "South Africa",
                                    total_vaccinations,
                                    people_vaccinated),
         people_fully_vaccinated = ifelse(country == "South Africa",
                                          total_vaccinations,
                                          people_fully_vaccinated))

# Verify no more problems in people_vaccinated column
find_problems(5)

#----------------------------------------------------#
# Replace all NA's in people_fully_vaccinated column #
#----------------------------------------------------#

# Set people_fully_vaccinated to total_vaccinations - people_vaccinated
# Exclude johnson_countries
progress <- progress %>%
  mutate(people_fully_vaccinated = ifelse(!country %in% johnson_countries,
                                          total_vaccinations -
                                            people_vaccinated,
                                          people_fully_vaccinated))

# Check to see when J&J vaccine is used in Czechia
vacs_by_man %>%
  filter(location == "Czechia" & str_detect(vaccine, "Johnson"))

# Make sure people_fully_vaccinated is not NA for Czechia on 2021-04-22
progress %>%
  filter(country == "Czechia" & date == "2021-04-22") %>%
  .$people_fully_vaccinated

# Set NA's in people_fully_vaccinated column for Czechia to difference between
# total_vaccinations and people_vaccinated
ind_na <- which(progress$country == "Czechia" &
                  is.na(progress$people_fully_vaccinated))
progress$people_fully_vaccinated[ind_na] <-
  progress$total_vaccinations[ind_na] - progress$people_vaccinated[ind_na]

# Check to see when J&J vaccine is used in United States
vacs_by_man %>%
  filter(location == "United States" & str_detect(vaccine, "Johnson"))

# Make sure people_fully_vaccinated is not NA for United States after 2021-03-07
progress %>%
  filter(country == "United States" & date > "2021-03-07") %>%
  .$people_fully_vaccinated %>%
  is.na() %>%
  sum()

# Set NA's in people_fully_vaccinated column for United States to difference
# between total_vaccinations and people_vaccinated
ind_na <- which(progress$country == "United States" &
                  is.na(progress$people_fully_vaccinated))
progress$people_fully_vaccinated[ind_na] <-
  progress$total_vaccinations[ind_na] - progress$people_vaccinated[ind_na]

# Make sure there are no more NA's in columns 4 through 7
sum(is.na(progress[,4:7]))

#----------------------------------------------------------------------#
# Adjust rows with decreasing sequential values in columns 4 through 6 #
#----------------------------------------------------------------------#

# Find rows with decreasing values for columns 4 through 6
ind <- find_problems(4:6)
sapply(ind, length)

# Extract indexes for column 6
ind <- ind[[3]]

# Examine above rows
problems <- progress[ind,]
view(problems)

# Count number of rows with negative values in people_fully_vaccinated column
ind <- which(progress$people_fully_vaccinated < 0)
length(ind)

# Add rows with negative values to problems
# Arrange problems to resort by country and date
problems <- union(problems, progress[ind,])
problems <- problems %>%
  arrange(country, date)

# For negative values in column 6, set to previous non-negative value and deduct
# last non-negative value from column 5 and add negative value of column 6 to
# column 5
# This will shuffle counts from column 5 over to column 6 ensuring column 6 does
# not have any negative or decreasing values in these rows
# Note none of these rows involve J&J vaccine, so no special consideration is
# necessary

# Find previous non-negative value in column 6
value <- progress[[ind[1] - 1, 6]]

# Shuffle counts from column 5 to column 6 to fix negative values
for (i in ind) {
  progress[i, 5] <- progress[i, 5] - value + progress[i, 6]
  progress[i, 6] <- value
}

# For remaining rows in problems, shuffle excess value from column 6 to column 5
# Find rows with decreasing values in column 6
ind <- find_problems(6)

# Use while loop to fix newly generated problems
# Class of ind will change from matrix/array to list when complete
while(class(ind) != "list") {
  
  # Work backwards through data set for efficiency
  for (i in rev(ind)) {
    
    # Calculate excess value for previous row of column 6
    excess <- progress[i - 1, 6] - progress[i, 6]
    
    # Shuffle excess value from column 6 to column 5
    progress[i - 1, 6] <- progress[i, 6]
    progress[i - 1, 5] <- progress[i - 1, 5] + excess
    
    # Find newly generated decreasing values in column 6
    ind <- find_problems(6)
  }
}

#--------------------------------#
# Verify cleaning process so far #
#--------------------------------#

# Ensure no more decreasing sequential values in columns 4 through 6
find_problems(4:6) %>% sapply(length)

# Make sure there are no negative values
sum(progress < 0, na.rm = TRUE)

# Make sure all total_vaccinations = people_vaccinated + people_fully_vaccinated
# Exclude johnson_countries
progress %>%
  filter(!country %in% johnson_countries) %>%
  summarize(mean(total_vaccinations == people_vaccinated +
                   people_fully_vaccinated))

# Make sure all total_vaccinations = previous value + daily_vaccinations for
# each country
# Find rows where above is true
ind <- which(progress[,4] == lag(progress[,4]) + progress[,7])

# Make sure all rows not a country's first row are in above rows
mean(ind_not_first %in% ind)

# Summarize progress data by country again
country_sums <- progress %>%
  group_by(country) %>%
  summarize(total_vacs = last(total_vaccinations),
            people_vaxed = last(people_vaccinated),
            people_fully_vaxed = last(people_fully_vaccinated),
            sum = sum(daily_vaccinations))

# View country sums
view(country_sums)

# Check to make sure no total_vacs < sum
sum(country_sums$total_vacs < country_sums$sum)

# Clean up
rm(excess, johnson_data, problems, i, ind_first, ind_na, ind_not_first,
   johnson_countries, value, find_problems)

#---------------------------------------------#
# Create rows for all dates for all countries #
#---------------------------------------------#

# Create column with first date with data for each country
progress <- progress %>%
  group_by(country) %>%
  mutate(first_date = first(date))

# Find date range of progress
date_range <- range(progress$date)

# Create vector of all dates in date_range
dates <- as.Date(date_range[1]:date_range[2], origin = "1970-01-01")

# Create data frame with all dates for all countries
df <- expand.grid(countries, dates) %>%
  `names<-`(c("country", "date")) %>%
  arrange(country, date)

# Create data frame with all static columns of progress
static <- progress %>%
  select(1:2, 12:15) %>%
  unique()

# Join progress with df
progress <- df %>%
  left_join(static, by = "country") %>%
  left_join(progress[c(1, 3:11)], by = c("country", "date")) %>%
  select(1, 3, 2, 7, 8:15, 4:6)

# Set all NA's in daily_vaccinations column to 0
progress <- progress %>%
  mutate(daily_vaccinations = replace_na(daily_vaccinations, 0))

# Set all NA's in columns 5 through 7 to 0 if before first_date
progress <- progress %>%
  mutate(total_vaccinations = ifelse(date < first_date,
                                     0,
                                     total_vaccinations),
         people_vaccinated = ifelse(date < first_date,
                                    0,
                                    people_vaccinated),
         people_fully_vaccinated = ifelse(date < first_date,
                                          0,
                                          people_fully_vaccinated))

# Set all remaining NA's in columns 5 through 7 to previous non-NA value
ind <- which(is.na(progress$total_vaccinations))
for (i in ind) {
  progress[ind, 5] <- progress[ind - 1, 5]
  progress[ind, 6] <- progress[ind - 1, 6]
  progress[ind, 7] <- progress[ind - 1, 7]
}

# Remove first_date column
progress <- progress %>%
  select(-first_date)

# Clean up
rm(df, static, date_range, dates)

#-------------------#
# Clean populations #
#-------------------#

# Alphabetize populations by country
populations <- populations %>%
  arrange(`Country (or dependency)`)

# Select desired columns from populations
populations <- populations %>%
  select(1, 2, 5, 9, 10)

# Rename populations columns
names <- c("country", "population", "pop_density",
           "median_age", "urban_percent")
names(populations) <- names

# Remove percent sign from urban_percent column of populations
# COnvert median_age and urban_percent to numeric
populations <- populations %>%
  mutate(urban_percent = str_remove(urban_percent, "%")) %>%
  mutate(median_age = as.numeric(median_age),
         urban_percent = as.numeric(urban_percent))

# Check to see if all countries in progress
# are listed with same name in populations
countries_pop <- populations$country
sum(countries %in% countries_pop)

# Check which countries have different names or are not listed in populations
ind <- which(!countries %in% countries_pop)
countries[ind]

# Change country names in populations to match names in progress
ind_pop <- c(34, 50, 53, 55, 176, 182, 210, 218)
populations$country[ind_pop] <-
  countries[ind[c(1:4, 12, 14, 16, 17)]]

# Display which countries are not listed in populations
countries_pop <- populations$country
ind <- which(!(countries %in% countries_pop))
countries[ind]

# Make vectors with population info for missing countries
# This data was gathered by web search, mostly wikipedia
pops <- c(56290000, 63155, 107800, 1873160, 372486, 1890000,
          5101414, 11940, 5460000, 3150000)
pop_dens <- c(432, 965, 912, 159, 106, 133, 847, 284, 65, 152)
med_age <- c(40, 44.3, 37.5, 29.1, 37.9, 38.9, 20.8, 35.3, 42, 42.5)
urban_pop <- c(79.3, 31, 31, 54.7, 66.8, 65, 76.7, 52.9, 83, 64.9)

# Make data frame with vectors for missing countries
df <- data.frame(country = countries[ind],
                 population = pops,
                 pop_density = pop_dens,
                 median_age = med_age,
                 urban_percent = urban_pop)

# Add df rows to populations
populations <- rbind(populations, df)

# Reorder populations by country
populations <- populations %>%
  arrange(country)

#--------------------------------#
# Join progress with populations #
#--------------------------------#

# Join populations with progress
progress <- progress %>%
  left_join(populations, by = "country")

# Clean up
rm(df, countries_pop, ind_pop, med_age, names, pop_dens, pops,
   urban_pop)

#----------------------------------------#
# Calculate remaining values in progress #
#----------------------------------------#

# Calculate all values for columns 8 through 11
progress <- progress %>%
  mutate(total_vaccinations_per_hundred =
           round(100 * total_vaccinations / population, 2),
         people_vaccinated_per_hundred =
           round(100 * people_vaccinated / population, 2),
         people_fully_vaccinated_per_hundred =
           round(100 * people_fully_vaccinated / population, 2),
         daily_vaccinations_per_million =
           round(10^6 * daily_vaccinations / population, 2))

#--------------------------------------------------------------------#
# Create logical columns for each vaccine being used in each country #
#--------------------------------------------------------------------#

# Make sure each country has the same entry for every row in vaccines column
progress %>%
  group_by(country) %>%
  summarize(n = n_distinct(vaccines)) %>%
  summarize(sum(n != 1))

# Group by country and extract vaccines from vaccines column
country_vacs <- progress %>%
  group_by(country) %>%
  summarize(vaccines = first(vaccines)) %>%
  mutate(oxford = ifelse(str_detect(vaccines, "Oxford/AstraZeneca"),
                         TRUE,
                         FALSE),
         pfizer = ifelse(str_detect(vaccines, "Pfizer/BioNTech"),
                         TRUE,
                         FALSE),
         sputnik = ifelse(str_detect(vaccines, "Sputnik V"),
                          TRUE,
                          FALSE),
         beijing = ifelse(str_detect(vaccines, "Sinopharm/Beijing"),
                          TRUE,
                          FALSE),
         wuhan = ifelse(str_detect(vaccines, "Sinopharm/Wuhan"),
                        TRUE,
                        FALSE),
         moderna = ifelse(str_detect(vaccines, "Moderna"),
                          TRUE,
                          FALSE),
         sinovac = ifelse(str_detect(vaccines, "Sinovac"),
                          TRUE,
                          FALSE),
         covaxin = ifelse(str_detect(vaccines, "Covaxin"),
                          TRUE,
                          FALSE),
         epivac = ifelse(str_detect(vaccines, "EpiVacCorona"),
                         TRUE,
                         FALSE),
         johnson = ifelse(str_detect(vaccines, "Johnson&Johnson"),
                          TRUE,
                          FALSE)) %>%
  select(-vaccines)

# Join progress with country_vacs
progress <- progress %>%
  left_join(country_vacs, by = "country")

# Update country_sums with new data
country_sums <- progress %>%
  group_by(country) %>%
  summarize(iso_code = first(iso_code),
            total_vacs = last(total_vaccinations),
            people_vaxed = last(people_vaccinated),
            people_fully_vaxed = last(people_fully_vaccinated),
            total_vacs_per_hundred = last(total_vaccinations_per_hundred),
            people_vaxed_per_hundred = last(people_vaccinated_per_hundred),
            people_fully_vaxed_per_hundred =
              last(people_fully_vaccinated_per_hundred)) %>%
  left_join(populations, by = "country") %>%
  left_join(country_vacs, by = "country")

# Clean up
rm(country_vacs)

#--------------------------------------------------#
# Add continent and sub_region columns to progress #
#--------------------------------------------------#

# Select desired columns from continents
continents <- continents %>%
  select(country, continent, sub_region)

# Check to see if all countries in progress
# are listed with same name in continents
countries_cont <- continents$country
sum(countries %in% countries_cont)

# Check which countries have different names or are not listed in populations
ind <- which(!countries %in% countries_cont)
countries[ind]

# Change country names in continents to match names in progress
ind_cont <- c(27, 34, 41, 55, 58, 60, 214, 73, 72, 105, 122, 146, 132, 170, 183,
              186, 119, 217, 218, 222, 235, 236, 241, 242)
continents$country[ind_cont] <-
  countries[ind[c(1:6, 8:11, 13:15, 18:20, 22:29)]]

# Display which countries are not listed in continents
countries_cont <- continents$country
ind <- which(!(countries %in% countries_cont))
countries[ind]

# Make vectors with continent info for missing countries
cont <- c(rep("Europe", 6))
sub_region <- c("Nothern Europe", rep("Southern Europe", 2),
                rep("Northern Europe", 3))

# Make data frame with vectors for missing countries
df <- data.frame(country = countries[ind],
                 continent = cont,
                 sub_region = sub_region)

# Add df rows to continents
continents <- rbind(continents, df)

# Reorder continents by country
continents <- continents %>%
  arrange(country)

# Join progress with continents
# Reorder columns so continent and sub_region are next to country
progress <- progress %>%
  left_join(continents, by = "country") %>%
  select(1, 29, 30, 2:28)

# Join country_sums with continents
# Reorder columns so continent and sub_region are next to country
country_sums <- country_sums %>%
  left_join(continents, by = "country") %>%
  select(1:2, 23, 24, 3:22)

# Clean up
rm(df, cont, countries_cont, ind_cont, sub_region)

#--------------------------#
# Save cleaned rda objects #
#--------------------------#

# Create rdas directory if doesn't exist and save progress
wd <- getwd()
ifelse(!dir.exists(file.path(wd, "rdas")),
       dir.create(file.path(wd, "rdas")),
       FALSE)
save(progress, file = "rdas/vaccine_progress_cleaned.rda")

# Save country_sums
save(country_sums, file = "rdas/vaccine_progress_country_totals.rda")

# Save populations
save(populations, file = "rdas/country_populations.rda")

#==============#
# Clean tweets #
#==============#

# View tweets
view(tweets)

# Table is_retweet column
table(tweets$is_retweet)

# Remove is_retweet column
tweets <- tweets %>%
  select(-is_retweet)

# Convert date column to "Date" class object
tweets <- tweets %>%
  mutate(date = as.Date(date))

#------------------------------------------#
# Extract country names from user_location #
#------------------------------------------#

# Create countries vector from populations$country
countries <- populations$country

# Inspect occurrences of "Jersey" in user_locations
# Create function for repeated use
view_location <- function(x) {
  tweets %>%
    filter(str_detect(user_location, x)) %>%
    .$user_location %>%
    view()
}
view_location("Jersey")

# Remove "Jersey" from countries
# Almost all occurrences of Jersey are New Jersey
countries <- countries[-which(countries == "Jersey")]

# Inspect occurrences of "Georgia" in user_locations
view_location("Georgia")

# Remove "Georgia" from countries
# Almost all occurrences of Georgia are from the US
countries <- countries[-which(countries == "Georgia")]

# Create function to count occurrences of location in user_locations
count_detect <- function(x) {
  sum(str_detect(tweets$user_location, x), na.rm = TRUE)
}

# Count occurrences of "Samoa" and "American Samoa" in user_locations
count_detect("Samoa")
count_detect("American Samoa")

# Count occurrences of "Netherlands" and "Caribbean Netherlands" in
# user_locations
count_detect("Netherlands")
count_detect("Caribbean Netherlands")

# Count occurrences of "Cyprus" and "Northern Cyprus" in user_locations
count_detect("Cyprus")
count_detect("Northern Cyprus")

# Count occurrences of "Dominica" and "Dominican Republic" in user_locations
count_detect("Dominica")
count_detect("Dominican Republic")

# Count occurrences of "England" and "New England" in user_locations
count_detect("England")
count_detect("New England")

# Count occurrences of "Guinea", "Papua New Guinea", and "Guinea-Bissau in 
# user_locations
count_detect("Guinea")
count_detect("Papua New Guinea")
count_detect("Guinea-Bissau")

# Count occurrences of "Ireland" and "Northern Ireland" in user_locations
count_detect("Ireland")
count_detect("Northern Ireland")

# Count occurrences of "Mexico" and "New Mexico" in user_locations
count_detect("Mexico")
count_detect("New Mexico")

# Count occurrences of "Sudan" and "South Sudan" in user_locations
count_detect("Sudan")
count_detect("South Sudan")

# Count occurrences of "Wales" and " New South Wales" in user_locations
count_detect("Wales")
count_detect("New South Wales")

# Look for country names in user_location column of tweets, add country column
# Create function to extract country names from string
extract_countries <- function(x) {
  list <- str_extract(x, countries)
  list <- na.exclude(list)
  paste(list, collapse = ",")
}

# Extract country names from user_location column of tweets
# Note this will take a minute
country <- unname(sapply(tweets$user_location, extract_countries))

# Inspect country elements that contain "Northern Ireland"
ind <- which(str_detect(country, "Northern Ireland"))
view_location("Northern Ireland")
country[ind]

# Remove "Ireland," from above elements
country[ind] <- str_remove(country[ind], "Ireland,")

# Inspect country elements where user_location contains "New Mexico"
ind <- which(str_detect(tweets$user_location, "New Mexico"))
view_location("New Mexico")
country[ind]

# Set above elements to "United States"
country[ind] <- "United States"

# Inspect country elements where user_location contains "New England"
ind <- which(str_detect(tweets$user_location, "New England"))
view_location("New England")
country[ind]

# Set above elements to "United States"
country[ind] <- "United States"

# Inspect country elements where user_location contains "New South Wales"
ind <- which(str_detect(tweets$user_location, "New South Wales"))
view_location("New South Wales")
country[ind]

# Set above elements to "Australia"
country[ind] <- "Australia"

# Add country column to tweets
tweets <- tweets %>%
  mutate(country = country)

# Reorder country column so it is next to user_location
tweets <- tweets[,c(1:3, 16, 4:15)]

# Clean up
rm(countries, country, extract_countries)

#-----------------------------------------#
# Find indicators of USA in user_location #
#-----------------------------------------#

# Look for user_location rows with US state abbreviations, US state names, US,
# or USA
# Inspect rows with "US " in user_location to make sure there aren't a lot of
# locations such as "US Virgin Islands" and the like
view_location("US ")

# Inspect rows with "America" in user_location to see if there are locations
# such as "South America", "North America", etc
view_location("America")

# Inspect rows with state abbreviations that might show up in other place names
view_location("IN")
view_location("AL")
view_location("CA")
view_location("ME")

# Create function to detect country indicators in a string
detect_country <- function(x) {
  ifelse(sum(str_detect(x, indicators), na.rm = TRUE) > 0,
         TRUE,
         FALSE)
}

# Create vector with biggest cities in US
cities <- c("Los Angeles", "San Francisco", "Seattle", "Houston", "Dallas",
            "Chicago", "Phoenix", "Philadelphia", "San Antonio", "San Diego",
            "San Jose", "Denver", "Boston", "Las Vegas", "Detroit", "Portland",
            "Cleveland", "Atlanta", "Austin", "DC", "Charleston", "New Orleans",
            "NY", "Jacksonville", "Miami", "Brooklyn")

# Create vector of state abbreviations that won't create too many mistakes
abbs <- paste0(" ", state.abb, "$")

# Set indicators for USA
indicators <- c(cities, state.name, abbs, "US", "in America", "of America",
                "^America$", "^CA$", "SoCal")

# Create logical vector for USA indicator found in tweets$user_location
# Note this will take a minute
usa <- unname(sapply(tweets$user_location, detect_country))

# Set or add "United States" to rows where usa = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(usa == TRUE & country == "" ~ "United States",
                             usa == TRUE & country != "" &
                               !str_detect(country, "United States") ~
                               paste(country, "United States", sep = ","),
                             TRUE ~ country))

# Clean up
rm(abbs, cities, usa)

#---------------------------------------------#
# Find indicators of England in user_location #
#---------------------------------------------#

# View user_locations that contain biggest cities in England
view_location("London")
view_location("Liverpool")
view_location("Sheffield")
view_location("Leeds")
view_location("Manchester")
view_location("Bristol")
view_location("Coventry")
view_location("Leicester")
view_location("Oxford")

# Set indicators for England
indicators <- c("(?<!New )London", "Liverpool", "Sheffield", "Leeds",
             "Manchester(?!, N|, M)", "Bristol(?!,C)", "Coventry(?! R)",
             "Leicester", "Oxford(?!, O)", "Nottingham", "Essex", "Yorkshire",
             "Chesterfield", "Watford", "Poole", "london")

# Create logical vector for England indicator found in tweets$user_location
eng <- unname(sapply(tweets$user_location, detect_country))

# Set or add "England" to rows where eng = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(eng == TRUE & country == "" ~ "England",
                             eng == TRUE & country != ""  &
                               !str_detect(country, "England") ~
                               paste(country, "England", sep = ","),
                             TRUE ~ country))

# Clean up
rm(eng)

#-------------------------------------------#
# Find indicators of Wales in user_location #
#-------------------------------------------#

# View user_locations that contain biggest cities in Wales
view_location("Cardiff")
view_location("Swansea")
view_location("Newport")

# Set indicators for Wales
indicators <- c("Cardiff", "Swansea(?!, I)", "d Newport")

# Create logical vector for Wales indicator found in tweets$user_location
wales <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Wales" to rows where wales = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(wales == TRUE & country == "" ~ "Wales",
                             wales == TRUE & country != ""  &
                               !str_detect(country, "Wales") ~
                               paste(country, "Wales", sep = ","),
                             TRUE ~ country))

# Clean up
rm(wales)

#----------------------------------------------#
# Find indicators of Scotland in user_location #
#----------------------------------------------#

# View user_locations that contain biggest cities in Scotland
view_location("Edinburgh")
view_location("Glasgow")
view_location("Aberdeen")
view_location("Dundee")

# Set indicators for Scotland
indicators <- c("Edinburgh", "Glasgow", "Dundee")

# Create logical vector for Scotland indicator found in tweets$user_location
scot <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Scotland" to rows where scot = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(scot == TRUE & country == "" ~ "Scotland",
                             scot == TRUE & country != ""  &
                               !str_detect(country, "Scotland") ~
                               paste(country, "Scotland", sep = ","),
                             TRUE ~ country))

# Clean up
rm(scot)

#------------------------------------------------------#
# Find indicators of Northern Ireland in user_location #
#------------------------------------------------------#

# View user_locations that contain biggest cities in Northern Ireland
view_location("Belfast")
view_location("Derry")
view_location("Newtownabbey")

# Set indicators for Northern Ireland
indicators <- c("Belfast", "Derry(?!, NH)", "Newtownabbey")

# Create logical vector for Northern Ireland indicator found in
# tweets$user_location
n_ire <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Northern Ireland" to rows where n_ire = TRUE depending on current
# value
tweets <- tweets %>%
  mutate(country = case_when(n_ire == TRUE & country == "" ~ "Northern Ireland",
                             n_ire == TRUE & country != ""  &
                               !str_detect(country, "Northern Ireland") ~
                               paste(country, "Northern Ireland", sep = ","),
                             TRUE ~ country))

# Clean up
rm(n_ire)

#----------------------------------------------------#
# Find indicators of United Kingdom in user_location #
#----------------------------------------------------#

# View rows with UK indicators in user_location
view_location("UK")
view_location("GB")
view_location("Britain")

# Set indicators for UK for user_location column
indicators <- c("UK", "^Uk$", "^uk$", "\\.uk", "GB", "Britain(?!, C)")

# Create logical vector for UK indicator found in tweets$user_location
uk_1 <- unname(sapply(tweets$user_location, detect_country))

# Set indicators for UK for country column
indicators <- c("England", "Scotland", "Wales", "Northern Ireland")

# Create logical vector for UK indicator found in tweets$country
uk_2 <- unname(sapply(tweets$country, detect_country))

# Combine uk_1 and uk_2, set TRUE if either is TRUE
uk <- ifelse(uk_1 == TRUE | uk_2 == TRUE,
             TRUE,
             FALSE)

# Set or add "United Kingdom" to rows where uk = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(uk == TRUE & country == "" ~ "United Kingdom",
                             uk == TRUE & country != ""  &
                               !str_detect(country, "United Kingdom") ~
                               paste(country, "United Kingdom", sep = ","),
                             TRUE ~ country))

# Clean up
rm(uk, uk_1, uk_2)

#---------------------------------------------#
# Check progress on filling in country column #
#---------------------------------------------#

# Calculate proportion of user_location column that is NA
mean(is.na(tweets$user_location))

# Calculate proportion of country column that is ""
mean(tweets$country == "")

# Examine 20 most prevalent user_location entries where country == ""
tweets %>%
  filter(country == "") %>%
  group_by(user_location) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(20)

#-------------------------------------------#
# Find indicators of India in user_location #
#-------------------------------------------#

# View user_locations that contain biggest cities in India
view_location("Delhi")
view_location("Mumbai")
view_location("Chennai")
view_location("Bangalore")
view_location("Hyderabad")
view_location("Kolkata")
view_location("Jaipur")
view_location("Ahmedabad")
view_location("Patna")
view_location("Surat")

# Set indicators for India
indicators <- c("Delhi", "Mumbai", "Chennai", "Bangalore", "Hyderabad",
                "Kolkata", "Jaipur", "Ahmedabad", "Patna", "Surat", "INDIA",
                "india", "patna", "Jammu", "Kashmir", "Bengaluru", "Bharat",
                "Rajasthan", "Bhubaneswar", "Pune", "Bombay", "Kerala", "delhi",
                "Nagar", "Kharar", "Mohali", "Noida", "Chandigarh", "Gurgaon",
                "BENGALURU", "Gujarat", "mumbai", "Assam")

# Create logical vector for India indicator found in tweets$user_location
india <- unname(sapply(tweets$user_location, detect_country))

# Set or add "India" to rows where india = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(india == TRUE & country == "" ~ "India",
                             india == TRUE & country != ""  &
                               !str_detect(country, "India") ~
                               paste(country, "India", sep = ","),
                             TRUE ~ country))

# Clean up
rm(india)

#-------------------------------------------#
# Find indicators of China in user_location #
#-------------------------------------------#

# View user_locations that contain biggest cities in China
view_location("Beijing")
view_location("Shanghai")
view_location("Shenzhen")

# Set indicators for China, include Hong Kong
indicators <- c("Beijing", "Shanghai", "Shenzhen", "Hong\\s*(K|k)ong", "CHINA")

# Create logical vector for China indicator found in tweets$user_location
china <- unname(sapply(tweets$user_location, detect_country))

# Set or add "China" to rows where china = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(china == TRUE & country == "" ~ "China",
                             china == TRUE & country != ""  &
                               !str_detect(country, "China") ~
                               paste(country, "China", sep = ","),
                             TRUE ~ country))

# Clean up
rm(china)

#--------------------------------------------#
# Find indicators of Turkey in user_location #
#--------------------------------------------#

# View user_locations that contain biggest cities in Turkey
view_location("Istanbul")
view_location("Ankara")

# Set indicators for Turkey, include Türkiye
indicators <- c("Istanbul", "Ankara", "T(ü|u)rkiye")

# Create logical vector for Turkey indicator found in tweets$user_location
turk <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Turkey" to rows where turk = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(turk == TRUE & country == "" ~ "Turkey",
                             turk == TRUE & country != ""  &
                               !str_detect(country, "Turkey") ~
                               paste(country, "Turkey", sep = ","),
                             TRUE ~ country))

# Clean up
rm(turk)

#--------------------------------------------#
# Find indicators of Brazil in user_location #
#--------------------------------------------#

# View user_locations that contain Sao Paulo
view_location("S(ã|a)o Paulo")

# Set indicators for Brazil, include "Brasil"
indicators <- c("S(ã|a)o Paulo", "Brasil")

# Create logical vector for Brazil indicator found in tweets$user_location
braz <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Brazil" to rows where braz = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(braz == TRUE & country == "" ~ "Brazil",
                             braz == TRUE & country != ""  &
                               !str_detect(country, "Brazil") ~
                               paste(country, "Brazil", sep = ","),
                             TRUE ~ country))

# Clean up
rm(braz)

#--------------------------------------------#
# Find indicators of Canada in user_location #
#--------------------------------------------#

# View user_locations that contain biggest cities in Canada
view_location("Montreal")
view_location("Vancouver")
view_location("Toronto")
view_location("Calgary")
view_location("Edmonton")
view_location("Ottawa")

# Create vector of Canadian cities
cities <- c("Montreal", "Vancouver", "Toronto", "Calgary", "Edmonton", "Ottawa")

# Create vector of Canadian provinces
provinces <- c("British Columbia", "Alberta", "Ontario", "Qu(e|é)bec", "Yukon",
               "Saskatchewan", "New Brunswick", "Nova Scotia", "Labrador",
               "Newfoundland", "Prince Edward Island", "Nunavut", "Manitoba",
               "La Belle Province")

# Inspect rows with abbreviations that might show up in other place names
view_location("AB")
view_location("ON")
view_location("NS")
view_location("PE")
view_location("BC")
view_location("NL")
view_location("NB")

# Create vector of province abbreviations that won't create too many mistakes
abbs <- c("BC$", "AB$", "(?<!D|I|O|T)ON$", "QC$", "YT$", "NB$", "MB$")

# Set indicators for Canada
indicators <- c(cities, provinces, abbs, "CANADA")

# Create logical vector for Canada indicator found in tweets$user_location
can <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Canada" to rows where can = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(can == TRUE & country == "" ~ "Canada",
                             can == TRUE & country != ""  &
                               !str_detect(country, "Canada") ~
                               paste(country, "Canada", sep = ","),
                             TRUE ~ country))

# Clean up
rm(abbs, can, cities, provinces)

#---------------------------------------------#
# Find indicators of Belgium in user_location #
#---------------------------------------------#

# View user_locations that contain biggest cities in Belgium
view_location("Brussels")
view_location("Antwerp")
view_location("Ghent")
view_location("Li(è|e)ge")
view_location("Bruges")

# Set indicators for Belgium
indicators <- c("Brussels", "Antwerp", "Ghent", "Li(è|e)ge", "Bruges", "Knokke",
                "Belgi")

# Create logical vector for Belgium indicator found in tweets$user_location
belg<- unname(sapply(tweets$user_location, detect_country))

# Set or add "Belgium" to rows where belg = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(belg == TRUE & country == "" ~ "Belgium",
                             belg == TRUE & country != ""  &
                               !str_detect(country, "Belgium") ~
                               paste(country, "Belgium", sep = ","),
                             TRUE ~ country))

# Clean up
rm(belg)

#-----------------------------------------#
# Find indicators of UAE in user_location #
#-----------------------------------------#

# View user_locations that contain biggest cities in UAE
view_location("Dubai")
view_location("Abu Dhabi")
view_location("Sharjah")

# Set indicators for UAE
indicators <- c("Dubai", "Abu Dhabi", "Sharjah", "UAE")

# Create logical vector for UAE indicator found in tweets$user_location
uae <- unname(sapply(tweets$user_location, detect_country))

# Set or add "United Arab Emirates" to rows where uae = TRUE depending on
# current value
tweets <- tweets %>%
  mutate(country = case_when(uae == TRUE & country == "" ~
                               "United Arab Emirates",
                             uae == TRUE & country != ""  &
                               !str_detect(country, "United Arab Emirates") ~
                               paste(country, "United Arab Emirates",
                                     sep = ","),
                             TRUE ~ country))

# Clean up
rm(uae)

#-------------------------------------------#
# Find indicators of Italy in user_location #
#-------------------------------------------#

# View user_locations that contain biggest cities in Italy
view_location("Rome")
view_location("Venice")
view_location("Milan")
view_location("Veneto")

# Set indicators for Italy
indicators <- c("Rome", "Milan", "Veneto", "Italia", "L'Aquila")

# Create logical vector for Italy indicator found in tweets$user_location
italy <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Italy" to rows where italy = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(italy == TRUE & country == "" ~ "Italy",
                             italy == TRUE & country != ""  &
                               !str_detect(country, "Italy") ~
                               paste(country, "Italy", sep = ","),
                             TRUE ~ country))

# Clean up
rm(italy)

#------------------------------------------#
# Check progress with country column again #
#------------------------------------------#

# Calculate proportion of country column that is ""
mean(tweets$country == "")

# Create vector of tweets$user_location rows with empty country column where
# user_location is not NA
empty <- tweets %>%
  filter(!is.na(user_location) & country == "") %>%
  .$user_location

# Extract all words from all elements of empty and show most prevalent ones
words <- str_c(empty, collapse = " ")
words <- str_extract_all(words, "[A-z]+")[[1]]
words <- data.frame(word = words)
top_words <- words %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
view(top_words)

# Clean up
rm(words, empty)

#---------------------------------------------#
# Find indicators of Germany in user_location #
#---------------------------------------------#

# View user_locations that contain biggest cities in Germany
view_location("Berlin|BERLIN")
view_location("Hamburg")
view_location("Munich")

# Set indicators for Germany
indicators <- c("Berlin", "BERLIN", "Hamburg", "Munich(?!, D)", "Germany",
                "GERMANY", "Deutschland")

# Create logical vector for Germany indicator found in tweets$user_location
germ <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Germany" to rows where germ = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(germ == TRUE & country == "" ~ "Germany",
                             germ == TRUE & country != ""  &
                               !str_detect(country, "Germany") ~
                               paste(country, "Germany", sep = ","),
                             TRUE ~ country))

# Clean up
rm(germ)

#---------------------------------------------#
# Find indicators of Ireland in user_location #
#---------------------------------------------#

# View user_locations that contain biggest cities in Ireland
view_location("Dublin")
view_location("Cork")

# Set indicators for Ireland
indicators <- c("Dublin", "Cork", "IRELAND")

# Create logical vector for Ireland indicator found in tweets$user_location
ire <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Ireland" to rows where ire = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(ire == TRUE & country == "" ~ "Ireland",
                             ire == TRUE & country != ""  &
                               !str_detect(country, "Ireland") ~
                               paste(country, "Ireland", sep = ","),
                             TRUE ~ country))

# Clean up
rm(ire)

#-------------------------------------------------#
# Find indicators of Netherlands in user_location #
#-------------------------------------------------#

# View user_locations that contain biggest cities in Netherlands
view_location("Amsterdam")
view_location("Rotterdam")
view_location("The Hague")

# Set indicators for Netherlands
indicators <- c("Amsterdam", "Rotterdam", "The Hague", "Nederland")

# Create logical vector for Netherlands indicator found in tweets$user_location
neth <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Netherlands" to rows where neth = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(neth == TRUE & country == "" ~ "Netherlands",
                             neth == TRUE & country != ""  &
                               !str_detect(country, "Netherlands") ~
                               paste(country, "Netherlands", sep = ","),
                             TRUE ~ country))

# Clean up
rm(neth)

#-------------------------------------------#
# Find indicators of Kenya in user_location #
#-------------------------------------------#

# View user_locations that contain biggest cities in Kenya
view_location("Nairobi")
view_location("Mombasa")
view_location("Nakuru")

# Set indicators for Kenya
indicators <- c("Nairobi", "Mombasa", "Nakuru")

# Create logical vector for Kenya indicator found in tweets$user_location
kenya <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Kenya" to rows where kenya = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(kenya == TRUE & country == "" ~ "Kenya",
                             kenya == TRUE & country != ""  &
                               !str_detect(country, "Kenya") ~
                               paste(country, "Kenya", sep = ","),
                             TRUE ~ country))

# Clean up
rm(kenya)

#---------------------------------------------#
# Find indicators of Czechia in user_location #
#---------------------------------------------#

# View user_locations that contain biggest cities in Czechia
view_location("Prague")
view_location("Brno")

# Set indicators for Czechia
indicators <- c("Prague", "Brno", "Czech")

# Create logical vector for Czechia indicator found in tweets$user_location
czech <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Czechia" to rows where czech = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(czech == TRUE & country == "" ~ "Czechia",
                             czech == TRUE & country != ""  &
                               !str_detect(country, "Czechia") ~
                               paste(country, "Czechia", sep = ","),
                             TRUE ~ country))

# Clean up
rm(czech)

#----------------------------------------------#
# Find indicators of Pakistan in user_location #
#----------------------------------------------#

# View user_locations that contain biggest cities in Pakistan
view_location("Karachi")
view_location("Islamabad")
view_location("Lahore")
view_location("Faisalabad")
view_location("Peshawar")

# Set indicators for Pakistan
indicators <- c("Karachi", "Islamabad", "Lahore", "Faisalabad", "Peshawar",
                "Clifton")

# Create logical vector for Pakistan indicator found in tweets$user_location
pak <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Pakistan" to rows where pak = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(pak == TRUE & country == "" ~ "Pakistan",
                             pak == TRUE & country != ""  &
                               !str_detect(country, "Pakistan") ~
                               paste(country, "Pakistan", sep = ","),
                             TRUE ~ country))

# Clean up
rm(pak)

#--------------------------------------------#
# Find indicators of Russia in user_location #
#--------------------------------------------#

# View user_locations that contain biggest cities in Russia
view_location("Moscow")
view_location("Saint Petersburg")

# Set indicators for Russia
indicators <- c("Moscow", "Moskau", "Saint Petersburg")

# Create logical vector for Russia indicator found in tweets$user_location
rus <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Russia" to rows where rus = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(rus == TRUE & country == "" ~ "Russia",
                             rus == TRUE & country != ""  &
                               !str_detect(country, "Russia") ~
                               paste(country, "Russia", sep = ","),
                             TRUE ~ country))

# Clean up
rm(rus)

#--------------------------------------------#
# Find indicators of France in user_location #
#--------------------------------------------#

# View user_locations that contain biggest cities in France
view_location("Paris")
view_location("Lyon")
view_location("Marseille")
view_location("Nice")

# Set indicators for France
indicators <- c("Paris", "Lyon", "Marseille", "Nice")

# Create logical vector for France indicator found in tweets$user_location
franc <- unname(sapply(tweets$user_location, detect_country))

# Set or add "France" to rows where franc = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(franc == TRUE & country == "" ~ "France",
                             franc == TRUE & country != ""  &
                               !str_detect(country, "France") ~
                               paste(country, "France", sep = ","),
                             TRUE ~ country))

# Clean up
rm(franc)

#-----------------------------------------------#
# Find indicators of Australia in user_location #
#-----------------------------------------------#

# View user_locations that contain biggest cities in Australia
view_location("Sydney")
view_location("Melbourne")
view_location("Brisbane")
view_location("Perth")

# Set indicators for Australia
indicators <- c("Sydney", "Melbourne, Victoria", "^Melbourne$", "AUSTRALIA",
                "Brisbane", "Perth, W", "Perthshire")

# Create logical vector for Australia indicator found in tweets$user_location
aus <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Australia" to rows where aus = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(aus == TRUE & country == "" ~ "Australia",
                             aus == TRUE & country != ""  &
                               !str_detect(country, "Australia") ~
                               paste(country, "Australia", sep = ","),
                             TRUE ~ country))

# Clean up
rm(aus)

#-------------------------------------------------#
# Find indicators of Philippines in user_location #
#-------------------------------------------------#

# View user_locations that contain biggest cities in Philippines
view_location("Manila")
view_location("Quezon")
view_location("Makati")
view_location("Davao")
view_location("Visayas")

# Set indicators for Philippines
indicators <- c("Manila", "Quezon", "Makati", "Davao", "Visayas")

# Create logical vector for Philippines indicator found in tweets$user_location
phil <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Philippines" to rows where phil = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(phil == TRUE & country == "" ~ "Philippines",
                             phil == TRUE & country != ""  &
                               !str_detect(country, "Philippines") ~
                               paste(country, "Philippines", sep = ","),
                             TRUE ~ country))

# Clean up
rm(phil)

#---------------------------------------------#
# Find indicators of Hungary in user_location #
#---------------------------------------------#

# View user_locations that contain biggest cities in Hungary
view_location("Budapest")
view_location("Debrecen")
view_location("Miskolc")

# Set indicators for Hungary
indicators <- c("Budapest", "Debrecen", "Magyarorsz", "Miskolc")

# Create logical vector for Hungary indicator found in tweets$user_location
hung <- unname(sapply(tweets$user_location, detect_country))

# Set or add "Hungary" to rows where hung = TRUE depending on current value
tweets <- tweets %>%
  mutate(country = case_when(hung == TRUE & country == "" ~ "Hungary",
                             hung== TRUE & country != ""  &
                               !str_detect(country, "Hungary") ~
                               paste(country, "Hungary", sep = ","),
                             TRUE ~ country))

# Clean up
rm(top_words, hung, ind, indicators, count_detect, detect_country,
   view_location)

#--------------------------------------------------#
# Check progress with country column one more time #
#--------------------------------------------------#

# Calculate proportion of country column that is ""
mean(tweets$country == "")

#------------------------------------------------------------------#
# Create logical columns for each vaccine being mentioned in tweet #
#------------------------------------------------------------------#

tweets <- tweets %>%
  mutate(oxford = ifelse(str_detect(text, "(?i)oxford|(?i)astrazeneca"),
                         TRUE,
                         FALSE),
         pfizer = ifelse(str_detect(text, "(?i)pfizer|(?i)biontech"),
                         TRUE,
                         FALSE),
         sputnik = ifelse(str_detect(text, "(?i)sputnik"),
                          TRUE,
                          FALSE),
         beijing = ifelse(str_detect(text, "(?i)sinopharm") &
                            str_detect(text, "(?i)beijing"),
                          TRUE,
                          FALSE),
         wuhan = ifelse(str_detect(text, "(?i)sinopharm") &
                          str_detect(text, "(?i)wuhan"),
                        TRUE,
                        FALSE),
         moderna = ifelse(str_detect(text, "(?i)moderna"),
                          TRUE,
                          FALSE),
         sinovac = ifelse(str_detect(text, "(?i)sinovac"),
                          TRUE,
                          FALSE),
         covaxin = ifelse(str_detect(text, "(?i)covaxin"),
                          TRUE,
                          FALSE),
         epivac = ifelse(str_detect(text, "(?i)epivaccorona"),
                         TRUE,
                         FALSE),
         johnson = ifelse(str_detect(text, "(?i)johnson"),
                          TRUE,
                          FALSE))

#-------------------------------------------------#
# Calculate average sentiment value of each tweet #
#-------------------------------------------------#

# Create function to separate words strung together in hashtags
word_sep <- function(text) {
  str_replace_all(text, "(?<=[a-z]{1})([A-Z]+)", " \\1")
}

# Create function to clean tweets
clean_tweet <- function(text) {
  cleaned <- text %>%
    
    # Remove links to pictures
    str_remove_all("&amp;") %>%
    
    # Remove @people
    str_remove_all("@\\w+") %>%
    
    # Remove punctuation
    gsub("[[:punct:]]", "", .) %>%
    
    # Remove web links
    str_remove_all("http\\w+") %>%
    
    # Separate words strung together in hashtags
    word_sep() %>%
    
    # Replace emojis with unique identifier
    replace_emoji_identifier() %>%
    
    # Remove left over emoji tags
    str_remove_all("<[\\w]*>") %>%
    
    # Remove digits
    str_remove_all("\\d")
  
  # Return cleaned string
  cleaned
}

# Load afinn sentiment dictionary
afinn <- get_sentiments("afinn")

# Load emoji sentiment dictionary
data("hash_sentiment_emojis")

# Rename columns of emoji dictionary to match afinn
# Multiply values of emoji dictionary by 5 to match range of afinn
emoji <- hash_sentiment_emojis %>%
  rename(word = x, value = y) %>%
  mutate(value = 5 * value)
rm(hash_sentiment_emojis)

# Combine emoji and afinn
sentiment_values <- rbind(afinn, emoji)

# Convert tweets to separate rows of individual words
# Note this will take a minute
tweets_words <- tweets %>%
  
  # Clean tweets 
  mutate(text = clean_tweet(text)) %>%
  
  # Unnest tokens
  unnest_tokens(word, text, token = "tweets") %>%
  
  # Remove stop words
  filter(!word %in% stop_words$word)

# Calculate sentiment values of each tweet
tweets_values <- tweets_words %>%
  
  # Join with sentiment values dictionary
  inner_join(sentiment_values, by = "word") %>%
  
  # Group by tweet id
  group_by(id) %>%
  
  # Calculate average sentiment value of tweet
  summarize(value = mean(value))

# Join tweets with sentiment values, set NA's to 0
tweets <- tweets %>%
  left_join(tweets_values, by = "id") %>%
  mutate(value = replace_na(value, 0))

# Clean up
rm(afinn, emoji, sentiment_values, tweets_values, clean_tweet, word_sep)

#--------------------------------------------------------------#
# Count number of occurrences of nrc sentiments for each tweet #
#--------------------------------------------------------------#

# Load nrc sentiment dictionary
nrc <- get_sentiments("nrc")

# Remove words "vaccine" and "trump" from nrc
nrc <- nrc %>%
  filter(word != "vaccine" & word != "trump")

# Join tweets_words with nrc sentiments
tweets_words <- tweets_words %>%
  inner_join(nrc, by = "word")

# Create columns with sentiment counts for each tweet
sentiment_counts <- tweets_words %>%
  count(id, sentiment) %>%
  spread(sentiment, n) %>%
  mutate_all(function(x) replace_na(x, 0))

# Join tweets with sentiment_counts
tweets <- tweets %>%
  left_join(sentiment_counts, by = "id")

# Set NA's in columns 28 through 37 to 0
tweets[,28:37]  <- tweets[,28:37] %>%
  mutate_all(function(x) replace_na(x, 0))

# Separate rows of tweets with multiple countries into individual rows
tweets <- tweets %>%
  separate_rows(country, sep = ",") %>%
  
  # Add continent info for each country
  left_join(continents, by = "country") %>%
  
  # Reorder columns so country, continent, and sub_region are together
  select(1:4, 38:39, 5:37) %>%
  
  # Replace NA's with ""
  mutate(continent = replace_na(continent, ""),
         sub_region = replace_na(sub_region, ""))

# Save tweets
save(tweets, file = "rdas/tweets_cleaned.rda")

# Separate rows of tweets_words with multiple countries into individual rows
tweets_words <- tweets_words %>%
  separate_rows(country, sep = ",") %>%
  
  # Add continent info for each country
  left_join(continents, by = "country") %>%
  
  # Reorder columns so country, continent, and sub_region are together
  select(1:4, 28:29, 5:27) %>%
  
  # Replace NA's with ""
  mutate(continent = replace_na(continent, ""),
         sub_region = replace_na(sub_region, ""))
  
# Save tweets_words
save(tweets_words, file = "rdas/tweets_words.rda")

# Clean up
rm(sentiment_counts)

#-----------------------------------------------------------------#
# Calculate number of tweets and sentiment stats for each country #
#-----------------------------------------------------------------#

# Count tweets and calculate average sentiment for each country
country_sentiments <- tweets %>%
  group_by(country) %>%
  summarize(n_tweets = n(),
            avg_sentiment = mean(value),
            avg_anger = mean(anger),
            avg_anticipation = mean(anticipation),
            avg_disgust = mean(disgust),
            avg_fear = mean(fear),
            avg_joy = mean(joy),
            avg_negative = mean(negative),
            avg_positive = mean(positive),
            avg_sadness = mean(sadness),
            avg_surprise = mean(surprise),
            avg_trust = mean(trust))

# Join country_sentiments with country_sums
country_sums <- country_sums %>%
  left_join(country_sentiments, by = "country")

# Set NA's in columns 25 through 36 to 0 for countries with no tweets
country_sums[25:36] <- country_sums[25:36] %>%
  mutate_all(function(x) replace_na(x, 0))

# Save country_sums and country_sentiments
save(country_sums, file = "rdas/vaccine_progress_country_totals.rda")
save(country_sentiments, file = "rdas/country_sentiments.rda")

################
# Explore data #
################

# Plot total vaccinations per hundred vs time by country
p <- progress %>%
  ggplot(aes(date, total_vaccinations_per_hundred)) +
  geom_line(aes(color = country,
                continent = continent)) +
  scale_x_date(date_labels = "%b '%y") +
  ylab("total vaccinations per hundred") +
  ggtitle("Total vaccinations per hundred vs time by country")
ggplotly(p)

# Plot people fully vaccinated per hundred vs people vaccinated per hundred by
# country over time
p <- progress %>%
  ggplot(aes(x = people_vaccinated_per_hundred,
             y = people_fully_vaccinated_per_hundred)) +
  geom_line(aes(color = country,
                continent = continent,
                date = date)) +
  geom_abline(color = "gray") +
  xlab("people vaccinated per hundred") +
  ylab("people fully vaccinated per hundred") +
  ggtitle("Vaccination progress over time by country")
ggplotly(p)

# Make same plot animated over time
p <- progress %>%
  mutate(day = as.numeric(date) - 18609) %>%
  ggplot(aes(x = people_vaccinated_per_hundred,
             y = people_fully_vaccinated_per_hundred)) +
  geom_abline(color = "gray") +
  geom_point(aes(ids = country,
                 color = continent,
                 size = population,
                 date = date,
                 frame = day,
                 vaccines = vaccines)) +
  theme_bw() +
  labs(title = "Vaccination progress over time by country since 12/14/2020",
       x = "people vaccinated per hundred",
       y = "people fully vaccinated per hundred")
ggplotly(p)


# Make same plot using plot_ly() function
progress %>%
  mutate(day = as.numeric(date) - 18609) %>%
  plot_ly(
    x = ~people_vaccinated_per_hundred,
    y = ~people_fully_vaccinated_per_hundred,
    size = ~population,
    color = ~continent,
    opacty = 0.5,
    frame = ~day,
    text = ~paste0("Country: ", country,
                  "\nContinent: ", continent,
                  "\nPopulation: ", population,
                  "\nDate: ", date,
                  "\nDay: ", day,
                  "\nVaccines: ", vaccines,
                  "\nPeople vaccinated per hundred :",
                  people_vaccinated_per_hundred,
                  "\nPeople fully vaccinated per hundred :",
                  people_fully_vaccinated_per_hundred),
    hoverinfo = "text",
    type = "scatter",
    mode = "markers"
  ) %>%
  layout(
    title = "Vaccination progress over time by country since 12/14/2020",
    xaxis = list(title = "people vaccinated per hundred"),
    yaxis = list(title = "people fully vaccinated per hundred")
  )

# Make same plot as gif
p <- progress %>%
  ggplot(aes(x = people_vaccinated_per_hundred,
             y = people_fully_vaccinated_per_hundred,
             size = total_vaccinations_per_hundred)) +
  geom_abline(color = "gray") +
  geom_text(x = 45,
            y = 70,
            label = "all vaccinated people are fully vaccinated ->",
            color = "gray",
            size = 3) +
  geom_point(aes(color = continent),
             alpha = 0.5) +
  geom_text(aes(x = people_vaccinated_per_hundred + 2 +
                  total_vaccinations_per_hundred / 100,
                y = people_fully_vaccinated_per_hundred + 2,
                label = iso_code),
            color = "black") +
  scale_size_continuous(range = c(1, 4)) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  scale_y_continuous(breaks = seq(0, 75, 25)) +
  theme_bw() +
  labs(title = "Date: {closest_state}",
       x = "people vaccinated per hundred",
       y = "people fully vaccinated per hundred",
       size = "total vaccinations per hundred") +
  transition_states(date)
  
# Create gif
animate(p,
        nframes = 260,
        height = 6,
        width = 9,
        units = "in",
        res = 100)

# Save gif
anim_save("figs/vaccination_progress_over_time_by_country.gif")

# Find which countries have the highest average tweet sentiment
# Filter for more than 25 tweets
country_sentiments %>%
  select(1:3) %>%
  filter(n_tweets > 25) %>%
  arrange(desc(avg_sentiment)) %>%
  head(10)

# Find which countries have the lowest average tweet sentiment
# Filter for more than 25 tweets
country_sentiments %>%
  select(1:3) %>%
  filter(n_tweets > 25) %>%
  arrange(avg_sentiment) %>%
  head(10)

# Plot avg_sentiment vs total_vacs_per_hundred
# Filter for more than 100 tweets
country_sums %>%
  filter(n_tweets > 100) %>%
  ggplot(aes(total_vacs_per_hundred, avg_sentiment, label = country)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_text(aes(x = 72,
                y = -0.06,
                label = "neutral"),
            color = "darkgray") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(size = 2.5) +
  geom_label(aes(x = 45,
                y = 1.25,
                label = "number of tweets > 100"),
            color = "slateblue4") +
  xlab("total vaccinations per hundred people as of 4/22/21") +
  ylab("average tweet sentiment as of 4/22/21") +
  ggtitle("Average tweet sentiment vs total vaccinations by country")

# Save plot
ifelse(!dir.exists(file.path(wd, "figs")),
       dir.create(file.path(wd, "figs")),
       FALSE)
ggsave("figs/sentiment_vs_total_vacs_by_country.png", dpi = 120)
rm(wd)

# Plot tweet sentiment vs time by country
# Filter for more than 750 tweets
tweets %>%
  group_by(country) %>%
  filter(n() > 750) %>%
  group_by(country, date) %>%
  summarize(avg_sentiment = mean(value)) %>%
  filter(country != "") %>%
  ggplot(aes(date, avg_sentiment, color = country)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_text(aes(x = as.Date("2020-12-21"),
                y = -0.08,
                label = "neutral"),
            color = "darkgray") +
  geom_smooth(se = FALSE) +
  geom_label(aes(x = as.Date("2021-02-28"),
                 y = 0.95,
                 label = "number of tweets > 750"),
             color = "slateblue4") +
  theme(legend.position = c(0.55, 0.83),
        legend.direction = "horizontal") +
  scale_x_date(date_breaks = "months",
               date_labels = "%m/%d/%Y") +
  ylab("average tweet sentiment") +
  ggtitle("Average tweet sentiment vs time by country")

# Save plot
ggsave("figs/sentiment_vs_time_by_country.png", dpi = 95)

# Plot tweet sentiment vs time by country
# Include all countries
# Make data points for all dates for all countries
# Use 13 day rolling average for tweet sentiment
dates <- seq.Date(min(tweets$date), max(tweets$date), 1)
countries <- country_sums$country
df <- expand.grid(countries, dates) %>%
  `colnames<-`(c("country", "date")) %>%
  arrange(country, date)
p <- tweets %>%
  filter(country != "") %>%
  group_by(country, date) %>%
  summarize(n_tweets = n(),
            avg_sentiment_daily = round(mean(value), 2)) %>%
  right_join(df, by = c("country", "date")) %>%
  mutate(n_tweets = replace_na(n_tweets, 0),
         avg_sentiment_daily = replace_na(avg_sentiment_daily, 0)) %>%
  left_join(continents, by = "country") %>%
  arrange(country, date) %>%
  mutate(avg_sentiment_13_day = round(rollmean(avg_sentiment_daily, 13, NA), 2))

p <- p %>%
  ggplot(aes(date, avg_sentiment_13_day)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_text(aes(x = as.Date("2020-12-21"),
                y = -0.08,
                label = "neutral"),
            color = "darkgray") +
  geom_line(aes(color = country,
                continent = continent,
                n_tweets = n_tweets,
                avg_sentiment_daily = avg_sentiment_daily)) +
  scale_x_date(date_labels = "%b '%y") +
  ylab("average tweet sentiment, 13 day rolling average") +
  ggtitle("Average tweet sentiment vs time by country")
ggplotly(p)

# Count number of tweets mentioning each vaccine
tweets %>%
  group_by(id) %>%
  filter(country == first(country)) %>%
  gather(vaccine, mentioned, oxford:johnson) %>%
  filter(mentioned == TRUE) %>%
  group_by(vaccine) %>%
  summarize(n = n())

# Plot tweet sentiment vs time by vaccine mentioned
# Filter for more than 250 tweets
tweets %>%
  group_by(id) %>%
  filter(country == first(country)) %>%
  gather(vaccine, mentioned, oxford:johnson) %>%
  filter(mentioned == TRUE) %>%
  group_by(vaccine) %>%
  filter(n() > 250) %>%
  group_by(vaccine, date) %>%
  summarize(avg_sentiment = mean(value)) %>%
  ggplot(aes(date, avg_sentiment, color = vaccine)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_text(aes(x = as.Date("2021-03-05"),
                y = 0.04,
                label = "neutral"),
            color = "darkgray") +
  geom_smooth(se = FALSE) +
  geom_label(aes(x = as.Date("2021-03-06"),
                 y = -0.2,
                 label = "number of tweets > 250"),
             color = "slateblue4") +
  theme(legend.position = c(0.6, 0.89),
        legend.direction = "horizontal") +
  scale_x_date(date_breaks = "months",
               date_labels = "%m/%d/%Y") +
  ylab("average tweet sentiment") +
  ggtitle("Average tweet sentiment vs time by vaccine")

# Save plot
ggsave("figs/sentiment_vs_time_by_vaccine.png",
       width = 7,
       height = 4.7,
       units = "in",
       dpi = 95)

# Plot tweet sentiment vs time by vaccine mentioned
# Filter for less than 250 tweets
tweets %>%
  group_by(id) %>%
  filter(country == first(country)) %>%
  gather(vaccine, mentioned, oxford:johnson) %>%
  filter(mentioned == TRUE) %>%
  group_by(vaccine) %>%
  filter(n() < 250) %>%
  group_by(vaccine, date) %>%
  summarize(avg_sentiment = mean(value)) %>%
  ggplot(aes(date, avg_sentiment, color = vaccine)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_text(aes(x = as.Date("2021-03-21"),
                y = -0.15,
                label = "neutral"),
            color = "darkgray") +
  geom_smooth(se = FALSE) +
  geom_label(aes(x = as.Date("2021-02-19"),
                 y = 2.75,
                 label = "number of tweets < 250"),
             color = "slateblue4") +
  theme(legend.position = c(0.8, 0.75)) +
  scale_x_date(date_breaks = "months",
               date_labels = "%m/%d/%Y") +
  ylab("average tweet sentiment") +
  ggtitle("Average tweet sentiment vs time by vaccine")

# Save plot
ggsave("figs/sentiment_vs_time_by_vaccine_small_n.png", dpi = 95)

# Plot tweet sentiment vs total vaccinations over time by country
# Set up data
dat <- progress %>%
  filter(country != "") %>%
  left_join(tweets, by = c("country", "date")) %>%
  group_by(country, date) %>%
  summarize(continent = first(continent),
            population = first(population),
            vaccines = first(vaccines),
            total_vaccinations_per_hundred =
              first(total_vaccinations_per_hundred),
            value = ifelse(is.na(value),
                               0,
                               mean(value)),
            n_tweets = n()) %>%
  unique()
  

# Make plot
# Use 7 day rolling average for tweet sentiment
p <- dat %>%
  group_by(country) %>%
  mutate(day = as.numeric(date) - 18612,
         sentiment = round(rollmean(value, 7, NA), 2)) %>%
  filter(!is.na(sentiment)) %>%
  select(-value) %>%
  ggplot(aes(total_vaccinations_per_hundred, sentiment)) +
  geom_hline(yintercept = 0,
             color = "gray") +
  geom_point(aes(ids = country,
                 color = continent,
                 size = population,
                 date = date,
                 frame = day,
                 n_tweets = n_tweets,
                 vaccines = vaccines)) +
  theme_bw() +
  labs(title = "Tweet sentiment vs total vaccinations over time by country",
       x = "total vaccinations per hundred",
       y = "tweet sentiment, rolling 7 day average")
ggplotly(p)

# Make same plot using plot_ly() function
dat %>%
  group_by(country) %>%
  mutate(day = as.numeric(date) - 18612,
         sentiment = round(rollmean(value, 7, NA), 2)) %>%
  filter(!is.na(sentiment)) %>%
  select(-value) %>%
  plot_ly(
    x = ~total_vaccinations_per_hundred,
    y = ~sentiment,
    size = ~population,
    color = ~continent,
    opacty = 0.5,
    frame = ~day,
    text = ~paste0("Country: ", country,
                   "\nContinent: ", continent,
                   "\nPopulation: ", population,
                   "\nDate: ", date,
                   "\nDay: ", day,
                   "\nNumber of tweets: ", n_tweets,
                   "\nVaccines: ", vaccines,
                   "\nTotal vaccinations per hundred :",
                   total_vaccinations_per_hundred,
                   "\nAverage tweet sentiment:",
                   sentiment),
    hoverinfo = "text",
    type = "scatter",
    mode = "markers"
  ) %>%
  layout(
    title = "Tweet sentiment vs total vaccinations over time by country",
    xaxis = list(title = "total vaccinations per hundred"),
    yaxis = list(title = "tweet sentiment, rolling 7 day average")
  )





# Find most prevalent tweet words with nrc sentiment
tweets_words %>%
  group_by(id) %>%
  filter(country == first(country)) %>%
  group_by(id, word) %>%
  filter(sentiment == first(sentiment)) %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(12) %>%
  as.data.frame()

# Find most prevalent tweet words with positive nrc sentiment
tweets_words %>%
  group_by(id) %>%
  filter(country == first(country) & sentiment == "positive") %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(12) %>%
  as.data.frame()

# Find most prevalent tweet words with negative nrc sentiment
tweets_words %>%
  group_by(id) %>%
  filter(country == first(country) & sentiment == "negative") %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(12) %>%
  as.data.frame()

# Find most prevalent tweet words with joy nrc sentiment
tweets_words %>%
  group_by(id) %>%
  filter(country == first(country) & sentiment == "joy") %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(12) %>%
  as.data.frame()

# Find most prevalent tweet words with fear nrc sentiment
tweets_words %>%
  group_by(id) %>%
  filter(country == first(country) & sentiment == "fear") %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(12) %>%
  as.data.frame()
















# This script analyzes COVID vaccines tweets alongside
# global vaccination progress
# Data downloaded from kaggle
# Vaccine tweets: https://www.kaggle.com/gpreda/all-covid19-vaccines-tweets
# Vaccine progress: https://www.kaggle.com/gpreda/covid-world-vaccination-progress
# Country populations: https://www.kaggle.com/tanuprabhu/population-by-country-2020

# Install necessary libraries
if (!require(tidyverse))
  install.packages("tidyverse", repos="http://cran.r-project.org")
if (!require(tidytext))
  install.packages("tidytext", repos="http://cran.r-project.org")
if (!require(textdata))
  install.packages("textdata", repos="http://cran.r-project.org")
if (!require(SentimentAnalysis))
  install.packages("SentimentAnalysis", repos = "http://cran.r-project.org")


# Load necessary libraries
library("tidyverse")
library("tidytext")
library("textdata")
library("SentimentAnalysis")

# Read data in from github
progress <- read_csv("https://raw.githubusercontent.com/oneskychai/covid-vaccines/trunk/country_vaccinations.csv")
tweets <- read_csv("https://raw.githubusercontent.com/oneskychai/covid-vaccines/trunk/vaccination_all_tweets.csv")

# View data
view(progress)
view(tweets)

# Summarize progress data by country
country_sums <- progress %>%
  group_by(country) %>%
  summarize(total_vacs = max(total_vaccinations, na.rm = TRUE),
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
country_sums <- country_sums[,-5]

# Make vector of country names
# Count number of countries
countries <- progress %>%
  .$country %>%
  unique()
n_countries <- length(countries)

# Count NA's in daily_vaccinations column
sum(is.na(progress$daily_vaccinations))

# Create vector of indexes for NA's in daily_vaccination column
ind_na <- which(is.na(progress$daily_vaccinations))

# Create vector of indexes for first row of each country
ind_first <- sapply(countries, function(c) first(which(progress$country == c)))

# Inspect rows in ind_na not in ind_first
ind <- setdiff(ind_na, ind_first)
progress[ind,]

# Set NA's in daily_vaccinations column of first rows of countries to 0 or
# total_vaccinations or sum of people_vaccinated and people_fully_vaccinated,
# whichever is larger
# If larger value is larger than first non-NA value for daily_vaccinations for
# that country, set value to first non-NA value for that country
values <- sapply(ind_first, function(ind) {
  total <- progress$total_vaccinations[ind]
  sum <- progress[ind,] %>%
    mutate(p_v = ifelse(is.na(people_vaccinated), 0, people_vaccinated),
           p_f_v = ifelse(is.na(people_fully_vaccinated),
                          0,
                          people_fully_vaccinated),
           sum = p_v + p_f_v) %>%
    .$sum
  value <- max(c(total, sum), na.rm = TRUE)
  max(c(value, 0))
})
country_first <- sapply(countries, function(c) {
  progress %>%
    filter(country == c & !is.na(daily_vaccinations)) %>%
    .$daily_vaccinations %>%
    first()
})
country_first <- ifelse(is.na(country_first), values, country_first)
values <- ifelse(values > country_first, country_first, values)
progress$daily_vaccinations[ind_first] <- values

# Calculate 17 missing NA values for daily_vaccinations column for Latvia
progress <- progress %>%
  mutate(p_f_v = ifelse(country == "Latvia" & is.na(people_fully_vaccinated),
                        0,
                        people_fully_vaccinated),
         daily_vaccinations =
           ifelse(country == "Latvia" & is.na(daily_vaccinations),
                  people_vaccinated + p_f_v -
                    lag(people_vaccinated) - lag(p_f_v),
                  daily_vaccinations)) %>%
  select(-p_f_v)

# Set NA's in people_fully_vaccinated column for countries where country_sums
# people_fully_vaxed column = -Inf to 0
countries_pfvna <- country_sums %>%
  filter(people_fully_vaxed == -Inf) %>%
  .$country
progress <- progress %>%
  mutate(people_fully_vaccinated = ifelse(country %in% countries_pfvna,
                                          0,
                                          people_fully_vaccinated))

# For rows where total_vaccinations and people_vaccinated are not NA,
# set people_fully_vaccinated = total_vaccinations - people_vaccinated
progress <- progress %>%
  mutate(people_fully_vaccinated = ifelse(!is.na(total_vaccinations) &
                                            !is.na(people_vaccinated),
                                          total_vaccinations -
                                            people_vaccinated,
                                          people_fully_vaccinated))

# Count number of countries with all NA's in total_vaccinations column
sum(country_sums$total_vacs == -Inf)

# Count number of countries with NA in first row of total_vaccinations column
sum(is.na(progress$total_vaccinations[ind_first]))

# Find which country has NA in first row of total_vaccinations column
ind <- ind_first[which(is.na(progress$total_vaccinations[ind_first]))]
ind

# Set first row of total_vaccinations for Latvia to value of daily_vaccinations
progress$total_vaccinations[ind] <- progress$daily_vaccinations[ind]

# Set NA's in total_vaccinations column to sum of daily_vaccinations and
# previous value of total_vaccinations
while (sum(is.na(progress$total_vaccinations)) != 0) {
  progress <- progress %>%
    mutate(total_vaccinations = ifelse(is.na(total_vaccinations),
                                       daily_vaccinations +
                                         lag(total_vaccinations),
                                       total_vaccinations))
}

# If people_vaccinated is NA and people_fully_vaccinated is not NA,
# set people_vaccinated to total_vaccinations - people_fully_vaccinated
progress <- progress %>%
  mutate(people_vaccinated = ifelse(is.na(people_vaccinated) &
                                      !is.na(people_fully_vaccinated),
                                    total_vaccinations -
                                      people_fully_vaccinated,
                                    people_vaccinated))

# Count number of rows where people_vaccinated is not NA and
# people_fully_vaccinated is NA
progress %>%
  filter(!is.na(people_vaccinated) & is.na(people_fully_vaccinated)) %>%
  summarize(n())

# If people_vaccinated is not NA and people_fully_vaccinated is NA,
# set people_fully_vaccinated to total_vaccinations - people_vaccinated
progress <- progress %>%
  mutate(people_fully_vaccinated = ifelse(!is.na(people_vaccinated) &
                                            is.na(people_fully_vaccinated),
                                          total_vaccinations -
                                            people_vaccinated,
                                          people_fully_vaccinated))

# If a row in a country's total_vaccinations column is less than the previous
# row's value, change it to the previous row's value
ind_not_first <- setdiff(1:nrow(progress), ind_first)
for (i in ind_not_first) {
  if (progress$total_vaccinations[i] < progress$total_vaccinations[i - 1]) {
    progress$total_vaccinations[i] <- progress$total_vaccinations[i - 1]
  }
}

# Set daily_vaccinations for each row for each country to difference between
# total_vaccinations value of the row and the previous row
for (i in ind_not_first) {
  progress$daily_vaccinations[i] <- progress$total_vaccinations[i] -
    progress$total_vaccinations[i - 1]
}

# Make sure people_vaccinated = total_vaccinations - people_fully_vaccinated
progress <- progress %>%
  mutate(people_vaccinated = total_vaccinations - people_fully_vaccinated)

# Count number of countries with NA's in first row for people_vaccinated and
# people_fully_vaccinated columns
progress[ind_first,] %>%
  filter(is.na(people_vaccinated) & is.na(people_fully_vaccinated)) %>%
  summarize(n())

# For countries above, set first row of people_vaccinated column to value of
# total_vaccinations column
ind_na <- which(is.na(progress$people_vaccinated) &
               is.na(progress$people_fully_vaccinated))
ind_na_first <- setdiff(ind_na, ind_not_first)
for (i in ind_na_first) {
  progress$people_vaccinated[i] <- progress$total_vaccinations[i]
}

# For NA's in people_vaccinated column, add value in daily_vaccinations column
# to value of people_vaccinated column in previous row
ind_na <- which(is.na(progress$people_vaccinated))
for (i in ind_na) {
  progress$people_vaccinated[i] <- progress$people_vaccinated[i - 1] +
    progress$daily_vaccinations[i]
}

# If value of people_vaccinated column for a country is more than the value for
# the next row, set it equal to the value for the next row
for (i in rev(ind_not_first)) {
  if (progress$people_vaccinated[i - 1] > progress$people_vaccinated[i]) {
    progress$people_vaccinated[i - 1] <- progress$people_vaccinated[i]
  }
}

# Set people_fully_vaccinated to total_vaccinations - people_vaccinated
progress <- progress %>%
  mutate(people_fully_vaccinated = total_vaccinations - people_vaccinated)

# Count NA's in columns 4 through 7
sum(is.na(progress[,4:7]))

# Count rows for each country where value is less than value in previous row for
# columns 4 through 6
# Create function for repeated use
find_all_problems <- function() {
  counts <- rep(0, 3)
  for (i in 1:3) {
    for (j in ind_not_first) {
      if (progress[j, i + 3] < progress[j - 1, i + 3]) {
        counts[i] <- counts[i] + 1
      }
    }
  }
  counts
}
counts <- find_all_problems()
counts

# Find indexes for above cases
# Create function for continued use
find_problems <- function() {
  ind <- which(progress[,6] < lag(progress[,6]))
  setdiff(ind, ind_first)
}
ind <- find_problems()

# Examine above rows
problems <- progress[ind,]
view(problems)

# Count number of rows with negative values in columns 6 and 7
colSums(progress[,6:7] < 0)

# Find rows with negative values in column 6 and add to problems
ind <- which(progress[,6] < 0)
problems <- union(problems, progress[ind,])
problems <- problems %>%
  arrange(country, date)

# For negative values in column 6, set to previous non-negative value and deduct
# last non-negative value from column 5 and add negative value of column 6
value <- progress[[ind[1] - 1, 6]]
for (i in ind) {
  progress[i, 5] <- progress[i, 5] - value + progress[i, 6]
  progress[i, 6] <- value
}

# For remaining rows in problems, shuffle excess value from column 6 to column 5
# Use while loop to fix newly generated problems
ind <- find_problems()
while(length(ind) > 0) {
  for (i in rev(ind)) {
    excess <- progress[i - 1, 6] - progress[i, 6]
    progress[i - 1, 6] <- progress[i, 6]
    progress[i - 1, 5] <- progress[i - 1, 5] + excess
    ind <- find_problems()
  }
}

# Check for problems with decreasing sequential values
count <- find_all_problems()
count

# Make sure there are no negative values
sum(progress < 0, na.rm = TRUE)

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

# Check to make sure all total_vacs = people_vaxed + people_fully_vaxed
country_sums %>%
  summarize(mean(total_vacs == people_vaxed + people_fully_vaxed))






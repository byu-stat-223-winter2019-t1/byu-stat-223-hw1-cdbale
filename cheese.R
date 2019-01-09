library(tidyverse)

# Read into R the data in the files "cheese1.csv" and "cheese2.csv".  These
# datasets contain the per capita consumption of different cheeses from 1995 to
# 2014. The two data sets cover the same years, although the names for the
# variable holding the year differ.  Merge the data together into a single
# data.frame in which all the consumption data for a given year is one
# observation.  Name your merged data.frame "cheese".  The dimension of this
# data.frame should be 20 rows and 7 columns.

## Reading in the cheese data files
cheese1 <- read_csv('cheese1.csv')
cheese2 <- read_csv('cheese2.csv')

## changing the name of the 'time' variable in cheese2 to 'Year' to match cheese1
colnames(cheese2)[which(names(cheese2) == 'Time')] <- 'Year'

## left join cheese2 to cheese1 on the 'Year' column and arrange by 'Year'
cheese <- cheese1 %>%
  left_join(cheese2, by = 'Year') %>%
  arrange(Year)

# Which kind of cheese has the highest average consumption per capita over all
# years?

## calculate the mean consumption of cheese for each cheese type over all years
cheese %>%
  summarize_at(vars(Cheddar:Neufchatel), mean)
## Cheddar has the highest average consumption per capita across all years: 9.81

# Which year saw the largest total consumption per capita of all cheeses?

## group by 'Year' and calculate total cheese consumption for each year, 'total_consumption'
## then ungroup the data and filter for the observation with the maximum 'total_consumption'
## value, selecting the year and consumption amount from that observation
cheese %>%
  group_by(Year) %>%
  mutate(total_consumption = sum(Cheddar:Neufchatel)) %>%
  ungroup() %>%
  filter(total_consumption == max(total_consumption)) %>%
  select(Year, total_consumption)
# 2006 saw the largest total consumption per capita: 55.4
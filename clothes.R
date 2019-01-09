library(tidyverse)

# Read in the data from "clothes.csv" file into a data.frame called "clothes".
# Remove all rows from the data.frame where the variable "Price" is empty,
# i.e., equal to "". 

## read in clothes data file
clothes <- read_csv('clothes.csv')

## remove missing values from 'Price' column
clothes <- clothes %>%
  filter(!is.na(Price))

# Write a function called "num" that takes a character vector of prices as
# input, removes the dollar sign, and returns a numeric vector of prices.

num <- function(string) { 
  
  no_sign <- substr(string, 2, nchar(string)) %>%
    as.double()
  
  return(no_sign)
}

# Using the "num" function you just wrote, convert the variable "Price" in the
# clothes data.frame from a character vector to a numeric vector.

## change the entries in the Price column to decimals without a dollar sign
clothes <- clothes %>%
  mutate(Price = num(Price))

# Determine the standard deviation of the price of clothes for each part of the
# body, as indicated by the variable "Body.Location".

## group data by 'body.location' and calculate the standard deviation of price for each
## body location
clothes %>%
  group_by(Body.Location) %>%
  summarize(price_sd = sd(Price))
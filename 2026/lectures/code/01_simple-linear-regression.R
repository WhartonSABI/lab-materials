########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "Lahman"))

# load packages
library(dplyr)
library(Lahman)

#########################
### DATA MANIPULATION ###
#########################

# preview Lahman dataset
Batting

# batting data 2020
batting_2020 = Batting |> 
    # filter for year
    filter(yearID == 2020) |> 
    # group by player, year
    group_by(playerID,yearID) |> 
    # get hits and at bats for each player-year
    summarise(
        hits = sum(H, na.rm = TRUE),
        at_bats = sum(AB, na.rm = TRUE)
    ) |> 
    ungroup() |> 
    # filter out players who never went to bat
    filter(at_bats != 0) |> 
    # calculate batting average
    mutate(
        ba = hits / at_bats
    )

# batting data 2021
batting_2021 = Batting |> 
    # filter for year
    filter(yearID == 2021) |> 
    # group by player, year
    group_by(playerID,yearID) |> 
    # get hits and at bats for each player-year
    summarise(
        hits = sum(H, na.rm = TRUE),
        at_bats = sum(AB, na.rm = TRUE)
    ) |> 
    ungroup() |> 
    # filter out players who never went to bat
    filter(at_bats != 0) |> 
    # calculate batting average
    mutate(
        ba = hits / at_bats
    )

# join two datasets
batting = batting_2020 |> 
    # join with 2021 data
    inner_join(batting_2021, by = "playerID", suffix = c("_2020", "_2021"))

#####################
### MODEL FITTING ###
#####################

# fit a linear model
model = lm(ba_2021 ~ ba_2020, data = batting)
# print
model
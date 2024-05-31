## This script does three things:
# - It consolidates retrosheet data into a single dataframe with yearly data (easy)
# - It consolidates the MLB odds data into a single dataframe (less easy)
# - It applies a mapping structure to the baseball teams to correspond with official codes

library(retrosheet) # package for accessing retrosheet data
## Condense CSVs
years <- 2010:2021 # years with odds data available

# Initialize games dataframe
games_df <- data.frame()

# Extracting games results
for (year in years) {
  game_result <- get_retrosheet("game", year) # local year call to retrosheet
  games_df <- rbind(games_df, game_result) # append to dataframe
}

# Save CSV
#write.csv(games_df,file="games_data.csv",append=FALSE)

## Merge with next R File
# Read all documents. Documents are in .XLSX format so need to handle
# Also have merged cells which hopefully don't create processing issues
# Will need to check for quality issues

# Package to read .XLSX files (uncomment if needed)
#install.packages("readxl")

# Load the readxl package
library(readxl)

# Path to .xlsx file
# Local directory (change as needed) where odds data is stored
path <- "./data/"

# Initialize games dataframe
odds_df <- data.frame()

# Several steps to data pipeline:
# - Reading / cleansing all files from Excel
# - Accommodating structure changes from 2014 onwards (and colname changes in 2020 onwards)
# - Eliminating unwanted text data and replacing it with NA so vectors may remain numeric

# Function for replacing text characters with NA, to be used later
replace_with_na <- function(text) {
  text[text == "x" | text == "NL"] <- NA
  return(text)
}

# Initial for loop for reading and consolidating data from raw files
for (year in years) {
  file_name <- paste("mlb-odds-", year, ".xlsx", sep="") # iterate through file names
  file_path <- paste(path, file_name, sep="") # full path including working directory
  odds_data <- read_excel(file_path) # from readxl library
  odds_data$"Year" <- year # add column to file to store year

  # Reorder columns
  if (year < 2014) {
    # Split odds data into two parts to be able to sandwich NA columns later
    odds_data_p1 <- odds_data[1:17]
    odds_data_p2 <- odds_data[18:ncol(odds_data)]
    colnames(odds_data_p2)[c(2,4)] <- c("...21", "...23") # needed for consistent naming later

    # Read first part (resets odds_data)
    odds_data <- odds_data_p1

    # Add placeholders
    odds_data$"Run Line" <- NA
    odds_data$"...19" <- NA

    # Combine remaining data and append to odds_df
    odds_data <- cbind(odds_data, odds_data_p2) # combine remaining columns
    odds_df <- rbind(odds_df, odds_data) # append yearly data to global odds_df
  } else {
    # Forced constraints for consistency
    colnames(odds_data)[18] = "Run Line"
    colnames(odds_data)[20] = "Open OU"
    colnames(odds_data)[22] = "Close OU"
    odds_df <- rbind(odds_df, odds_data) # append yearly data to global odds_df
  }

}

# Note: Due to merged cells, some data loses labels in .xlsx files.
# This code improves column names of final data structure
colnames(odds_df)[18:23] <- c("Run_Line",
                           "Run_Line_Odds",
                           "Opening_OU",
                           "Opening_Odds",
                           "Closing_OU",
                           "Closing_Odds")

# Apply the function to selected columns
odds_df[] <- sapply(odds_df, replace_with_na, simplify = FALSE)

# Further cleaning - convert removed text data into NA and datatypes to numeric
odds_df[6:ncol(odds_df)] <- lapply(odds_df[6:ncol(odds_df)], as.numeric)


## Next big step: Data cleansing and flattening row counts 2:1
# Core issue: Odds data currently shows two rows per game (1 row per playing team per game)
# Games data shows one row per game
# To consolidate our data sources, we need to break down data frame
# Each of the two rows for a given game are separated by "V" or "H" teams

library(dplyr) # easily drop / manipulate columns in dataframe

# Split odds_df into data for home and visiting teams (reduce rows by half)
h_odds_df <- odds_df[odds_df["VH"] == "H",]
colnames(h_odds_df) <- paste("H_", colnames(h_odds_df), sep="")
v_odds_df <- odds_df[odds_df["VH"] == "V",]
colnames(v_odds_df) <- paste("V_", colnames(v_odds_df), sep="")

# Data cleansing
game_odds_df <- cbind(h_odds_df, v_odds_df)
game_odds_df <- game_odds_df %>%
  select(-"H_VH", -"V_VH") %>%
  rename(
    Date_Raw = H_Date,
    Rot = H_Rot,
    Year = H_Year
  ) %>%
  select(-"V_Date", -"V_Year") %>%
  rename(
    HmTm = H_Team,
    VisTm = V_Team
  ) %>%
  mutate(
    Month = as.numeric(substr(Date_Raw, 1, 1)),
    Day = as.numeric(substr(Date_Raw, 2, 3)),
    Date = as.Date(paste(Year, Month, Day, sep = "-"), format="%Y-%m-%d")
  )

## Matching

map = read.csv("team_code_map.csv")
team_code_df = merge(game_odds_df, map, by.x="HmTm", by.y="Team_Code_Odds_DF")
team_code_df = team_code_df %>%
  rename(
    Home_Team_Code = "Abbreviation_Retrosheet"
  ) %>%
  select(-"City", -"Team")
team_code_df = merge(team_code_df, map, by.x="VisTm", by.y="Team_Code_Odds_DF")
team_code_df = team_code_df %>%
  rename(
    Visiting_Team_Code = "Abbreviation_Retrosheet"
  ) %>%
  select(-"City", -"Team")

team_code_df$key <- paste(team_code_df$Date, team_code_df$Home_Team_Code, sep="_")
games_df$key <- paste(games_df$Date, games_df$HmTm, sep="_")

merged_df <- merge(games_df, team_code_df, by = "key")
merged_df <- merged_df %>%
  rename(
    Date = Date.x,
    Day = Day.x,
  ) %>%
  select(-"Date.y",
         -"Day.y",
         -"VisTm.y",
         -"HmTm.y",
         -"Date_Raw",
         -"key",
         -"HmTm.x",
         -"VisTm.x")


calculate_implied_odds <- function(odds) {
  if (is.na(odds)) {
    return(NA)
  } else if (odds > 0) {
    implied_probability <- 100 / (odds + 100)
  } else {
    implied_probability <- abs(odds) / (abs(odds) + 100)
  }
  return(implied_probability)
}

add_implied_odds <- function(df) {
  df$h_implied_opening_odds <- sapply(df$H_Open, calculate_implied_odds)
  df$v_implied_opening_odds <- sapply(df$V_Open, calculate_implied_odds)
  df$h_implied_closing_odds <- sapply(df$H_Close, calculate_implied_odds)
  df$v_implied_closing_odds <- sapply(df$V_Open, calculate_implied_odds)

  df$h_implied_closing_odds_mid <- (df$h_implied_opening_odds + (1 - df$v_implied_opening_odds)) / 2
  df$v_implied_closing_odds_mid <- (df$h_implied_closing_odds + (1 - df$v_implied_closing_odds)) / 2
  return(df)
}

merged_df <- add_implied_odds(merged_df)
#Dropping games with no odds. Only have 2 of these.
merged_df <- merged_df %>%
  filter(!is.na(H_Open) & !is.na(V_Open) &
         !is.na(H_Close) & !is.na(V_Close))

merged_df$season <- as.numeric(substr(merged_df$Date, 1, 4))

merged_df$run_dif <- merged_df$HmRuns - merged_df$VisRuns
merged_df$home_win <- ifelse(merged_df$run_dif > 0, 1, 0)

## File writing
write.csv(merged_df,file="odds_and_R_data.csv",append=FALSE)

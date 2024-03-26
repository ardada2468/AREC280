library(tidyverse)
library(maps)
library(readr)
library(scatterplot3d)
library(usmap)

getData <- function() {

  Data1 <- read_csv("./Data/1992_1988_Winter_Wheat_Data.csv") # nolint
  Data2 <- read_csv("./Data/1997_1993_Winter_Wheat_Data.csv")
  Data3 <- read_csv("./Data/2002_1998_Winter_Wheat_Data.csv")
  Data4 <- read_csv("./Data/2007_2003_Winter_Wheat_Data.csv")
  Data5 <- read_csv("./Data/2013_2008_Winter_Wheat_Data.csv")
  Data6 <- read_csv("./Data/2022_2014_Winter_Wheat_Data.csv")
  tmp_us <- read_rds("./Data/tmp_US_counties.rds")
  pcp <- read_rds("./Data/pcp_US_counties.rds")

  #Merging and tidying data

  #Combining Data from the diffrent years
  combined_data <- rbind(Data1, Data2, Data3, Data4, Data5, Data6)

  #Pivioting Data to make it tidy
  pivoted_data <- combined_data |>
    pivot_wider(names_from = "Data Item", values_from = Value) |>
    #Getting rid of vals that are null
    select(-c("CV (%)", "Region", "Zip Code", "Watershed", "watershed_code"))

  #Converting from str to numeric type
  columns_to_convert <- c("WHEAT, WINTER - ACRES HARVESTED", "WHEAT, WINTER - ACRES PLANTED", "WHEAT, WINTER - PRODUCTION, MEASURED IN BU", "WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE")
  pivoted_data <- pivoted_data |>
    mutate_at(vars(columns_to_convert), as.numeric)

  #Make the month column numeric to easily filter growing period months
  month_mapping <- c("JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, "JUL" = 7, "AUG" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)
  tmp_us$MONTH <- month_mapping[tmp_us$MONTH]
  pcp$MONTH <- month_mapping[pcp$MONTH]
  # Convert MONTH to numeric in tmp
  tmp_us$MONTH <- as.numeric(tmp_us$MONTH)
  pcp$MONTH <- as.numeric(tmp_us$MONTH)

  # Filter data for months September to May
  filtered_tmp_entireSeason <- tmp_us %>% filter((MONTH >= 10 & MONTH <= 12) | (MONTH >= 1 & MONTH <= 6))
  filtered_pcp_entireSeason <- pcp %>% filter((MONTH >= 10 & MONTH <= 12) | (MONTH >= 1 & MONTH <= 6))

  filtered_tmp_winterFrost <- tmp_us %>% filter((MONTH >= 10 & MONTH <= 12) | (MONTH >= 1 & MONTH <= 3))
  filtered_pcp_winterFrost <- pcp %>% filter((MONTH >= 10 & MONTH <= 12) | (MONTH >= 1 & MONTH <= 3))


  # Group by County and Year, then calculate the average temperature
  average_temp_growing_season <- filtered_tmp_entireSeason |>
    group_by(COUNTYFP, YEAR) |>
    summarise(Avg_Temperature = mean(TMP, na.rm = TRUE))

  average_pcp_growing_season <- filtered_pcp_entireSeason |>
    group_by(COUNTYFP, YEAR) |>
    summarise(avgPcp = mean(PCP, na.rm = TRUE))

  average_pcp_winter_months <- filtered_pcp_winterFrost |>
    group_by(COUNTYFP, YEAR) |>
    summarise(avgPcp_winter = mean(PCP, na.rm = TRUE))

  average_temp_winter_months <- filtered_tmp_winterFrost |>
    group_by(COUNTYFP, YEAR) |>
    summarise(Avg_Temperature_winter = mean(TMP, na.rm = TRUE))

  merged_data <- merge(pivoted_data, average_temp_growing_season, by.x = c("County ANSI", "Year"), by.y = c("COUNTYFP", "YEAR"), all.x = TRUE)
  merged_data <- merge(merged_data, average_pcp_growing_season, by.x = c("County ANSI", "Year"), by.y = c("COUNTYFP", "YEAR"), all.x = TRUE)
  merged_data <- merge(merged_data, average_pcp_winter_months, by.x = c("County ANSI", "Year"), by.y = c("COUNTYFP", "YEAR"), all.x = TRUE)
  merged_data <- merge(merged_data, average_temp_winter_months, by.x = c("County ANSI", "Year"), by.y = c("COUNTYFP", "YEAR"), all.x = TRUE)


  countyData <- merged_data |> group_by(State, County, Year) |> summarise(
    avgYeild = mean(`WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE`, na.rm= TRUE),
    avgTemp = Avg_Temperature,
    avgPcp_winter = avgPcp_winter,
    avgPcp = avgPcp,
    avgTempWinter= Avg_Temperature_winter,
  )
  countyData <- countyData[!is.na(countyData$avgYeild), ]




  mutatedCountyData <- countyData |> group_by(State, County) |> arrange(Year) |> mutate(
    lowerCaseState= str_to_lower(State),
    avgChangeYeild = avgYeild- lag(avgYeild),
    avgChangeTemp = avgTemp - lag(avgTemp),
    avgChangePcp_winter = avgPcp_winter - lag(avgPcp_winter),
    avgChangePcp = avgPcp - lag(avgPcp),
    avgChangeTempWinter= avgTempWinter - lag(avgTempWinter)
  )

  state_to_region_mapping <- data.frame(
    State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
              "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
              "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
              "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
              "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
    Region = c("South", "West", "West", "South", "West", "West", "Northeast", "Northeast", "South", "South",
               "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South", "Northeast", "South",
               "Northeast", "Midwest", "Midwest", "South", "Midwest", "West", "Midwest", "West", "Northeast", "Northeast",
               "West", "Northeast", "South", "Midwest", "South", "Midwest", "West", "Midwest", "West", "Northeast",
               "Northeast", "South", "Midwest", "West", "Northeast", "South", "West", "South", "West", "Midwest")
  ) |> mutate(
    State = str_to_upper(State)
  )

  # mutatedCountyData2 <- merge(mutatedCountyData, state_to_region_mapping, by.x = c("State", "State"))

  return(mutatedCountyData)
}

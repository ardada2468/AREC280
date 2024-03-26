library(tidyverse)
library(maps)
library(readr)
library(scatterplot3d)
library(usmap)
library(stargazer)
source("./LoadData.R")


mutatedCountyData <- getData()
countyData <- mutatedCountyData




reg_winter_temp <- lm(countyData$avgYeild ~ countyData$avgTempWinter + as.factor(countyData$State))
summary(reg_winter_temp)
# stargazer(reg_winter_temp)

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
countyData <- countyData %>%
  left_join(state_to_region_mapping, by = "State")


#Export Tables
countyDataMidwest <- countyData |> filter(Region == "Midwest")
reg_winter_temp_change <- lm(countyDataMidwest$avgChangeYeild ~ countyDataMidwest$avgChangePcp_winter + as.factor(countyDataMidwest$County))
stargazer(reg_winter_temp_change, type = "html",
          dep.var.labels = "Yield (BU / ACRE)",
          covariate.labels = c("Temperature (°F)"),
          flip = TRUE,
          out = "./Exports/reg_winter_rain_Midwest.htm",
          omit = "County",
          title = "Linear Model for Change in Yield and Change in Winter Precipitation (Midwest)"

)

countyDataMidwest <- countyData |> filter(Region == "Northeast")
reg_winter_temp_change <- lm(countyDataMidwest$avgChangeYeild ~ countyDataMidwest$avgChangePcp_winter + as.factor(countyDataMidwest$County))
stargazer(reg_winter_temp_change, type = "html",
          dep.var.labels = "Yield (BU / ACRE)",
          covariate.labels = c("Temperature (°F)"),
          flip = TRUE,
          out = "./Exports/reg_winter_rain_Northeast.htm",
          omit = "County",
          title = "Linear Model for Change in Yield and Change in Winter Precipitation (Northeast)"

)

countyDataMidwest <- countyData |> filter(Region == "South")
reg_winter_temp_change <- lm(countyDataMidwest$avgChangeYeild ~ countyDataMidwest$avgChangePcp_winter + as.factor(countyDataMidwest$County))
stargazer(reg_winter_temp_change, type = "html",
          dep.var.labels = "Yield (BU / ACRE)",
          covariate.labels = c("Temperature (°F)"),
          flip = TRUE,
          out = "./Exports/reg_winter_rain_South.htm",
          omit = "County",
          title = "Linear Model for Change in Yield and Change in Winter Precipitation (South)"

)

countyDataMidwest <- countyData |> filter(Region == "West")
reg_winter_temp_change <- lm(countyDataMidwest$avgChangeYeild ~ countyDataMidwest$avgChangePcp_winter + as.factor(countyDataMidwest$County))
stargazer(reg_winter_temp_change, type = "html",
          dep.var.labels = "Yield (BU / ACRE)",
          covariate.labels = c("Temperature (°F)"),
          flip = TRUE,
          out = "./Exports/reg_winter_rain_west.htm",
          omit = "County",
          title = "Linear Model for Change in Yield and Change in Winter Precipitation (West)"

)



countyDataMidwest <- countyData |> filter(Region == "Midwest")
reg_winter_temp_change <- lm(countyDataMidwest$avgChangeYeild ~ countyDataMidwest$avgChangeTempWinter + as.factor(countyDataMidwest$County))
stargazer(reg_winter_temp_change, type = "html",
          dep.var.labels = "Yield (BU / ACRE)",
          covariate.labels = c("Temperature (°F)"),
          flip = TRUE,
          out = "./Exports/reg_winter_temp_Midwest.htm",
          omit = "County",
          title = "Linear Model for Change in Yield and Change in Winter Temp (Midwest)"

)

countyDataMidwest <- countyData |> filter(Region == "Northeast")
reg_winter_temp_change <- lm(countyDataMidwest$avgChangeYeild ~ countyDataMidwest$avgChangeTempWinter + as.factor(countyDataMidwest$County))
stargazer(reg_winter_temp_change, type = "html",
          dep.var.labels = "Yield (BU / ACRE)",
          covariate.labels = c("Temperature (°F)"),
          flip = TRUE,
          out = "./Exports/reg_winter_temp_NorthEast.htm",
          omit = "County",
          title = "Linear Model for Change in Yield and Change in Winter Temp (NorthEast)"

)

countyDataMidwest <- countyData |> filter(Region == "South")
reg_winter_temp_change <- lm(countyDataMidwest$avgChangeYeild ~ countyDataMidwest$avgChangeTempWinter + as.factor(countyDataMidwest$County))
stargazer(reg_winter_temp_change, type = "html",
          dep.var.labels = "Yield (BU / ACRE)",
          covariate.labels = c("Temperature (°F)"),
          flip = TRUE,
          out = "./Exports/reg_winter_temp_South.htm",
          omit = "County",
          title = "Linear Model for Change in Yield and Change in Winter Temp (South)"

)

countyDataMidwest <- countyData |> filter(Region == "West")
reg_winter_temp_change <- lm(countyDataMidwest$avgChangeYeild ~ countyDataMidwest$avgChangeTempWinter + as.factor(countyDataMidwest$County))
stargazer(reg_winter_temp_change, type = "html",
          dep.var.labels = "Yield (BU / ACRE)",
          covariate.labels = c("Temperature (°F)"),
          flip = TRUE,
          out = "./Exports/reg_winter_temp_West.htm",
          omit = "County",
          title = "Linear Model for Change in Yield and Change in Winter Temp (West)"

)

reg_winter_pcp <- lm(countyData$avgYeild ~ countyData$avgPcp_winter + as.factor(countyData$State))
summary(reg_winter_pcp)


reg_winter_pcp_change <- lm(countyData$avgChangeYeild ~ countyData$avgChangePcp_winter + as.factor(countyData$State))
summary(reg_winter_pcp_change)



reg_winter_temp <- lm(countyData$avgYeild ~ countyData$avgTempWinter + as.factor(countyData$State))
summary(reg_winter_temp)
stargazer(reg_winter_temp, type = "html",
          dep.var.labels = "Yield (BU/Acre)",
          covariate.labels = c("Temperature(°C)"), out = "./Exports/reg_winter_temp.htm")




# ggplot(countyData, aes(x = avgChangePcp_winter, y = avgChangeYeild, color = Region)) +
#   geom_point() +
#   geom_smooth(method = "lm", color = "black")+
#   labs(title="Change in Yield vs Change in Winter Tempratrue")


ggplot(countyData, aes(x = avgChangeTempWinter, y = avgChangeYeild, color = Region)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black")+
  facet_wrap(~Region) +
  labs(title="Change in Yield vs Change in Winter Tempratrue")



ggplot(countyData, aes(x = avgChangePcp_winter, y = avgChangeYeild, color = Region)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black")+
  facet_wrap(~Region) +
  labs(title="Change in Yield vs Change in Winter Precipitation")


# ggplot(countyData, aes(x = avg))

#Graph Production
LMmodelByState_temp <- data.frame()
LMmodelByState_pcp <- data.frame()
states <- unique(mutatedCountyData$State)

# Gernate LM models for each state
for (i in 1:42){
  currentstate <- toString(states[i])
  #idk why florida was causing too many issues
  if(currentstate != "FLORIDA"){
    tempData <- mutatedCountyData |> filter(State == currentstate);
    reg_winter_tempChangeState <- lm(tempData$avgChangeYeild ~ tempData$avgChangeTempWinter)
    cf <- coef(reg_winter_tempChangeState)
    LMmodelByState_temp <- rbind(LMmodelByState_temp, list(currentstate, cf[2]));
    # print(LMmodelByState_temp)
  }
}


#Gernate LM models for each state
for (i in 1:42){
  currentstate <- toString(states[i])
  if(currentstate != "FLORIDA"){
    tempData <- mutatedCountyData |> filter(State == currentstate);
    reg_winter_tempChangeState_pcp <- lm(tempData$avgChangeYeild ~ tempData$avgChangePcp_winter)
    cf <- coef(reg_winter_tempChangeState_pcp)
    LMmodelByState_pcp <- rbind(LMmodelByState_pcp, list(currentstate, cf[2]));
    print(LMmodelByState_pcp)
  }
}



#this is to get data of the LM model by state
df_TMP <- data.frame(
  state = LMmodelByState_temp$X.ALABAMA.,
  values = LMmodelByState_temp$c..tempData.avgChangeTempWinter....1.48271217217911.
)

# plot_usmap(data = df_TMP)
#
# plot_usmap(data = df_TMP) +
#   scale_fill_stepsn(breaks=c(0),
#                     colors=c("blue","orange"))


plot_usmap(data = df_TMP,)+
  scale_fill_gradientn(name    = "Change in Yield vs Change in Temp (Slope)",
                       colours = c("red", "white", "forestgreen"),
                       breaks  = c(0),
                       label   = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Change in Yield vs Change in Temperature in Winter")





df_PCP <- data.frame(
  state = LMmodelByState_pcp$X.ALABAMA.,
  values = LMmodelByState_pcp$c..tempData.avgChangePcp_winter....4.35803945823288.
)

plot_usmap(data = df_PCP) +
  scale_fill_gradientn(name    = "Change in Yield vs Change in PCP (Slope)",
                       colours = c("red", "white", "forest green"),
                       breaks  = c(0),
                       label   = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Change in Yield vs Change in Precipitation in Winter")




merged_df <- merge(df_PCP, df_TMP, by = "state" , suffixes = c("_PCP", "_TMP"))

# Create a new column 'Color' based on the specified conditions
merged_df$Color <- ifelse(merged_df$values_PCP < 0 & merged_df$values_TMP < 0, "Negtivie correlation for both",
                          ifelse(merged_df$values_PCP >= 0 & merged_df$values_TMP < 0 |
                                   merged_df$values_PCP < 0 & merged_df$values_TMP >= 0, "Opposite correlation", "Postive correlation"))

# Plot the map
plot_usmap(data = merged_df, values = "Color", labels = FALSE) +
  scale_fill_manual(values = c("red", "white", "forestgreen"), name = "Color Key") +
  theme(legend.position = "right") +
  labs(title = "Postive, Negitive or mixed imapct of Tempratre and Rain on Yield")
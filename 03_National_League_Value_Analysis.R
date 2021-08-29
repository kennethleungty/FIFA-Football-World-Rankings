library(tidyverse)

# Import data
rankings_data <- read_csv('FIFA_World_Rankings_Extracted_20210610.csv')
league_value_data <- read_csv("League_value_data.csv")

####-----Data Processing-----####

# Find the top 150 countries based on 2021 rankings
topranking_countries<-rankings_data%>%
  separate(date, into = c("Day", "Month", "Year"))%>%
  mutate(Year = as.integer(Year))%>%
  group_by(country, Year)%>%
  summarise(avg_points = round(mean(points), 1))%>%
  ungroup()%>%
  filter(Year >= 2021)%>%
  arrange(desc(avg_points))%>%
  mutate(Ranking = rank(-avg_points, ties.method = "random"),
         country = as.character(country))%>%
  filter(Ranking <=150)

# Process of league value data

# Currency in pounds
# Convert monetary values in terms of millions
league_value_data_2 <- league_value_data%>%
  mutate(Value = as.character(Value))%>%
  mutate(Value = gsub("£", "" , Value))%>%
  mutate(Monetary_value = as.numeric(gsub("(\\d+\\.*\\d+).*", "\\1", Value)),
         Scale = gsub("(\\d+\\.*\\d+)", "", Value))%>%
  mutate(Total_value = case_when(
    Scale == "bn" ~ Monetary_value * 1000,
    Scale == "m" ~ Monetary_value,
    Scale == "Th." ~ Monetary_value / 1000
  ))

# Group by country because New Zealand has three tier 1 leagues over different regions, the average value of the three leagues is used
# Given that different leagues have different number of participating clubs, the value is normalised to value per club 
league_value_data_3<-league_value_data_2%>%
  group_by(Country)%>%
  summarise(Total_value = mean(Total_value),
            Clubs = mean(Clubs),
            Age = mean(Average_Age))%>%
  mutate(Average_value = Total_value / Clubs)
  
# Merge both rankings and league values data

# Rename some values of the countries to align with both dataframes
topranking_countries[(topranking_countries$country == "IR Iran"), c("country")] = "Iran"
topranking_countries[(topranking_countries$country == "China PR"), c("country")] = "China"

# Join both dataframes
# 59 countries are removed due to missing league value data
merged_data<-topranking_countries%>%
  rename("Country" = "country")%>%
  left_join(league_value_data_3, by = "Country")%>%
  na.omit()

####-----Linear Regression Analysis-----####

# Given that the average monetary values are skewed, they are logged to better fit the linear regression model
res = lm(avg_points ~ log(Average_value), data = merged_data)  
summary(res)

####-----Visualisation of the Linear Regression-----####

p1 <- merged_data%>%
  ggplot(aes(x = log(Average_value), y = avg_points))+
  geom_point(colour = "tomato")+
  geom_smooth(method = "lm", colour = "navy")+
  geom_text(aes(x = 4, y = 1100, label = "R-squared = 0.47"))+
  labs(title = "Relationship between monetary value of national\nleagues and FIFA world rankings",
       y = "Total Points",
       x = "Log Monetary Value of Nation's Tier 1 League\n(£ M/club)",
       caption = "Source: FIFA & Transfermarket.co.uk | Visualisation: @nxrunning")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5))

ggsave(p1, dpi = 600, filename = "Fifa_rankings_monetary_value.jpeg")




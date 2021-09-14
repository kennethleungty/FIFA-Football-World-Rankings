# Import required libraries
library(tidyverse)
library(gganimate)
library(ggimage)
library(showtext)

# Import data
rankings_data <- read_csv("FIFA_World_Rankings_Extracted_20210610.csv")
flags_data <- read_csv("country_flags_dataset.csv")

####-----Data processing-----####

# Separate date by day, month, and year
rankings_data_2 <- rankings_data%>%
  separate(date, into = c("Day", "Month", "Year"))%>%
  mutate(Year = as.integer(Year))

# Compute average points per year
rankings_data_3 <- rankings_data_2%>%
  group_by(country, Year)%>%
  summarise(avg_points = round(mean(points), 1))

# Rank the top ten countries for every year
rankings_data_4<-rankings_data_3%>%
  group_by(Year)%>%
  arrange(Year, desc(avg_points))%>%
  mutate(rank = rank(-avg_points, ties.method = "random"))%>% #random selection if points are tied
  filter(rank <= 10)

# Renaming Yugoslavia as Serbia
rankings_data_4[(rankings_data_4$country == "Yugoslavia"), c("country")] = "Serbia"

# Minor changes to the flags data to help merge with the raw data
flags_data[(flags_data$Country == "United States"), c("Country")] <- "USA"
flags_data <- flags_data %>%
  rename("country" = "Country")

# Merge both data
merged_data<-rankings_data_4%>%
  left_join(flags_data, by = "country")

# Grouping the countries based on UN geoscheme classification
Western_europe_list = c("Germany","Belgium","France","Netherlands","Switzerland")
Northern_europe_list = c("Sweden","Denmark","Republic of Ireland","England","Norway")
Southern_europe_list = c("Italy","Croatia","Spain","Serbia","Portugal","Turkey")
Eastern_europe_list = c("Czech Republic","Poland","Romania","Russia")
South_America_list = c("Brazil","Argentina","Mexico","Colombia","Paraguay","Uruguay","Chile")
North_America_list = c("USA")

merged_data<-merged_data%>%
  mutate(Region = case_when((country %in% c(Western_europe_list))  ~ "Western Europe",
                            (country %in% c(Northern_europe_list))  ~ "Northern Europe",
                            (country %in% c(Southern_europe_list))  ~ "Southern Europe",
                            (country %in% c(Eastern_europe_list))  ~ "Eastern Europe",
                            (country %in% c(South_America_list))   ~ "South America",
                            (country %in% c(North_America_list))  ~ "North America"),
         Year_label = as.numeric(Year))%>%
  mutate(Region = factor(Region, levels = c("South America", "North America", "Northern Europe",
                                            "Southern Europe", "Eastern Europe", "Western Europe")))

####-----Visualisation-----####

# Fonts
font_add_google(name = "Open Sans", family = "Open")
showtext_auto()

# Colour Palette
customised_palette = c("#F16745","#404040","#FFC65D","#7BC8A4","#4CC3D9","#93648D")

# Bar chart race plot
p1 <- ggplot(merged_data, aes(x = -rank, y = avg_points, fill = Region))+
  # Create static plot
  geom_tile(aes(y = avg_points/2, height = avg_points), width = 0.9)+
  # We use text labels to act as 'axis' labels instead
  geom_text(aes(y = 0, label = country), color = "black", hjust = "right", vjust = 0.5, family = "Open", nudge_y = -30)+
  # Add points as text label
  geom_text(aes(label = as.character(round(avg_points,0))), color = "black", hjust = "left", family = "roboto", nudge_y = 220)+
  # Add country flag images
  geom_image(aes(image = ImageURL, y = avg_points + 120))+
  # Flip axis
  coord_flip(clip = "off", expand = FALSE)+
  # Set axis limits
  scale_y_continuous(limits = c(-350, max(merged_data$avg_points)+1200),
                     breaks = c(0,500,1000,1500))+
  # Set colour palette to our customised palette colours
  scale_fill_manual(values = customised_palette)+
  # Customise theme of plot
  theme(legend.position =  c(0.88,0.4),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 10, family = "Open"),
        legend.text = element_text(size = 10, family = "Open"))+
  # Add animation
  transition_time(Year)+
  ease_aes('cubic-in-out')+
  # Labels of plot
  labs(title = "FIFA World Rankings",
       subtitle = "{frame_time}",
       y = "Yearly average points",
       caption = "Source: FIFA | Visualisation: @nxrunning",
       fill = "Region")+
  # Further customise title, subtitle and caption
  theme(plot.title = element_text(size = 30, family = "Open", face = "bold", colour = "#005391", hjust = 0.5, margin = margin(0,0,20,0)),
        plot.subtitle = element_text(size = 30, family = "Open", face = "bold", hjust = 0.5, margin = margin(0,0,20,0)),
        plot.caption = element_text(size = 13, family = "Open"))

# Animate and save as gif
animate(p1, nframes = 400, fps = 15, height = 900, width = 1200, end_pause = 30)
anim_save("FifaWorldRankings_Barchartrace.gif")
